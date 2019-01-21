{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes      #-}

-- | Specification of 'Pos.Chain.Block.VAR'.

module Test.Pos.Block.Logic.VarSpec
       ( spec
       , runTest
       ) where

import           Universum hiding ((<>))

import           Control.Monad.Random.Strict (MonadRandom (..), RandomGen,
                     evalRandT, uniform)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Ratio as Ratio
import           Data.Semigroup ((<>))
import           Formatting -- (sformat, build, (%))
import           Serokell.Util (listJson)
import           Test.Hspec (Spec, beforeAll_, describe)
import           Test.Hspec.Runner (hspec) -- With, defaultConfig, configQuickCheckSeed)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck.Gen (Gen (MkGen))
import           Test.QuickCheck.Monadic (assert, pick, pre)
import           Test.QuickCheck.Random (QCGen)

import           Pos.Chain.Block
import           Pos.Chain.Genesis as Genesis
import           Pos.Chain.Txp
import           Pos.Core (ProtocolConstants (..), pcBlkSecurityParam)
import           Pos.Core.Chrono (NE, NewestFirst (..), OldestFirst (..),
                     nonEmptyNewestFirst, nonEmptyOldestFirst, toNewestFirst,
                     splitAtNewestFirst, toNewestFirst, _NewestFirst)
import           Pos.Core.Slotting
import           Pos.DB.Block (verifyAndApplyBlocks, verifyBlocksPrefix)
import           Pos.DB.Pure (dbPureDump)

import           Pos.Generator.BlockEvent.DSL (BlockApplyResult (..),
                     BlockEventGenT, BlockRollbackFailure (..),
                     BlockRollbackResult (..), BlockScenario,
                     Path, byChance, emitBlockApply,
                     emitBlockRollback, enrichWithSnapshotChecking,
                     pathSequence, runBlockEventGenT)
import           Pos.Generator.BlockEvent (BlockScenario' (..), BlockEvent' (..), BlockEventApply' (..), BlockEventRollback' (..), CheckCount (..))
import qualified Pos.GState as GS
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.Wlog (setupTestLogging)

import           Test.Pos.Block.Logic.Event (BlockScenarioResult (..),
                     DbNotEquivalentToSnapshot (..), runBlockScenario)
import           Test.Pos.Block.Logic.Mode (BlockProperty, BlockTestMode)
import           Test.Pos.Block.Logic.Util (EnableTxPayload (..),
                     InplaceDB (..), bpGenBlock, bpGenBlocks,
                     bpGoToArbitraryState, getAllSecrets, satisfySlotCheck)
import           Test.Pos.Block.Property (blockPropertySpec)
import           Test.Pos.Configuration (HasStaticConfigurations,
                     withStaticConfigurations)
import           Test.Pos.Util.QuickCheck.Property (splitIntoChunks,
                     stopProperty)


-- stack test cardano-sl --fast --test-arguments "-m Test.Pos.Chain.Block.Var"
spec :: Spec
-- Unfortunatelly, blocks generation is quite slow nowdays.
-- See CSL-1382.
spec = beforeAll_ setupTestLogging $ withStaticConfigurations $ \txpConfig _ ->
    describe "Block.Logic.VAR" $ modifyMaxSuccess (min 4) $ do
        describe "verifyBlocksPrefix" $ verifyBlocksPrefixSpec txpConfig
        describe "verifyAndApplyBlocks" $ verifyAndApplyBlocksSpec txpConfig
        describe "applyBlocks" applyBlocksSpec
        describe "Block.Event" $ do
            describe "Successful sequence" $ blockEventSuccessSpec txpConfig
            describe "Apply through epoch" $ applyThroughEpochSpec txpConfig 0
            describe "Apply through epoch" $ applyThroughEpochSpec txpConfig 4
            describe "Fork - short" $ singleForkSpec txpConfig ForkShort
            describe "Fork - medium" $ singleForkSpec txpConfig ForkMedium
            describe "Fork - deep" $ singleForkSpec txpConfig ForkDeep

runTest :: IO ()
runTest = do
    setupTestLogging
    withStaticConfigurations $ \txpConfig _ -> hspec $ -- With (defaultConfig { configQuickCheckSeed = Just 10000000001234567890 }) $
        describe "\nErik: Successful sequence\n\n" $
            blockPropertySpec blockEventSuccessDesc $ \genesisConfig -> do
                (scenario, checkCount) <- genScenario genesisConfig
                                                        txpConfig
                                                        (genSuccessWithForks genesisConfig)
                -- let scenario = BlockScenario . List.take 5 $ unBlockScenario scenarioX
                when (checkCount <= 0) $ stopProperty $
                    "No checks were generated, this is a bug in the test suite: " <>
                    prettyScenario scenario

                when False $ do
                    putTextLn ""
                    putTextLn $ prettyScenario scenario

                runBlockScenarioAndVerify genesisConfig txpConfig scenario
  where
    blockEventSuccessDesc =
        "a sequence of interleaved block applications and rollbacks " <>
        "results in the original state of the blockchain"

    genScenario
        :: HasConfigurations
        => Genesis.Config
        -> TxpConfiguration
        -> BlockEventGenT QCGen BlockTestMode ()
        -> BlockProperty (BlockScenario, CheckCount)
    genScenario genesisConfig txpConfig m = do
        scenario <- blockPropertyScenarioGen genesisConfig txpConfig m

        if  | not (uniqueBlockApply scenario) ->
                genScenario genesisConfig txpConfig m

            | otherwise -> pure $ enrichWithSnapshotChecking scenario

-- | Return 'True' if the applied blocks (taking into account rollbacks) results
-- in a unique set of block hashes. The generator tests will fail if the
-- generated blocks (accounting for rollback) are not unique.
uniqueBlockApply :: BlockScenario -> Bool
uniqueBlockApply (BlockScenario bs) =
    let ys = foldScenario [] bs in
    ys == ordNub ys
  where
    foldScenario :: [HeaderHash] -> [BlockEvent' Blund] -> [HeaderHash]
    foldScenario !acc [] = List.sort acc
    foldScenario !acc (x:xs) =
        case x of
            BlkEvApply ea ->
                foldScenario ((map headerHash . toList $ toNewestFirst (_beaInput ea)) ++ acc) xs
            BlkEvRollback re ->
                foldScenario (rollback (map headerHash . toList $ getNewestFirst (_berInput re)) acc) xs
            BlkEvSnap _ ->
                foldScenario acc xs

    rollback :: [HeaderHash] -> [HeaderHash] -> [HeaderHash]
    rollback _ [] = []
    rollback [] acc = acc
    rollback (r:rs) (x:xs) =
        if (r == x)
            then rollback rs xs
            else error $ sformat
                    ("uniqueBlockApply: rollback failed " % listJson % " " % listJson)
                        (r:rs) (x:xs)

----------------------------------------------------------------------------
-- verifyBlocksPrefix
----------------------------------------------------------------------------

verifyBlocksPrefixSpec
    :: HasStaticConfigurations
    => TxpConfiguration
    -> Spec
verifyBlocksPrefixSpec txpConfig = do
    blockPropertySpec verifyEmptyMainBlockDesc
                      (flip verifyEmptyMainBlock txpConfig)
    blockPropertySpec verifyValidBlocksDesc
                      (flip verifyValidBlocks txpConfig)
  where
    verifyEmptyMainBlockDesc =
        "verification of consistent empty main block " <>
        "created by the leader of the 0-th slot " <>
        "always succeeds for initial GState"
    verifyValidBlocksDesc =
        "verification of (hopefully) valid blocks " <>
        "generated by the block generator " <>
        "always succeeds for GState for which these blocks where generated " <>
        "as long as all these blocks are from the same epoch"

verifyEmptyMainBlock :: HasConfigurations
                     => Genesis.Config
                     -> TxpConfiguration
                     -> BlockProperty ()
verifyEmptyMainBlock genesisConfig txpConfig = do
    emptyBlock <- fst <$> bpGenBlock genesisConfig
                                     txpConfig
                                     (EnableTxPayload False)
                                     (InplaceDB False)
    curSlot <- getCurrentSlot $ configEpochSlots genesisConfig
    whenLeftM (lift $ verifyBlocksPrefix genesisConfig curSlot (one emptyBlock))
        $ stopProperty
        . pretty

verifyValidBlocks
    :: HasConfigurations
    => Genesis.Config
    -> TxpConfiguration
    -> BlockProperty ()
verifyValidBlocks genesisConfig txpConfig = do
    bpGoToArbitraryState
    blocks <- map fst . toList <$> bpGenBlocks genesisConfig
                                              txpConfig
                                              Nothing
                                              (EnableTxPayload True)
                                              (InplaceDB False)
    pre (not $ null blocks)
    let blocksToVerify = OldestFirst $ case blocks of
            -- impossible because of precondition (see 'pre' above)
            [] -> error "verifyValidBlocks: impossible"
            (block0:otherBlocks) ->
                let (otherBlocks', _) = List.span isRight otherBlocks
                in block0 :| otherBlocks'

    verRes <- lift $ satisfySlotCheck blocksToVerify $ verifyBlocksPrefix
        genesisConfig
        Nothing
        blocksToVerify
    whenLeft verRes $ stopProperty . pretty

----------------------------------------------------------------------------
-- verifyAndApplyBlocks
----------------------------------------------------------------------------

verifyAndApplyBlocksSpec :: HasStaticConfigurations
                         => TxpConfiguration
                         -> Spec
verifyAndApplyBlocksSpec txpConfig =
    blockPropertySpec applyByOneOrAllAtOnceDesc $ \genesisConfig ->
        applyByOneOrAllAtOnce genesisConfig txpConfig (applier genesisConfig)
  where
    applier
        :: Genesis.Config
        -> OldestFirst NE Blund
        -> BlockTestMode ()
    applier genesisConfig blunds = do
        let blocks = map fst blunds
        -- we don't check current SlotId, because the applier is run twice
        -- and the check will fail the verification
        satisfySlotCheck blocks $ whenLeftM
            (verifyAndApplyBlocks genesisConfig txpConfig Nothing True blocks)
            throwM
    applyByOneOrAllAtOnceDesc =
        "verifying and applying blocks one by one leads " <>
        "to the same GState as verifying and applying them all at once " <>
        "as well as applying in chunks"

----------------------------------------------------------------------------
-- applyBlocks
----------------------------------------------------------------------------

-- Commented out because tests are slow.
-- We can enable it later if we make tests much faster.

applyBlocksSpec :: Spec
applyBlocksSpec = pass
-- applyBlocksSpec = do
--     prop applyByOneOrAllAtOnceDesc (applyByOneOrAllAtOnce applier)
--   where
--     applier = applyBlocks True Nothing
--     applyByOneOrAllAtOnceDesc =
--         "applying blocks one by one leads to the same GState as " <>
--         "applying them all at once"

----------------------------------------------------------------------------
-- General functions
----------------------------------------------------------------------------

applyByOneOrAllAtOnce
    :: HasConfigurations
    => Genesis.Config
    -> TxpConfiguration
    -> (OldestFirst NE Blund -> BlockTestMode ())
    -> BlockProperty ()
applyByOneOrAllAtOnce genesisConfig txpConfig applier = do
    bpGoToArbitraryState
    blunds <- getOldestFirst <$> bpGenBlocks genesisConfig
                                             txpConfig
                                             Nothing
                                             (EnableTxPayload True)
                                             (InplaceDB False)
    pre (not $ null blunds)
    let blundsNE = OldestFirst (NE.fromList blunds)
    stateAfter1by1 <- lift $ GS.withClonedGState $ do
        mapM_ (applier . one) (getOldestFirst blundsNE)
        dbPureDump
    chunks             <- splitIntoChunks 5 (blunds)
    stateAfterInChunks <- lift $ GS.withClonedGState $ do
        mapM_ (applier . OldestFirst) chunks
        dbPureDump
    stateAfterAllAtOnce <- lift $ do
        applier blundsNE
        dbPureDump
    assert
        (  stateAfter1by1
        == stateAfterInChunks
        && stateAfterInChunks
        == stateAfterAllAtOnce
        )

----------------------------------------------------------------------------
-- Block events
----------------------------------------------------------------------------

blockEventSuccessSpec :: HasStaticConfigurations
                      => TxpConfiguration
                      -> Spec
blockEventSuccessSpec txpConfig =
    blockPropertySpec blockEventSuccessDesc
                      (flip blockEventSuccessProp txpConfig)
  where
    blockEventSuccessDesc =
        "a sequence of interleaved block applications and rollbacks " <>
        "results in the original state of the blockchain"

{- | This generator is carefully designed to cover multitude of success
   scenarios. Apply/rollback are interleaved in various ways, shown by diagrams below:

   0 -----a----> 2
   |             |        Synchronous apply/rollback
   0 <----r----- 2

   0 -a-> 1 -a-> 2
   |             |        Multiple apply per rollback
   0 <----r----- 2

   0 -----a----> 2
   |             |        Multiple rollback per apply
   0 <-r- 1 <-r- 2

   0 -a-> 3 -a-> 6 -a-> 9
   |                    |        Desynchronous apply/rollback
   0 <--r--- 4 <---r--- 9

   Furthermore, it allows nested forks (forks of forks), generates both unique
   and repeated forks, respects the 'blkSecurityParam', and can be used with
   'enrichWithSnapshotChecking'. (I would draw diagrams for these features as
   well, but they're barely readable in the ASCII format). Just trust me that
   this generator gives diverse block event sequences -- I spent an entire night
   and a few sheets of paper trying to figure out how to write it.
-}

genSuccessWithForks :: forall g m . (RandomGen g, Monad m)
                    => Genesis.Config
                    -> BlockEventGenT g m ()
genSuccessWithForks genesisConfig = do
    emitBlockApply BlockApplySuccess $ pathSequence mempty ["0"]
    generateFork "0" []
    emitBlockApply BlockApplySuccess $ pathSequence "0" ["1", "2"]
    generateFork ("0" <> "1" <> "2") []
  where
    generateFork ::
           Path -- base path (from the main chain)
        -> NewestFirst [] Path -- current fork state
        -> BlockEventGenT g m ()
    generateFork basePath rollbackFork = do
        let
            forkLen    = length rollbackFork
            k          = (pcBlkSecurityParam . configProtocolConstants) genesisConfig
            wiggleRoom = fromIntegral k - forkLen
        stopFork <- byChance (if forkLen > 0 then 0.1 else 0)
        if stopFork
            then whenJust (nonEmptyNewestFirst rollbackFork) $
                 emitBlockRollback BlockRollbackSuccess
            else do
                needRollback <-
                    -- forkLen=0                => needRollback 0%
                    -- forkLen=blkSecurityParam => needRollback 100%
                    byChance (realToFrac $ forkLen Ratio.% fromIntegral k)
                if needRollback
                    then do
                        retreat <- getRandomR (1, forkLen)
                        whenJust (nonEmptyNewestFirst rollbackFork) $ \rollbackFork' -> do
                            -- forkLen > 0, therefore retreat > 0
                            let (over _NewestFirst NE.fromList -> before, after) = splitAtNewestFirst retreat rollbackFork'
                            emitBlockRollback BlockRollbackSuccess before
                            generateFork basePath after
                    else do
                        advance <- getRandomR (1, wiggleRoom)
                        relPaths <- OldestFirst <$> replicateM advance generateRelativePath1
                        whenJust (nonEmptyOldestFirst relPaths) $ \relPaths' -> do
                            let
                                curPath = maybe basePath NE.head $ nonEmpty (getNewestFirst rollbackFork)
                                paths = pathSequence curPath relPaths'
                            emitBlockApply BlockApplySuccess paths
                            generateFork basePath (over _NewestFirst toList (toNewestFirst paths) <> rollbackFork)
    generateRelativePath1 :: BlockEventGenT g m Path
    generateRelativePath1 =
        uniform (["rekt", "kek", "mems", "peka"] :: NE Path)

blockPropertyScenarioGen
    :: HasConfigurations
    => Genesis.Config
    -> TxpConfiguration
    -> BlockEventGenT QCGen BlockTestMode ()
    -> BlockProperty BlockScenario
blockPropertyScenarioGen genesisConfig txpConfig m = do
    allSecrets <- getAllSecrets
    let genStakeholders = configBootStakeholders genesisConfig
    g <- pick $ MkGen $ \qc _ -> qc
    lift $ flip evalRandT g $ runBlockEventGenT genesisConfig
                                                txpConfig
                                                allSecrets
                                                genStakeholders
                                                m

prettyScenario :: BlockScenario -> Text
prettyScenario scenario = pretty (fmap (headerHash . fst) scenario)

blockEventSuccessProp
    :: HasConfigurations => Genesis.Config -> TxpConfiguration -> BlockProperty ()
blockEventSuccessProp genesisConfig txpConfig = do
    scenario <- blockPropertyScenarioGen genesisConfig
                                         txpConfig
                                         (genSuccessWithForks genesisConfig)
    let (scenario', checkCount) = enrichWithSnapshotChecking scenario
    when (checkCount <= 0) $ stopProperty $
        "No checks were generated, this is a bug in the test suite: " <>
        prettyScenario scenario'
    runBlockScenarioAndVerify genesisConfig txpConfig scenario'

runBlockScenarioAndVerify
    :: HasConfigurations
    => Genesis.Config
    -> TxpConfiguration
    -> BlockScenario
    -> BlockProperty ()
runBlockScenarioAndVerify genesisConfig txpConfig bs = verifyBlockScenarioResult
    =<< lift (runBlockScenario genesisConfig txpConfig bs)

verifyBlockScenarioResult :: BlockScenarioResult -> BlockProperty ()
verifyBlockScenarioResult = \case
    BlockScenarioFinishedOk -> return ()
    BlockScenarioUnexpectedSuccess -> stopProperty $
        "Block scenario unexpected success"
    BlockScenarioUnexpectedFailure e -> stopProperty $
        "Block scenario unexpected failure: " <>
        pretty e
    BlockScenarioDbChanged d ->
        let DbNotEquivalentToSnapshot snapId dbDiff = d in
        stopProperty $
            "Block scenario resulted in a change to the blockchain" <>
            " relative to the " <> show snapId <> " snapshot:\n" <>
            show dbDiff

----------------------------------------------------------------------------
-- Multi-epoch
----------------------------------------------------------------------------

-- Input: the amount of blocks after crossing.
applyThroughEpochSpec
    :: HasStaticConfigurations
    => TxpConfiguration
    -> Int
    -> Spec
applyThroughEpochSpec txpConfig afterCross =
    blockPropertySpec applyThroughEpochDesc $ \genesisConfig ->
        applyThroughEpochProp genesisConfig txpConfig afterCross
  where
    applyThroughEpochDesc =
      "apply a sequence of blocks that spans through epochs (additional blocks after crossing: " ++
      show afterCross ++ ")"

applyThroughEpochProp :: HasConfigurations
                      => Genesis.Config
                      -> TxpConfiguration
                      -> Int
                      -> BlockProperty ()
applyThroughEpochProp genesisConfig txpConfig afterCross = do
    scenario <- blockPropertyScenarioGen genesisConfig txpConfig $ do
        let
            approachEpochEdge =
                pathSequence mempty . OldestFirst . NE.fromList $
                replicate (fromIntegral (configEpochSlots genesisConfig) - 1) "a"
            crossEpochEdge =
                pathSequence (NE.last $ getOldestFirst approachEpochEdge) $
                OldestFirst . NE.fromList $
                -- 2 blocks to ensure that we cross,
                -- then some additional blocks
                replicate (afterCross + 2) "x"
        emitBlockApply BlockApplySuccess approachEpochEdge
        emitBlockApply BlockApplySuccess crossEpochEdge
    runBlockScenarioAndVerify genesisConfig txpConfig scenario

----------------------------------------------------------------------------
-- Forks
----------------------------------------------------------------------------

singleForkSpec :: HasStaticConfigurations
               => TxpConfiguration
               -> ForkDepth
               -> Spec
singleForkSpec txpConfig fd = blockPropertySpec singleForkDesc
    $ \genesisConfig -> singleForkProp genesisConfig txpConfig fd
  where
    singleForkDesc =
      "a blockchain of length q<=(9.5*k) blocks can switch to a fork " <>
      "of length j>i with a common prefix i, rollback depth d=q-i"

singleForkProp :: HasConfigurations
               => Genesis.Config
               -> TxpConfiguration
               -> ForkDepth
               -> BlockProperty ()
singleForkProp genesisConfig txpConfig fd = do
    scenario <- blockPropertyScenarioGen genesisConfig txpConfig $ genSingleFork genesisConfig fd
    runBlockScenarioAndVerify genesisConfig txpConfig scenario

data ForkDepth = ForkShort | ForkMedium | ForkDeep

genSingleFork :: forall g m . (RandomGen g, Monad m)
              => Genesis.Config -> ForkDepth -> BlockEventGenT g m ()
genSingleFork genesisConfig fd = do
    let k = pcK (configProtocolConstants genesisConfig)
    -- 'd' is how deeply in the chain the fork starts. In other words, it's how many
    -- blocks we're going to rollback (therefore must be >1).
    d <- getRandomR $ case fd of
        ForkShort  -> (1, if k > 1 then k-1 else 1)
        ForkMedium -> (if k > 2 then k - 2 else 1, k+2)
        ForkDeep   -> (k+1, div (k*3) 2 + 1)
    -- the depth must be <=k for a successful rollback.
    let expectSuccess = d <= k
    -- original blockchain max index q<(9.5*k)
    q <- getRandomR (d+1, 9 * k + div k 2)
    let
        -- max index of the common prefix. i>0 because d<q
        i = q-d
    -- fork blockchain max index j>i. the upper bound is arbitrary.
    -- dj=j-i
    dj <- getRandomR (1, d*2)
    -- now we can generate paths:
    --
    -- B0 - B1 - B2 - B3 - B4 - B5 - B6 - B7
    --              \
    --                C3 - C4 - C5 - C6
    --
    -- in this example, q=7, d=5, i=2, dj=4
    let
        nonEmptyCuz r [] = error ("Requirement failed: " <> r)
        nonEmptyCuz _ xs = NE.fromList xs
        commonPrefix = pathSequence mempty $
            OldestFirst . nonEmptyCuz "i > 0" $ replicate i "B"
        originalChain = pathSequence mempty $
            OldestFirst . nonEmptyCuz "q > 0" $ replicate q "B"
        rollbackChain = toNewestFirst . pathSequence (stimes i "B") $
            OldestFirst . nonEmptyCuz "d > 0" $ replicate d "B"
        forkChain = pathSequence (NE.last $ getOldestFirst commonPrefix) $
            OldestFirst . nonEmptyCuz "dj > 0" $ replicate dj "C"
    emitBlockApply BlockApplySuccess originalChain
    if expectSuccess
        then do
            emitBlockRollback BlockRollbackSuccess rollbackChain
            emitBlockApply BlockApplySuccess forkChain
        else do
            emitBlockRollback (BlockRollbackFailure BlkRbSecurityLimitExceeded) rollbackChain
