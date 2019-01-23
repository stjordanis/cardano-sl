{-# LANGUAGE RecordWildCards #-}

-- | Functions operation on 'SlogContext' and its subtypes.

module Pos.DB.Block.Slog.Context
       ( mkSlogGState
       , mkSlogContext
       , cloneSlogGState
       , slogGetLastSlots
       , slogPutLastSlots
       , slogRollbackLastSlots

       , validateFlatSlotIds
       ) where

import           Universum

import qualified Data.List as List
import           Formatting (int, sformat, stext, (%))
import qualified System.Metrics as Ekg

import           Pos.Chain.Block (HasBlockConfiguration, HasSlogGState (..),
                     LastBlkSlots, LastSlotInfo (..), SlogContext (..), SlogGState (..),
                     fixedTimeCQSec, sgsLastBlkSlots)
import           Pos.Core (BlockCount, FlatSlotId)
import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Core.Metrics.Constants (withCardanoNamespace)
import           Pos.Core.Reporting (MetricMonitorState, mkMetricMonitorState)
import           Pos.DB.Block.GState.BlockExtra (getLastSlots, putLastSlots,
                     rollbackLastSlots)
import           Pos.DB.Class (MonadDB, MonadDBRead)

import           Serokell.Util (listJson)
import           Pos.Util.Wlog (CanLog, HasLoggerName, logDebug)
import           System.Exit (ExitCode (..))
import           System.IO (hFlush, stderr, stdout)
import           System.Posix.Process (exitImmediately)

-- | Make new 'SlogGState' using data from DB.
mkSlogGState :: (MonadIO m, MonadDBRead m) => m SlogGState
mkSlogGState = do
    _sgsLastBlkSlots <- getLastSlots >>= newIORef
    return SlogGState {..}

-- | Make new 'SlogContext' using data from DB.
mkSlogContext
    :: forall m
     . (MonadIO m, MonadDBRead m, HasBlockConfiguration)
    => BlockCount
    -> Ekg.Store
    -> m SlogContext
mkSlogContext k store = do
    _scGState <- mkSlogGState

    let mkMMonitorState :: Text -> m (MetricMonitorState a)
        mkMMonitorState = flip mkMetricMonitorState store
    -- Chain quality metrics stuff.
    let metricNameK = sformat ("chain_quality_last_k_("%int%")_blocks_%") k
    let metricNameOverall = "chain_quality_overall_%"
    let metricNameFixed =
            sformat ("chain_quality_last_"%int%"_sec_%")
                fixedTimeCQSec
    _scCQkMonitorState <- mkMMonitorState metricNameK
    _scCQOverallMonitorState <- mkMMonitorState metricNameOverall
    _scCQFixedMonitorState <- mkMMonitorState metricNameFixed

    -- Other metrics stuff.
    _scDifficultyMonitorState <- mkMMonitorState "total_main_blocks"
    _scEpochMonitorState <- mkMMonitorState "current_epoch"
    _scLocalSlotMonitorState <- mkMMonitorState "current_local_slot"
    _scGlobalSlotMonitorState <- mkMMonitorState "current_global_slot"
    let crucialValuesName = withCardanoNamespace "crucial_values"
    _scCrucialValuesLabel <-
        liftIO $ Ekg.createLabel crucialValuesName store
    return SlogContext {..}

-- | Make a copy of existing 'SlogGState'.
cloneSlogGState :: (MonadIO m) => SlogGState -> m SlogGState
cloneSlogGState SlogGState {..} =
    SlogGState <$> (readIORef _sgsLastBlkSlots >>= newIORef)

-- | Read 'LastBlkSlots' from in-memory state.
slogGetLastSlots ::
       (MonadReader ctx m, HasSlogGState ctx, MonadIO m) => m LastBlkSlots
slogGetLastSlots = do
    -- 'LastBlkSlots' is stored in two places, the DB and an 'IORef' so just
    -- grab the copy in the 'IORef'.
    xs <- readIORef =<< view (slogGState . sgsLastBlkSlots)
    validateFlatSlotIds "slogGetLastSlots" (toList $ map lsiFlatSlotId xs)
    pure xs

-- | Update 'LastBlkSlots' in 'SlogContext'.
slogPutLastSlots ::
       (MonadReader ctx m, MonadDB m, HasSlogGState ctx, MonadIO m)
    => Text -> LastBlkSlots
    -> m ()
slogPutLastSlots fname slots = do
    validateFlatSlotIds fname (toList $ map lsiFlatSlotId slots)
    -- When we set 'LastBlkSlots' we set it in both the DB and the 'IORef'.
    view (slogGState . sgsLastBlkSlots) >>= flip writeIORef slots
    putLastSlots slots

-- | Roll back the specified count of 'LastBlkSlots'.
slogRollbackLastSlots
    :: (CanLog m, HasLoggerName m, MonadReader ctx m, MonadDB m, HasSlogGState ctx, MonadIO m)
    => Genesis.Config -> Int -> m ()
slogRollbackLastSlots genesisConfig count = do
    -- Roll back in the DB, then read the DB and set the 'IORef'.
    logDebug "Pos.DB.Block.Slog.Context.slogRollbackLastSlots: About to call rollbackLastSlots"
    rollbackLastSlots genesisConfig count
    slots <- getLastSlots
    validateFlatSlotIds "slogRollbackLastSlots" (toList $ map lsiFlatSlotId slots)
    view (slogGState . sgsLastBlkSlots) >>= flip writeIORef slots


validateFlatSlotIds :: MonadIO m => Text -> [FlatSlotId] -> m ()
validateFlatSlotIds fname xs =
    when (List.sort xs /= xs) $ do
        putTextLn $ sformat ("\n\n\n" % stext % " sort error " % listJson % "\n") fname xs
        liftIO $ do
            hFlush stdout
            hFlush stderr
            exitImmediately (ExitFailure 42)

