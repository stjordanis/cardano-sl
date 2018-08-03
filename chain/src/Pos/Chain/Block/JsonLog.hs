module Pos.Chain.Block.JsonLog
       ( jlCreatedBlock
       , jlAdoptedBlock
       ) where

import           Formatting (sformat)
import           Universum

import           Pos.Core (HasConfiguration, SlotId (..), getEpochIndex,
                     getSlotIndex, mkLocalSlotIndex)
import           Pos.Core.Block (Block, HeaderHash, gbHeader, gbhPrevBlock,
                     headerHash, headerHashF, mainBlockTxPayload)
import           Pos.Core.Block.Genesis (genBlockEpoch)
import           Pos.Core.Block.Union (mainBlockSlot)
import           Pos.Core.JsonLog.LogEvents (JLBlock (..), JLEvent (..))
import           Pos.Core.Txp (txpTxs)
import           Pos.Crypto (hash, hashHexF)

-- | Return event of created block.
jlCreatedBlock :: HasConfiguration => Block -> JLEvent
jlCreatedBlock block = JLCreatedBlock $ JLBlock {..}
  where
    jlHash = showHeaderHash $ headerHash block
    jlPrevBlock = showHeaderHash $ case block of
        Left  gB -> view gbhPrevBlock (gB ^. gbHeader)
        Right mB -> view gbhPrevBlock (mB ^. gbHeader)
    jlSlot = (getEpochIndex $ siEpoch slot, getSlotIndex $ siSlot slot)
    jlTxs = case block of
              Left _   -> []
              Right mB -> map fromTx . toList $ mB ^. mainBlockTxPayload . txpTxs
    slot :: SlotId
    slot = case block of
        Left  gB -> let slotZero = case mkLocalSlotIndex 0 of
                                        Right sz -> sz
                                        Left _   -> error "impossible branch"
                    in SlotId (gB ^. genBlockEpoch) slotZero
        Right mB -> mB ^. mainBlockSlot
    fromTx = sformat hashHexF . hash

-- | Returns event of created 'Block'.
jlAdoptedBlock :: Block -> JLEvent
jlAdoptedBlock = JLAdoptedBlock . showHeaderHash . headerHash

showHeaderHash :: HeaderHash -> Text
showHeaderHash = sformat headerHashF
