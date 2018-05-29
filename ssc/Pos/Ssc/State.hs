{-# LANGUAGE TypeFamilies #-}

module Pos.Ssc.State
       ( mkSscState
       , module Pos.Ssc.State.Global
       , module Pos.Ssc.State.Local
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Pos.Util.Log (WithLogger)

import           Pos.DB (MonadDBRead)
import           Pos.Slotting.Class (MonadSlots)
import           Pos.Ssc.Types (SscState (..))

-- Reexports
import           Pos.Ssc.State.Global
import           Pos.Ssc.State.Local

mkSscState
    :: forall ctx m .
       ( WithLogger m
       , MonadDBRead m
       , MonadSlots ctx m
       )
    => m SscState
mkSscState = do
    gState <- sscLoadGlobalState
    ld <- sscNewLocalData
    liftIO $ SscState <$> STM.newTVarIO gState <*> STM.newTVarIO ld
