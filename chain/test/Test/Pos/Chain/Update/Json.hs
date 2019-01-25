{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.Chain.Update.Json
       ( tests
       ) where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.Pos.Chain.Update.Gen (genBlockVersionData,
                     genSoftforkRule)
import           Test.Pos.Util.Golden (eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip,
                     roundTripsAesonYamlBuildable)

--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

roundTripBlockVersionData :: Property
roundTripBlockVersionData =
    eachOf 1000 genBlockVersionData roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

roundTripSoftforkRule :: Property
roundTripSoftforkRule = eachOf 1000 genSoftforkRule roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- Main Testing Function
--------------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$discoverRoundTrip
