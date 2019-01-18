import           Universum

import           Test.Hspec (hspec, parallel)

import           Spec (spec)
import           Test.Pos.Configuration (defaultTestConf)
import           Test.Pos.Util.Parallel.Parallelize (parallelizeAllCores)
import           Test.Pos.Block.Logic.VarSpec (runTest)

main :: IO ()
main =
    if False
        then do
            parallelizeAllCores
            putText $ "default configuration: " <> show defaultTestConf
            hspec $ parallel spec
        else do
            putText $ "default configuration: " <> show defaultTestConf
            runTest
