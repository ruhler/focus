
module Test ((~:), (~=?), runtests)
  where

import System
import Test.HUnit

-- Pass
pass :: Test
pass = test returnIO
  where returnIO :: IO ()
        returnIO = return ()

-- Run tests, exiting failure if any failed, exiting success if all succeeded.
runtests :: Test -> IO ()
runtests t = do
    cnts <- runTestTT t
    putStrLn $ show cnts
    if (errors cnts + failures cnts > 0)
        then exitFailure
        else exitWith ExitSuccess

