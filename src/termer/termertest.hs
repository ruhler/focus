
import Test

import qualified InputterTest
import qualified OutputterTest
import qualified ScreenTest

tests = "Tests" ~: [
    InputterTest.test, 
    OutputterTest.test,
    ScreenTest.test
    ]
    

main :: IO ()
main = runtests tests

