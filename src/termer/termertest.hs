
import Test

import qualified InputterTest
import qualified ScreenTest

tests = "Tests" ~: [
    InputterTest.test, 
    ScreenTest.test
    ]
    

main :: IO ()
main = runtests tests

