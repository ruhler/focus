
import Test

import qualified InputterTest

tests = "Tests" ~: [
    InputterTest.test
    ]
    

main :: IO ()
main = runtests tests

