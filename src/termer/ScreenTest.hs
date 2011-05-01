
module ScreenTest (test)
  where

import Control.Monad.State

import Screen
import Test

-- Run a sequence of commands on a 80x25 terminal and return the result
run :: State Screen a -> a
run x = fst $ runState x (screen 80 25)

test = "ScreenTest" ~: [
    "initial cursor" ~: Position 0 0 ~=? (run $ gets cursor),

    "cursor_address" ~: Position 5 10 ~=? (run $ do
        modify $ cursor_address (Position 5 10)
        gets cursor
        )

    ]

