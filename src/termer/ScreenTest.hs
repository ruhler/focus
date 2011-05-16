
module ScreenTest (test)
  where

import Control.Monad.State

import Screen
import Test

-- Run a sequence of commands on a given sized terminal and return the result
run :: Integer -> Integer -> State Screen a -> a
run cls lns x = fst $ runState x (screen cls lns)

test = "ScreenTest" ~: [
    "initial cursor" ~: Position 0 0 ~=? (run 80 25 $ gets cursor),

    "cursor_address" ~: Position 5 10 ~=? (run 80 25 $ do
        modify $ cursor_address (Position 5 10)
        gets cursor
        ),

    "carriage_return" ~: Position 0 5 ~=? (run 80 25 $ do
        modify $ cursor_address (Position 12 5)
        modify $ carriage_return
        gets cursor
        ),

    "newline" ~: Position 0 6 ~=? (run 80 25 $ do
        modify $ cursor_address (Position 12 5)
        modify $ newline
        gets cursor
        ),

    "tab" ~: Position 16 6 ~=? (run 80 25 $ do
        modify $ cursor_address (Position 10 6)
        modify $ tab
        gets cursor
        ),

    "column_address" ~: Position 16 6 ~=? (run 80 25 $ do
        modify $ cursor_address (Position 1 6)
        modify $ column_address 16
        gets cursor
        ),

    "row_address" ~: Position 16 12 ~=? (run 80 25 $ do
        modify $ cursor_address (Position 16 6)
        modify $ row_address 12
        gets cursor
        ),

    "cursor_down" ~: Position 15 7 ~=? (run 80 25 $ do
        modify $ cursor_address (Position 15 6)
        modify $ cursor_down
        gets cursor
        ),

    "cursor_home" ~: Position 0 0 ~=? (run 80 25 $ do
        modify $ cursor_address (Position 15 6)
        modify $ cursor_home
        gets cursor
        ),

    "cursor_left" ~: Position 5 8 ~=? (run 80 25 $ do
        modify $ cursor_address (Position 6 8)
        modify $ cursor_left
        gets cursor
        ),

    "cursor_right" ~: Position 7 8 ~=? (run 80 25 $ do
        modify $ cursor_address (Position 6 8)
        modify $ cursor_right
        gets cursor
        ),

    "cursor_to_ll" ~: Position 0 24 ~=? (run 80 25 $ do
        modify $ cursor_address (Position 6 8)
        modify $ cursor_to_ll
        gets cursor
        ),

    "cursor_up" ~: Position 6 7 ~=? (run 80 25 $ do
        modify $ cursor_address (Position 6 8)
        modify $ cursor_up
        gets cursor
        ),

    "parm_left_cursor" ~: Position 3 9 ~=? (run 80 25 $ do
        modify $ cursor_address (Position 26 9)
        modify $ parm_left_cursor 23
        gets cursor
        ),

    "parm_right_cursor" ~: Position 26 9 ~=? (run 80 25 $ do
        modify $ cursor_address (Position 3 9)
        modify $ parm_right_cursor 23
        gets cursor
        ),

    "parm_down_cursor" ~: Position 26 15 ~=? (run 80 25 $ do
        modify $ cursor_address (Position 26 4)
        modify $ parm_down_cursor 11
        gets cursor
        ),

    "put_char puts" ~: Cell 'a' (Attributes WHITE BLACK (Style False False))
        ~=? (run 6 4 $ do
            modify $ cursor_address (Position 2 3)
            modify $ put_char 'a'
            gets $ cellat (Position 2 3)
            ),

    "put_char moves" ~: Position 3 3
        ~=? (run 6 4 $ do
            modify $ cursor_address (Position 2 3)
            modify $ put_char 'a'
            gets cursor
            ),

    "put_char am" ~: Position 0 2
        ~=? (run 6 4 $ do
            modify $ cursor_address (Position 5 1)
            modify $ put_char 'a'
            gets cursor
            ),

    "put_char multiple" ~: Cell 'J' (Attributes WHITE BLACK (Style False False))
        ~=? (run 6 4 $ do
            mapM_ (modify . put_char) "abcdefGHIJKLmno"
            gets $ cellat (Position 3 1)
            ),

    "clear_screen homes" ~: Position 0 0
        ~=? (run 6 4 $ do
            mapM_ (modify . put_char) "abcdefGHIJKLmno"
            modify $ clear_screen
            gets cursor
            ),

    "clear_screen clears" ~: Cell ' ' (Attributes WHITE BLACK (Style False False))
        ~=? (run 6 4 $ do
            mapM_ (modify . put_char) "abcdefGHIJKLmno"
            modify $ clear_screen
            gets $ cellat (Position 3 1)
            ),

    "put_char scrolls at end" ~: Cell 'J' (Attributes WHITE BLACK (Style False False))
        ~=? (run 6 4 $ do
            modify $ cursor_address (Position 0 3) 
            mapM_ (modify . put_char) "abcdefGHIJKLmno"
            gets $ cellat (Position 3 2)
            ),

    "clr_eos clears full line" ~: Cell ' ' (Attributes WHITE BLACK (Style False False))
        ~=? (run 6 3 $ do
            mapM_ (modify . put_char) "abcdefGHIJKLmno"
            modify $ cursor_home
            modify $ clr_eos
            gets $ cellat (Position 4 1)
            ),

    "newline at bottom scrolls" ~: Cell 'K' (Attributes WHITE BLACK (Style False False))
        ~=? (run 6 4 $ do
            modify $ cursor_down
            mapM_ (modify . put_char) "abcdefGHIJKLmno"
            modify $ cursor_address (Position 0 3)
            modify $ newline
            gets $ cellat (Position 4 1)
            )

    ]

