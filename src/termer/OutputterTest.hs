
module OutputterTest (test)
  where

import Control.Monad.State

import Outputter
import Screen
import Test

--  Run a sequence of commands on a given sized terminal and return the result.
runexp :: Integer -> Integer -> State Screen () -> Screen
runexp cls lns x = snd $ runState x (screen cls lns)


type TS = (String, Screen)

getf :: State TS Char
getf = do
    s <- gets fst
    if null s
      then return '.'
      else (modify $ \(_, scr) -> (tail s, scr)) >> return (head s)

updatef :: (Screen -> Screen) -> State TS () 
updatef f = modify $ \(str, scr) -> (str, f scr)

-- Run an outputter on the given characters and sized terminal and return
-- resulting screen.
runwnt :: Integer -> Integer -> String -> Screen
runwnt cls lns chrs = snd . snd $ runState (outputter (Just '.') getf updatef) (chrs, screen cls lns)


-- Test an outputter test by passing the chars to an outputter, and running the
-- commands manually, and checking to make sure the resulting screens are the
-- same.
run :: Integer -> Integer -> String -> State Screen () -> Test
run cls lns chrs x = runexp cls lns x ~=? runwnt cls lns chrs

test = "OutputterTest" ~: [
    "simple" ~: (run 6 4 "a" $ do
        modify $ put_char 'a'
        ),

    "multi" ~: (run 6 4 "abc" $ do
        modify $ put_char 'a'
        modify $ put_char 'b'
        modify $ put_char 'c'
        ),

    "newline" ~: (run 6 4 "ab\n\rc" $ do
        modify $ put_char 'a'
        modify $ put_char 'b'
        modify $ newline
        modify $ put_char 'c'
        ),

    "backspace" ~: (run 6 4 "ab\BSc" $ do
        modify $ put_char 'a'
        modify $ put_char 'b'
        modify $ cursor_left
        modify $ put_char 'c'
        ),

    "ESC[m" ~: (run 6 4 "ab\ESC[mc" $ do
        modify $ put_char 'a'
        modify $ put_char 'b'
        modify $ exit_attribute_mode
        modify $ put_char 'c'
        ),

    "ESC[0m" ~: (run 6 4 "ab\ESC[0mc" $ do
        modify $ put_char 'a'
        modify $ put_char 'b'
        modify $ exit_attribute_mode
        modify $ put_char 'c'
        ),

    "sgr: ESC[0;10m" ~: (run 6 4 "a\ESC[1mb\ESC[0;10mc" $ do
        modify $ put_char 'a'
        modify $ enter_bold_mode
        modify $ put_char 'b'
        modify $ exit_attribute_mode
        modify $ put_char 'c'
        ),

    "boldoff: ESC[22m" ~: (run 6 4 "a\ESC[1mb\ESC[22mc" $ do
        modify $ put_char 'a'
        modify $ enter_bold_mode
        modify $ put_char 'b'
        modify $ exit_bold_mode
        modify $ put_char 'c'
        ),

    "cup: ESC[4;3H" ~: (run 6 4 "\ESC[4;3H" $ do
        modify $ cursor_address (Position 3 4)
        ),

    "cuf: ESC[4C" ~: (run 6 4 "\ESC[4C" $ do
        modify $ parm_right_cursor 4
        )

    ]

