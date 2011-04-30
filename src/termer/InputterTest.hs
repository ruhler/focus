
module InputterTest (test)
  where

import Control.Monad.State

import Consoler
import Inputter
import Test

data TryState = TryState {
    events :: [Event],
    chars :: String
}

tryget :: State TryState Event
tryget = do
    es <- gets events
    if null es
        then return Quit
        else do
            modify $ \s -> s {events = tail es}
            return $ head es

tryput :: Char -> State TryState ()
tryput c = modify $ \s -> s {chars = (chars s) ++ [c]}

-- Send a sequence of events to an inputter and return the sequence of
-- characters it outputs.
try :: [Event] -> String
try events = chars . snd $ runState (inputter tryget tryput) (TryState events "")

test = "InputterTest" ~: [
    "a" ~: "a" ~=? (try [
        Keypress A, Keyrelease A]),

    "abcd" ~: "abcd" ~=? (try [
        Keypress A, Keyrelease A,
        Keypress B, Keyrelease B,
        Keypress C, Keyrelease C,
        Keypress D, Keyrelease D]),

    "aBc" ~: "aBc" ~=? (try [
        Keypress A, Keyrelease A,
        Keypress LSHIFT,
        Keypress B, Keyrelease B,
        Keyrelease LSHIFT,
        Keypress C, Keyrelease C])
    ]
 
