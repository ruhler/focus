
module Outputter (outputter)
  where

import Debug.Trace
import Data.Char

import Screen

-- Convert a digit to an integer.
digit2int :: Char -> Integer
digit2int c = toInteger $ (ord c) - (ord '0')

-- Given a function to get the next character in an input stream,
--  return the next integer in the input stream and following character.
--  If there is not an integer at the front of the stream, returns Nothing for
--  the integer value.
getnum :: (Monad m) => m Char -> m (Maybe Integer, Char)
getnum getf = do
    c <- getf
    if isDigit c
      then getnum_aux (digit2int c) getf
      else return (Nothing, c)

-- Auxilliary function to help with getnum. Takes the current integer as
-- input.
getnum_aux :: (Monad m) => Integer -> m Char -> m (Maybe Integer, Char)
getnum_aux x getf = do
    c <- getf
    if isDigit c
      then getnum_aux (x*10 + (digit2int c)) getf
      else return (Just x, c)

    

-- outputter term getf updatef
--  term - character sent when outputter should stop (or Nothing to go forever)
--  getf - function which gets the next character.
--  updatef - function to call to update the screen.
outputter :: (Monad m) => Maybe Char -> m Char -> ((Screen -> Screen) -> m ()) -> m ()
outputter term getf updatef = do
    c <- getf
    case term of
      Just x | x == c -> return ()
      _ -> do
        case c of
          '\LF' -> updatef cursor_down
          '\BS' -> updatef cursor_left
          '\CR' -> updatef carriage_return
          '\ESC' -> do
            c <- getf
            case c of
              '[' -> do
                c <- getnum getf
                case c of
                  (Nothing, '@') -> updatef insert_character
                  (Nothing, 'A') -> updatef cursor_up
                  (Nothing, 'B') -> updatef cursor_down
                  (Nothing, 'C') -> updatef cursor_right
                  (Nothing, 'D') -> updatef cursor_left
                  (Nothing, 'H') -> updatef cursor_home
                  (Nothing, 'I') -> updatef tab
                  (Nothing, 'J') -> updatef clr_eos
                  (Nothing, 'K') -> updatef clr_eol
                  (Nothing, 'L') -> updatef insert_line
                  (Nothing, 'M') -> updatef delete_line
                  (Nothing, 'P') -> updatef delete_character
                  (Nothing, 'm') -> updatef exit_attribute_mode
                  (Just 1, 'K') -> updatef clr_bol
                  (Just 0, 'm') -> updatef exit_attribute_mode
                  (Just 1, 'm') -> updatef enter_bold_mode
                  (Just 7, 'm') -> updatef enter_reverse_mode
                  (Just 22, 'm') -> updatef exit_bold_mode
                  (Just 30, 'm') -> updatef $ set_foreground BLACK
                  (Just 31, 'm') -> updatef $ set_foreground RED
                  (Just 32, 'm') -> updatef $ set_foreground GREEN
                  (Just 33, 'm') -> updatef $ set_foreground YELLOW
                  (Just 34, 'm') -> updatef $ set_foreground BLUE
                  (Just 35, 'm') -> updatef $ set_foreground MAGENTA
                  (Just 36, 'm') -> updatef $ set_foreground CYAN
                  (Just 37, 'm') -> updatef $ set_foreground WHITE
                  (Just 39, 'm') -> updatef $ set_foreground WHITE
                  (Just 40, 'm') -> updatef $ set_background BLACK
                  (Just 41, 'm') -> updatef $ set_background RED
                  (Just 42, 'm') -> updatef $ set_background GREEN
                  (Just 43, 'm') -> updatef $ set_background YELLOW
                  (Just 44, 'm') -> updatef $ set_background BLUE
                  (Just 45, 'm') -> updatef $ set_background MAGENTA
                  (Just 46, 'm') -> updatef $ set_background CYAN
                  (Just 47, 'm') -> updatef $ set_background WHITE
                  (Just 49, 'm') -> updatef $ set_background WHITE
                  (Just x, 'A') -> updatef $ parm_up_cursor x
                  (Just x, 'B') -> updatef $ parm_down_cursor x
                  (Just x, 'C') -> updatef $ parm_right_cursor x
                  (Just x, 'D') -> updatef $ parm_left_cursor x
                  (Just x, 'G') -> updatef $ column_address (x-1)
                  (Just x, 'L') -> updatef $ parm_insert_line x
                  (Just x, 'M') -> updatef $ parm_delete_line x
                  (Just x, 'P') -> updatef $ parm_dch x
                  (Just x, 'S') -> updatef $ parm_index x
                  (Just x, 'T') -> updatef $ parm_rindex x
                  (Just x, 'X') -> updatef $ erase_chars x
                  (Just x, '@') -> updatef $ parm_ich x
                  (Just x, 'd') -> updatef $ row_address (x-1)
                  (Just x, ';') -> do
                      c <- getnum getf
                      case c of
                          (Just 10, 'm') | x == 0 -> updatef exit_attribute_mode
                          (Just y, 'H') -> updatef $ cursor_address (Position y x)
                          _ -> trace ("unhandled control sequence ESC[" ++ (show x) ++ ";" ++ (show c)) (return ())
                  x -> trace ("unhandled control sequence ESC[" ++ (show x)) (return ())
          c -> updatef $ put_char c         
        outputter term getf updatef

