
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

    
-- Given the update function to change a mode for the given integer
mode :: Integer -> (Screen -> Screen)
mode 0 = exit_attribute_mode
mode 1 = enter_bold_mode
mode 7 = enter_reverse_mode
mode 10 = id    -- primary font ??
mode 22 = exit_bold_mode
mode 30 = set_foreground BLACK
mode 31 = set_foreground RED
mode 32 = set_foreground GREEN
mode 33 = set_foreground YELLOW
mode 34 = set_foreground BLUE
mode 35 = set_foreground MAGENTA
mode 36 = set_foreground CYAN
mode 37 = set_foreground WHITE
mode 39 = set_foreground WHITE
mode 40 = set_background BLACK
mode 41 = set_background RED
mode 42 = set_background GREEN
mode 43 = set_background YELLOW
mode 44 = set_background BLUE
mode 45 = set_background MAGENTA
mode 46 = set_background CYAN
mode 47 = set_background WHITE
mode 49 = set_background WHITE

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
          '\HT' -> updatef tab
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
                  (Just x, 'm') -> updatef $ mode x
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
                          (Just y, 'm') -> do
                              updatef $ mode x
                              updatef $ mode y
                          (Just y, 'H') -> updatef $ cursor_address (Position (y-1) (x-1))
                          _ -> trace ("unhandled control sequence ESC[" ++ (show x) ++ ";" ++ (show c)) (return ())
                  x -> trace ("unhandled control sequence ESC[" ++ (show x)) (return ())
              x -> trace ("unhandled escape sequence ESC" ++ (show x)) (return ())
          c -> updatef $ put_char c         
        outputter term getf updatef

