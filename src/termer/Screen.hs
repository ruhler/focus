
module Screen (
    Color(..), Style(..), Cell(..), Position(..), Screen(), Attributes(..),
    screen,
    carriage_return, newline, tab, column_address, row_address, 
    cursor_address, cursor_down, cursor_home, cursor_left, cursor_right,    
    cursor_to_ll, cursor_up, parm_left_cursor, parm_right_cursor, parm_up_cursor,
    parm_down_cursor, clear_screen, clr_bol, clr_eol, clr_eos,
    enter_bold_mode, enter_reverse_mode, exit_attribute_mode,
    set_foreground, set_background, delete_character, parm_dch,
    delete_line, parm_delete_line, erase_chars, insert_character,
    parm_ich, insert_line, parm_insert_line, scroll_forward,
    parm_index, scroll_reverse, parm_rindex,
    put_char, cursor, cellat 
    )
  where

import Prelude hiding (reverse, lines)
import Data.Array

data Color = BLACK | RED | GREEN | YELLOW | BLUE | MAGENTA | CYAN | WHITE
    deriving(Eq, Show)

instance Enum Color
  where
    fromEnum BLACK = 0
    fromEnum RED = 1
    fromEnum GREEN = 2
    fromEnum YELLOW = 3
    fromEnum BLUE = 4
    fromEnum MAGENTA = 5
    fromEnum CYAN = 6
    fromEnum WHITE = 7

    toEnum 0 = BLACK
    toEnum 1 = RED
    toEnum 2 = GREEN
    toEnum 3 = YELLOW
    toEnum 4 = BLUE
    toEnum 5 = MAGENTA
    toEnum 6 = CYAN
    toEnum 7 = WHITE


data Style = Style {
    reverse :: Bool,
    bold :: Bool
} deriving (Eq, Show)

normal :: Style
normal = Style False False

data Attributes = Attributes {
    fgcolor :: Color,
    bgcolor :: Color,
    style :: Style
} deriving (Eq, Show)

default_attributes :: Attributes
default_attributes = Attributes WHITE BLACK normal

data Cell = Cell {
    character :: Char,
    cattrs :: Attributes
} deriving (Eq, Show)

defaultcell :: Cell
defaultcell = Cell ' ' default_attributes

-- The first column is 0
-- The first line is 0
data Position = Position {
    column :: Integer,
    line :: Integer
} deriving(Eq, Show, Ord, Ix)

  

-- The home cursor position
home :: Position
home = Position 0 0

data Screen = Screen {
    columns :: Integer,
    lines :: Integer,
    cursor :: Position,
    cells :: Array Position Cell,
    sattrs :: Attributes
} deriving (Eq, Show)

-- make an array where every element is the same
uniformArray :: (Ix i) => (i, i) -> a -> Array i a
uniformArray bounds x = listArray bounds (repeat x)

-- screen columns lines
-- Return a clear screen with the given number of rows and columns.
screen :: Integer -> Integer -> Screen
screen cols lns
  = let bounds = (Position 0 0, Position cols lns)
        cells = uniformArray bounds defaultcell
    in Screen cols lns home cells default_attributes

blank :: Screen -> Cell
blank scr = Cell ' ' (sattrs scr)



-- carriage_return
-- Move cursor to the left edge of the current row.
carriage_return :: Screen -> Screen
carriage_return scr = column_address 0 scr

-- Move the the first column of the next line
newline :: Screen -> Screen
newline = cursor_down . carriage_return

-- Advance the cursor column to the next 8-space tab stop
tab :: Screen -> Screen
tab scr = parm_right_cursor (8 - ((column . cursor $ scr) `mod` 8)) scr

-- column_address column
-- Move the cursor to the given column
column_address :: Integer -> Screen -> Screen
column_address ncol scr
  = cursor_address ( (cursor scr) { column = ncol } ) scr

-- row_address row
-- Move the cursor to the given row
row_address :: Integer -> Screen -> Screen
row_address nrow scr
  = cursor_address ( (cursor scr) { line = nrow } ) scr

-- cursor_address pos
-- Move the cursor to the given position
cursor_address :: Position -> Screen -> Screen
cursor_address pos scr = scr { cursor = pos }

-- Move cursor down one line
cursor_down :: Screen -> Screen
cursor_down = parm_down_cursor 1

-- Move cursor to the home position
cursor_home :: Screen -> Screen
cursor_home = cursor_address home

-- Move the cursor left one column
cursor_left :: Screen -> Screen
cursor_left = parm_left_cursor 1

-- Move the cursor right one column
-- It is undefined what happens at the right edge of the terminal.
cursor_right :: Screen -> Screen
cursor_right = parm_right_cursor 1

-- Move the cursor to the last line, first column
cursor_to_ll :: Screen -> Screen
cursor_to_ll scr = cursor_address (Position 0 ((lines scr)-1)) scr

-- Move the cursor up one column
cursor_up :: Screen -> Screen
cursor_up = parm_up_cursor 1

-- Move the cursor left n columns
parm_left_cursor :: Integer -> Screen -> Screen
parm_left_cursor n scr = column_address ((column . cursor $ scr) - n) scr

-- Move the cursor right n columns
parm_right_cursor :: Integer -> Screen -> Screen
parm_right_cursor n scr = column_address ((column . cursor $ scr) + n) scr

-- Move the cursor up n lines
parm_up_cursor :: Integer -> Screen -> Screen
parm_up_cursor n scr = row_address ((line . cursor $ scr) - n) scr

-- Move the cursor down n lines
parm_down_cursor :: Integer -> Screen -> Screen
parm_down_cursor n scr = row_address ((line . cursor $ scr) + n) scr


-- Clear the screen and move the cursor to the home position
clear_screen :: Screen -> Screen
clear_screen scr 
  = let bounds = (Position 0 0, Position (columns scr) (lines scr))
        cells = uniformArray bounds (blank scr)
    in scr { cells = cells, cursor = home }

-- Clear from the beggining of the line to the current cursor position
-- inclusive, leaving the cursor where it is.
clr_bol :: Screen -> Screen
clr_bol scr 
  = let cline = line . cursor $ scr
        ccol = column . cursor $ scr
        upds = [(Position x cline, blank scr) | x <- [0..ccol]]
        ncells = (cells scr)//upds
    in scr { cells = ncells }

-- Clear from the cursor position to the end of the line, leaving the cursor
-- where it is.
clr_eol :: Screen -> Screen
clr_eol scr
  = let cline = line . cursor $ scr
        ccol = column . cursor $ scr
        upds = [(Position x cline, blank scr) | x <- [ccol..(columns scr)]]
        ncells = (cells scr)//upds
    in scr { cells = ncells }

-- Clear from the current cursor position to the end of the display.
clr_eos :: Screen -> Screen
clr_eos scr
  = let cline = line . cursor $ scr
        ccol = column . cursor $ scr
        lupds = [(Position x cline, blank scr) | x <- [ccol..(columns scr)]]
        supds = [(Position x y, blank scr) | x <- [0..(lines scr)], y <- [(cline+1)..(lines scr)]]
        upds = lupds ++ supds
        ncells = (cells scr)//upds
    in scr { cells = ncells }

-- Enter bold mode
enter_bold_mode :: Screen -> Screen
enter_bold_mode scr
  = let attrs = sattrs scr
        nattrs = attrs { style = ((style attrs) { bold = True }) }
    in scr { sattrs = nattrs }

-- Enter reverse mode
enter_reverse_mode :: Screen -> Screen
enter_reverse_mode scr
  = let attrs = sattrs scr
        nattrs = attrs { style = ((style attrs) { reverse = True }) }
    in scr { sattrs = nattrs }

-- Turn off all attributes
exit_attribute_mode scr = scr { style = normal, fgcolor = WHITE, bgcolor = BLACK }

-- Set the forground color
set_foreground :: Color -> Screen -> Screen
set_foreground c scr = scr { sattrs = ((sattrs scr) { fgcolor = c}) }

-- Set the background color
set_background :: Color -> Screen -> Screen
set_background c scr = scr { sattrs = ((sattrs scr) { bgcolor = c}) }

-- Delete the character at the cursor position
delete_character :: Screen -> Screen
delete_character = parm_dch 1

-- Delete n characters from the cursor position
parm_dch :: Integer -> Screen -> Screen
parm_dch n scr
  = let cline = line . cursor $ scr
        ccol = column . cursor $ scr
        
        f x = if (x + n >= columns scr)
                  then blank scr
                  else (cells scr) ! (Position (x+n) cline)
        
        upds = [(Position x cline, f x) | x <- [ccol..(columns scr)]]
        ncells = (cells scr)//upds
    in scr { cells = ncells }

-- Delete the line at the cursor position
delete_line :: Screen -> Screen
delete_line = parm_delete_line 1

-- Delete n lines starting at the cursor position
parm_delete_line :: Integer -> Screen -> Screen
parm_delete_line n scr
  = let cline = line . cursor $ scr
        f x y = if (y + n >= lines scr)
                  then blank scr
                  else (cells scr) ! (Position x (y+n))
        upds = [(Position x y, f x y) | x <- [0..(columns scr)], y <- [cline..(lines scr)]] 
        ncells = (cells scr)//upds
    in scr { cells = ncells }

-- erase n characters (clear them) at the cursor position without moving the
-- cursor.
erase_chars :: Integer -> Screen -> Screen
erase_chars n scr
  = let cline = line . cursor $ scr
        ccol = column . cursor $ scr
        upds = [(Position x cline, blank scr) | x <- [ccol..(ccol+n)]]
        ncells = (cells scr)//upds
    in scr { cells = ncells }

-- Insert a (blank) character at the cursor position without moving the cursor
-- position. Shifts following characters on the line over.
insert_character :: Screen -> Screen
insert_character = parm_ich 1

-- Insert n characters at the cursor position without moving the cursor
-- position. Shifts following characters on the line over.
parm_ich :: Integer -> Screen -> Screen
parm_ich n scr 
  = let cline = line . cursor $ scr
        ccol = column . cursor $ scr
        
        f x = if (x - n >= 0)
                  then (cells scr) ! (Position (x-n) cline)
                  else blank scr
        
        blks = [(Position x cline, blank scr) | x <- [ccol..(ccol + n)]]
        shfts = [(Position x cline, f x) | x <- [(ccol+n)..(columns scr)]]
        upds = blks ++ shfts
        ncells = (cells scr)//upds
    in scr { cells = ncells }

-- Insert a line at the cursor position without moving the cursor.
insert_line :: Screen -> Screen
insert_line = parm_insert_line 1

-- Insert n lines at the cursor position
parm_insert_line :: Integer -> Screen -> Screen
parm_insert_line n scr
  = let cline = line . cursor $ scr
        f x y = if (y - n >= 0)
                  then (cells scr) ! (Position x (y-n))
                  else blank scr
        
        blks = [(Position x y, blank scr) | x <- [0..(columns scr)], y <- [cline..(cline+n)]]
        shfts = [(Position x y, f x y) | x <- [0..(columns scr)], y <- [(cline+n)..(lines scr)]]
        upds = blks ++ shfts
        ncells = (cells scr)//upds
    in scr { cells = ncells }

-- Scroll forward one line
scroll_forward :: Screen -> Screen
scroll_forward = parm_index 1

-- Scroll forward n lines
parm_index :: Integer -> Screen -> Screen
parm_index = scroll

-- Scroll backward 1 lines
scroll_reverse :: Screen -> Screen
scroll_reverse = parm_rindex 1

-- Scroll backwards n lines
parm_rindex :: Integer -> Screen -> Screen
parm_rindex n = scroll (-n)

  
-- Scroll n lines
-- scrolls forward if n is negative, reverse if n is positive.
scroll :: Integer -> Screen -> Screen
scroll n scr
  = let cols = columns scr
        lns = lines scr
        bounds = (Position 0 0, Position cols lns)
        f (Position x y)
          = if (y + n >=0 && y + n < lns)
            then (cells scr) ! (Position x (y+n))
            else blank scr
        ncells = array bounds [(Position x y, f (Position x y)) | x <- [0..cols], y <- [0..lns]]
    in scr { cells = ncells }



-- Place a character on the screen at the given cursor position. Advances the
-- cursor position.
-- We have auto_margin, so cursor is in the last column, after putting the
-- character the cursor will go down to the beginning of the next line.
put_char :: Char -> Screen -> Screen
put_char c scr
  = let cl = line . cursor $ scr
        cc = column . cursor $ scr
        ncursor = if (cc +1 >= columns scr)
                    then Position 0 (cl+1)
                    else Position (cc+1) cl
        nscr = scr { cells = ((cells scr)//[(cursor scr, (blank scr) { character = c } )]) }
    in cursor_address ncursor nscr

-- Get the cell at the given position
cellat :: Position -> Screen -> Cell
cellat pos scr = (cells scr) ! pos

