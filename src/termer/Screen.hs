
module Screen (
    Color(..), Style(..), Cell(..), Position(..), Screen(),
    screen, clear, moveto, up, down, left, right, home
    )
  where

import Data.Array

data Color = BLACK | RED | GREEN | YELLOW | BLUE | MAGENTA | CYAN | WHITE
    deriving(Eq, Show)

data Style = Style {
    reverse :: Bool
    bold :: Bool
}

normal :: Style
normal = Style False False

data Cell = Cell [
    character :: Char,
    fgcolor :: Color,
    bgcolor :: Color,
    style :: Style
}

defaultcell :: Cell
defaultcell = Cell ' ' WHITE BLACK normal

-- The first line is 1
-- The first column is 1
data Position = {
    column :: Integer
    line :: Integer,
}

-- The home cursor position
home :: Position
home = Position 1 1

data Screen = {
    columns :: Integer,
    lines :: Integer,
    cursor :: Position,
    cells :: Array Position Cell
    fgcolor :: Color,
    bgcolor :: Color,
    style :: Style
}

-- make an array where every element is the same
uniformArray :: (Idx i) -> (i, i) -> a -> Array i a
uniformArray bounds x = listArray bounds (repeat x)

-- screen columns lines
-- Return a clear screen with the given number of rows and columns.
screen :: Integer -> Integer -> Screen
screen cols lines
  = let bounds = (Position 1 1, Position cols lines)
        cells = uniformArray bounds defaultcell
    in Screen cols lines home cells WHITE BLACK normal

clearcell :: Screen -> Cell
clearcell scr = Cell ' ' (fgcolor scr) (bgcolor scr) (style csr)

-- clear the screen and moves the cursor to the home position
clear :: Screen -> Screen
clear scr 
  = let bounds = (Position 1 1, Position cols lines)
        cells = uniformArray bounds (clearcell scr)
    in scr { cells = cells, cursor = home }

-- Move the cursor to the given position
moveto :: Position -> Screen -> Screen
moveto pos scr
 = if (line pos >= 1 && line pos <= (lines scr)
         && column pos >= 1 && column pos <= (columns scr))
    then scr { cursor = pos }
    else scr

-- Move the cursor up n lines
up :: Integer -> Screen -> Screen
up n scr
 = let src = cursor scr
       dst = src { line = (line src) - n }
   in moveto dst

-- Move the cursor down n lines
down :: Integer -> Screen -> Screen
down n scr
 = let src = cursor scr
       dst = src { line = (line src) + n }
   in moveto dst

-- Move the cursor left n columns
left :: Integer -> Screen -> Screen
left n scr
 = let src = cursor scr
       dst = src { column = (column src) - n }
   in moveto dst

-- Move the cursor right n columns
right :: Screen -> Screen
right scr
 = let src = cursor scr
       dst = src { column = (column src) + n }
   in moveto dst

-- Apply the given function to update all the cells of the screen.
modifyby :: (Position -> Cell) -> Screen -> Screen
modifyby f scr
  = let cols = columns scr
        lines = lines scr
        bounds = (Position 1 1, Position cols lines)
        ncells = array bounds [(Position x y, f (Position x y)) | x <- [1..cols], y <- [1..lines]]
    in scr { cells  = ncells }

-- Scroll forward n lines
scroll :: Integer -> Screen -> Screen
scroll n scr
  = let f (Position x y)
          = if (y + n >=1 && y + n <= lines)
            then (cells scr) ! (Position x (y+n))
            else clearcell scr
    in modifyby f scr

-- Insert a line at the given location
insert_line :: Integer -> Screen -> Screen
insert_line line scr
  = let f (Position x y)
          = if (y > 1 && y < line)
              then (cells scr) ! (Position x (y+1))
              else if (y == line)
                then clearcell scr
                else (cells scr) ! (Position x y)
    in modifyby f scr

-- Clear from the cursor to the end of line
clr_eol :: Screen -> Screen
clr_eol scr
  = let pos = cursor scr
        update = [(pos { column = x }, clearcell scr | x <- [(column pos)..(columns scr)]]
    in scr { cells = (cells csr) // update }

-- Set the current bold state
-- True for bold
-- False for normal
setbold :: Bool -> Screen -> Screen
setbold b scr = scr { style = ((style scr) { bold = b }) }

-- Set the current reverse state
-- True for bold
-- False for normal
setreverse :: Bool -> Screen -> Screen
setreverse b scr = scr { style = ((style scr) { reverse = b }) }

-- Set the forground color
setfg :: Color -> Screen -> Screen
setfg c scr = scr { fgcolor = c }

-- Set the background color
setbg :: Color -> Screen -> Screen
setbg c scr = scr { bgcolor = c }

-- Write a character to the screen at the current cursor position.
writechar :: Char -> Screen -> Screen
writechar c scr
  = let old = (cells scr) ! (cursor scr)
        new = old { char = c }
    in scr { cells = (cells scr)//[(cursor scr, new)] }

