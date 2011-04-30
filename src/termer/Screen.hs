
module Screen (
    Color(..), Style(..), Cell(..), Position(..), Screen(),
    screen, clear
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

defaultcursor = Position 1 1

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
    in Screen cols lines defaultcursor cells WHITE BLACK normal

-- clear the screen
clear :: Screen -> Screen
clear scr 
  = let bounds = (Position 1 1, Position cols lines)
        cell = Cell ' ' (fgcolor scr) (bgcolor scr) (style csr)
        cells = uniformArray bounds cell
    in scr { cells = cells }

