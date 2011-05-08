
{-# LANGUAGE ForeignFunctionInterface #-}

module CTermer (
    init, deinit, getEvent, toTermClient, fromTermClient, drawCell, showDisplay
    )
  where

import Prelude hiding (init, reverse)
import Consoler
import Screen

-- Initialize CTermer
-- Fails if there was an error.
init :: IO ()
init = do
    x <- ctermer_Init
    case x of
        0 -> return ()
        _ -> fail "CTermer failed to initialize"

-- Deinitialize CTermer
deinit :: IO ()
deinit = ctermer_DeInit

-- Get the next input event
getEvent :: IO Event
getEvent = do
    ctermer_EventGet
    t <- ctermer_EventType
    v <- ctermer_EventValue
    case t of
        0 -> return $ Keypress (toEnum v)
        1 -> return $ Keyrelease (toEnum v)

toTermClient :: Char -> IO ()
toTermClient = ctermer_ToTermClient

fromTermClient :: IO Char
fromTermClient = ctermer_FromTermClient

drawCell :: Position -> Cell -> IO ()
drawCell pos cell =
  let col = fromInteger $ column pos
      row = fromInteger $ line pos
      chr = character cell
      rev = reverse . style . cattrs $ cell
      bld = if bold . style . cattrs $ cell then 1 else 0
      fgc = fromEnum ((if rev then bgcolor else fgcolor) (cattrs cell))
      bgc = fromEnum ((if rev then fgcolor else bgcolor) (cattrs cell))
  in ctermer_DrawCell col row chr bld fgc bgc
      

showDisplay :: IO ()
showDisplay = ctermer_ShowDisplay
        

foreign import ccall "ctermer.h"
    ctermer_Init :: IO Int

foreign import ccall "ctermer.h"
    ctermer_DeInit :: IO ()

foreign import ccall "ctermer.h"
    ctermer_EventGet :: IO ()

foreign import ccall "ctermer.h"
    ctermer_EventType :: IO Int

foreign import ccall "ctermer.h"
    ctermer_EventValue :: IO Int

foreign import ccall "ctermer.h"
    ctermer_ToTermClient :: Char -> IO ()

foreign import ccall "ctermer.h"
    ctermer_FromTermClient :: IO Char

foreign import ccall "ctermer.h"
    ctermer_DrawCell :: Int -> Int -> Char -> Int -> Int -> Int -> IO ()

foreign import ccall "ctermer.h"
    ctermer_ShowDisplay :: IO ()
    

