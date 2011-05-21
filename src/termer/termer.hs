
import Control.Concurrent
import Control.Monad.State

import qualified CTermer
import Inputter
import Outputter
import Screen

lns = 24
cols = 80

-- Change the contents of the cursor cell to make it apparent
curserify :: Cell -> Cell
curserify (Cell char (Attributes fg bg s)) = Cell char (Attributes BLACK WHITE s)

data TermerState = TermerState {
    m_screen :: Screen,
    m_lastshown :: Screen,
    m_fromclient :: String
};

initialts :: TermerState
initialts = TermerState (screen cols lns) (screen cols lns) ""

-- showdisplay old new
showdisplay :: Screen -> Screen -> IO ()
showdisplay old new = do
    let ocur = cursor old
    let ncur = cursor new
    sequence [CTermer.drawCell p (cellat p new) | p <- diff old new]
    CTermer.drawCell ocur (cellat ocur new)
    CTermer.drawCell ncur (curserify (cellat ncur new))
    CTermer.showDisplay
    

updatef :: (Screen -> Screen) -> StateT TermerState IO ()
updatef f =
    let draw scr pos = CTermer.drawCell pos (cellat pos scr)
    in modify (\s -> s { m_screen = f (m_screen s) })

getf :: StateT TermerState IO Char
getf = do
    fc <- gets m_fromclient
    case fc of
      [] -> do
        -- update the screen now, then ask for more input.
        new <- gets m_screen
        old <- gets m_lastshown
        lift $ showdisplay old new
        modify (\s -> s { m_lastshown = new })

        cs <- lift $ CTermer.fromTermClient
        case cs of
          [] -> return '\0';
          c:cs -> do
            modify (\s -> s { m_fromclient = cs })
            return c
      c:cs -> do
        modify (\s -> s { m_fromclient = cs })
        return c
    

main :: IO ()
main = do
    CTermer.init
    forkIO $ inputter CTermer.getEvent CTermer.toTermClient
    _ <- runStateT (outputter (Just '\0') getf updatef) initialts
    return ()


