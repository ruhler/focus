
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
    m_fromclient :: String
};

initialts :: TermerState
initialts = TermerState (screen cols lns) ""

showdisplay :: Screen -> IO ()
showdisplay scr = do
    let pcur = cursor scr
    mapM_ (\(p, c) -> CTermer.drawCell p c) (recent scr)
    CTermer.drawCell pcur (curserify (cellat pcur scr))
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
        scr <- gets m_screen
        lift $ showdisplay scr
        modify (\s -> s { m_screen = clear_recent (m_screen s) })

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


