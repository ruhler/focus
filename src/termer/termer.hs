
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

updatef :: (Screen -> Screen) -> StateT TermerState IO ()
updatef f =
    let draw scr pos = CTermer.drawCell pos (cellat pos scr)
    in do
        modify (\s -> s { m_screen = f (m_screen s) })
        r <- gets $ recent . m_screen
        scr <- gets m_screen
        let pcur = cursor scr
        lift $ do
            mapM_ (\(p, c) -> CTermer.drawCell p c) r
            CTermer.drawCell pcur (curserify (cellat pcur scr))
            CTermer.showDisplay
        modify (\s -> s { m_screen = clear_recent (m_screen s) })

getf :: StateT TermerState IO Char
getf = do
    fc <- gets m_fromclient
    case fc of
      [] -> do
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


