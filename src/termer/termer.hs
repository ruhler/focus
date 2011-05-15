
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

updatef :: (Screen -> Screen) -> StateT Screen IO ()
updatef f =
    let draw scr pos = CTermer.drawCell pos (cellat pos scr)
    in do
        modify f
        r <- gets recent
        scr <- get
        let pcur = cursor scr
        lift $ do
            mapM_ (\(p, c) -> CTermer.drawCell p c) r
            CTermer.drawCell pcur (curserify (cellat pcur scr))
            CTermer.showDisplay
        modify $ clear_recent
    

main :: IO ()
main = do
    CTermer.init
    forkIO $ inputter CTermer.getEvent CTermer.toTermClient
    _ <- runStateT (outputter (Just '\0') (lift CTermer.fromTermClient) updatef) (screen cols lns)
    return ()


