
import Control.Concurrent
import Control.Monad.State

import qualified CTermer
import Inputter
import Outputter
import Screen

lns = 24
cols = 80


updatef :: (Screen -> Screen) -> StateT Screen IO ()
updatef f =
    let draw scr pos = CTermer.drawCell pos (cellat pos scr)
    in do
        modify f
        r <- gets recent
        lift $ mapM_ (\(p, c) -> CTermer.drawCell p c) r
        lift $ CTermer.showDisplay
        modify $ clear_recent
    

main :: IO ()
main = do
    CTermer.init
    forkIO $ inputter CTermer.getEvent CTermer.toTermClient
    _ <- runStateT (outputter (Just '\0') (lift CTermer.fromTermClient) updatef) (screen cols lns)
    return ()


