
import Control.Concurrent
import Control.Monad.State

import qualified CTermer
import Inputter
import Outputter
import Screen

updatef :: (Screen -> Screen) -> StateT Screen IO ()
updatef f =
    let draw scr pos = CTermer.drawCell pos (cellat pos scr)
    in do
        modify f
        scr <- get
        lift $ mapM_ (draw scr) [Position x y | x <- [0..79], y <- [0..23]]
        lift $ CTermer.showDisplay
    

main :: IO ()
main = do
    CTermer.init
    forkIO $ inputter CTermer.getEvent CTermer.toTermClient
    _ <- runStateT (outputter (Just '\ENQ') (lift CTermer.fromTermClient) updatef) (screen 80 24)
    return ()


