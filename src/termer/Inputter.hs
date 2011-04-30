
module Inputter (inputter)
  where

import Consoler

data InputterState = InputterState {
    shifton :: Bool,
    ctrlon :: Bool
}

initial :: InputterState
initial = InputterState False False

setshift :: (MonadState InputterState sm) => Bool -> sm ()
setshift x = modify $ \s -> s { shifton = x }

setctrl :: (MonadState InputterState sm) => Bool -> sm ()
setctrl x = modify $ \s -> s { ctrlon = x }

inputter :: (Monad m) => m Event -> (Char -> m ()) -> m ()
inputter get put = fst $ runStateT (sinputter get put) initial
    
-- sinputter: Inputter with modifier key state
sinputter :: (Monad m) => m Event -> (Char -> m()) -> StateT InputterState m ()
sinputter iget iput = do
    event <- iget
    shift = gets shifton
    ctrl = gets ctrlon
    case event of
        Quit -> return ()
        Keypress x -> press (lift iput) x shift ctrl >> sinputter iget iput
        Keyrelease x -> release x >> sinputter iget iput

-- press: handle a single input keypress event
-- press iput sym shifton ctrlon
press :: (MonadState InputterState sm) => (Char -> sm ()) -> Keysym -> Bool -> Bool -> sm ()

press _ LSHIFT _ _ = setshift True
press _ RSHIFT _ _ = setshift True
press _ LCTRL _ _ = setctrl True
press _ RCTRL _ _ = setctrl True

press iput BACKSPACE _ _ = iput '\BS'
press iput TAB _ _ = iput '\HT'
press iput RETURN _ _ = iput '\CR'
press iput ESCAPE _ _ = iput '\ESC'
press iput SPACE _ True = iput '\NUL'
press iput SPACE _ False = iput ' '
press iput D0 False _ = iput '0'
press iput D0 True _ = iput ')'
press iput D1 False _ = iput '1'
press iput D1 True _ = iput '!'
press iput D2 False _ = iput '2'
press iput D2 True _ = iput '@'
press iput D3 False _ = iput '3'
press iput D3 True _ = iput '#'
press iput D4 False _ = iput '4'
press iput D4 True _ = iput '$'
press iput D5 False _ = iput '5'
press iput D5 True _ = iput '%'
press iput D6 False _ = iput '6'
press iput D6 True _ = iput '^'
press iput D7 False _ = iput '7'
press iput D7 True _ = iput '&'
press iput D8 False _ = iput '8'
press iput D8 True _ = iput '*'
press iput D9 False _ = iput '9'
press iput D9 True _ = iput '('

press iput A _ True = iput '\SOH'
press iput A False False = iput 'a'
press iput A True False = iput 'A'

press iput B _ True = iput '\STX'
press iput B False False = iput 'b'
press iput B True False = iput 'B'

press iput C _ True = iput '\ETX'
press iput C False False = iput 'c'
press iput C True False = iput 'C'

press iput D _ True = iput '\EOT'
press iput D False False = iput 'd'
press iput D True False = iput 'D'

press iput E _ True = iput '\ENQ'
press iput E False False = iput 'e'
press iput E True False = iput 'E'

press iput F _ True = iput '\ACK'
press iput F False False = iput 'f'
press iput F True False = iput 'F'

press iput G _ True = iput '\BEL'
press iput G False False = iput 'g'
press iput G True False = iput 'G'

press iput H _ True = iput '\BS'
press iput H False False = iput 'h'
press iput H True False = iput 'H'

press iput I _ True = iput '\HT'
press iput I False False = iput 'i'
press iput I True False = iput 'I'

press iput J _ True = iput '\LF'
press iput J False False = iput 'j'
press iput J True False = iput 'J'

press iput K _ True = iput '\VT'
press iput K False False = iput 'k'
press iput K True False = iput 'K'

press iput L _ True = iput '\FF'
press iput L False False = iput 'l'
press iput L True False = iput 'L'

press iput M _ True = iput '\CR'
press iput M False False = iput 'm'
press iput M True False = iput 'M'

press iput N _ True = iput '\SO'
press iput N False False = iput 'n'
press iput N True False = iput 'N'

press iput O _ True = iput '\SI'
press iput O False False = iput 'o'
press iput O True False = iput 'O'

press iput P _ True = iput '\DLE'
press iput P False False = iput 'p'
press iput P True False = iput 'P'

press iput Q _ True = iput '\DC1'
press iput Q False False = iput 'q'
press iput Q True False = iput 'Q'

press iput R _ True = iput '\DC2'
press iput R False False = iput 'r'
press iput R True False = iput 'R'

press iput S _ True = iput '\DC3'
press iput S False False = iput 's'
press iput S True False = iput 'S'

press iput T _ True = iput '\DC4'
press iput T False False = iput 't'
press iput T True False = iput 'T'

press iput U _ True = iput '\NAK'
press iput U False False = iput 'u'
press iput U True False = iput 'U'

press iput V _ True = iput '\SYN'
press iput V False False = iput 'v'
press iput V True False = iput 'V'

press iput W _ True = iput '\ETB'
press iput W False False = iput 'w'
press iput W True False = iput 'W'

press iput X _ True = iput '\CAN'
press iput X False False = iput 'x'
press iput X True False = iput 'X'

press iput Y _ True = iput '\EM'
press iput Y False False = iput 'y'
press iput Y True False = iput 'Y'

press iput Z _ True = iput '\SUB'
press iput Z False False = iput 'z'
press iput Z True False = iput 'Z'

press iput LEFTBRACKET _ True = iput '\ESC'
press iput LEFTBRACKET False False = iput '['
press iput LEFTBRACKET True False = iput '{'

press iput RIGHTBRACKET _ True = iput '\GS'
press iput RIGHTBRACKET False False = iput ']'
press iput RIGHTBRACKET True False = iput '}' 

press iput BACKSLASH _ True = iput '\FS'
press iput BACKSLASH False False = iput '\\'
press iput BACKSLASH True False = iput '|'

press iput BACKQUOTE _ True = iput '\RS'
press iput BACKQUOTE False False = iput '`'
press iput BACKQUOTE True False = iput '~'

press iput SLASH _ True = iput '\US'
press iput SLASH False False = iput '/'
press iput SLASH True False = iput '?'

press iput UNDERSCORE False _ = iput '-'
press iput UNDERSCORE True _ = iput '_'

press iput EQUALS False _ = iput '='
press iput EQUALS True _ = iput '+'

press iput COMMA False _ = iput ','
press iput COMMA True _ = iput '<'

press iput PERIOD False _ = iput '.'
press iput PERIOD True _ = iput '>'

press iput QUOTE False _ = iput '\''
press iput QUOTE True _ = iput '"'

press iput UP _ _ = mapM_ iput "\ESCA"
press iput DOWN _ _ = mapM_ iput "\ESCB"
press iput LEFT _ _ = mapM_ iput "\ESCC"
press iput RIGHT _ _ = mapM_ iput "\ESCD"
press iput HOME _ _ = mapM_ iput "\ESCH"
press iput END _ _ = mapM_ iput "\ESC4~"

press iput DELETE _ _ = iput '\DEL'
press _ x s c = error $ "unhandled input keysym " ++ show x

-- release: handle a single input keyrelease event
-- release sym
--
-- release is only for keeping track of modifiers. We never need to send the
-- client anything.
release :: (MonadState InputterState sm) => Keysym -> sm ()
release RSHIFT = setshift false
release LSHIFT = setshift false
release RCTRL = setctrl false
release LCTRL = setctrl false
release _ = return ()

