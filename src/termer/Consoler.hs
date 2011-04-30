
module Consoler (Event(..), Keysym(..))
  where

import ConsolerKeysym

data Event = Quit | Keypress Keysym | Keyrelease Keysym

