module CCC where

import Control.Category (Category ())

toCCC :: Category k => (a -> b) -> k a b
toCCC = error "we didn't fire!"

