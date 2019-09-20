{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fplugin=Lib  #-}

module Test where

import CCC
import Prelude hiding (id)

test :: (ConstCat k n, Num n, Category k) => k a n
test = toCCC (\i -> 5)

