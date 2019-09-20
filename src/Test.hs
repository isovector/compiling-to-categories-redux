{-# OPTIONS_GHC -fplugin=Lib #-}

module Test where

import CCC
import Control.Category

test :: Category k => k Int Int
test = toCCC (\i -> i)

