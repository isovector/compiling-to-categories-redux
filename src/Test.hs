{-# OPTIONS_GHC -fplugin=Lib #-}

module Test where

import CCC

foo :: Int -> Bool
foo = toCCC (\i -> i == 0)

