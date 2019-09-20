{-# LANGUAGE TypeApplications #-}

module Main where

import Test
import Control.Arrow

main :: IO ()
main = print $ runKleisli @Maybe test 5
