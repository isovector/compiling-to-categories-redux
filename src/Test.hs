{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fplugin=Lib  #-}

module Test where

import CCC
import Prelude hiding (id, (.), const, curry)


foo :: (Closed k, NumCat k c, ConstCat k c, Num c) => k c c
foo = toCCC (\x -> (+) x 2)

-- sqr :: (Closed k, NumCat k a) => k a a
-- -- sqr = toCCC (\a -> (+) a a)

-- sqr = apply . (curry addC &&& id)

-- (.) apply (&&&) (.) apply (&&&) constFun addC id id

-- -- test :: Integer -> Integer
-- -- test = (.) apply ((&&&) ((.) apply ((&&&)) (constFun addC) id) (const 6))
-- -- -- test = toCCC (\i -> (+) i 6)

