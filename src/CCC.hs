{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module CCC
  ( module CCC
  , module Control.Category
  , module Control.Category.Cartesian.Closed
  , module Control.Category.Cartesian
  ) where

import Control.Category (Category ())
import Control.Category
import Control.Category.Cartesian.Closed
import Control.Category.Cartesian
import Prelude (error)

toCCC :: Category k => (a -> b) -> k a b
toCCC = error "we didn't fire!"

class Category k => Terminal k where
  type Term k :: *
  it :: k a (Term k)

instance Terminal (->) where
  type Term (->) = ()
  it _ = ()

class Terminal k => ConstCat k b where
  unitArrow :: b -> k (Term k) b

instance ConstCat (->) b where
  unitArrow b _ = b

const :: ConstCat k b => b -> k a b
const b = unitArrow b . it

