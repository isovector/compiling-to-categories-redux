{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module CCC
  ( module CCC
  , module Control.Category
  ) where

import Control.Category
-- import Control.Category
-- import Control.Category.Cartesian.Closed
-- import Control.Category.Cartesian
import Prelude (error)
import qualified Prelude as P

toCCC :: Category k => (a -> b) -> k a b
toCCC = error "we didn't fire!"

class Category k => Cartesian k where
  (&&&) :: k a c -> k a d -> k a (c, d)
  fst :: k (a, b) a
  snd :: k (a, b) b

instance Cartesian (->) where
  (&&&) ac ad = \a -> (ac a, ad a)
  fst (a, _) = a
  snd (_, b) = b

class Cartesian k => Closed k where
  apply :: k (a -> b, a) b
  curry :: k (a, b) c -> k a (b -> c)
  uncurry :: k a (b -> c) -> k (a, b) c

instance Closed (->) where
  apply (f, a) = f a
  curry = P.curry
  uncurry = P.uncurry

class Category k => Terminal k where
  it :: k a ()

instance Terminal (->) where
  it _ = ()

class Terminal k => ConstCat k b where
  unitArrow :: b -> k () b

instance ConstCat (->) b where
  unitArrow b _ = b

const :: ConstCat k b => b -> k a b
const b = unitArrow b . it


class NumCat k a where
  addC :: k (a, a) a

instance P.Num a => NumCat (->) a where
  addC = P.uncurry (P.+)

constFun :: Closed k => k a b -> k z (a -> b)
constFun f = curry (f . snd)

