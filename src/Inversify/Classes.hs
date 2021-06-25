module Inversify.Classes (IsoFunctor (..), ProductFunctor (..), Alternative (..), Syntax (..)) where

import Inversify.Iso
import Prelude hiding (pure, (<$>), (<*>))

class IsoFunctor f where
  (<$>) :: Iso a b -> f a -> f b

infix 5 <$>

class ProductFunctor f where
  (<*>) :: f a -> f b -> f (a, b)

infixr 6 <*>

class Alternative f where
  (<|>) :: f a -> f a -> f a
  empty :: f a

infixl 3 <|>

class (IsoFunctor d, ProductFunctor d, Alternative d) => Syntax d where
  pure :: Eq a => a -> d a
  token :: d Char
