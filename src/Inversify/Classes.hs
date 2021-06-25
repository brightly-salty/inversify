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

class (IsoFunctor (p t), ProductFunctor (p t), Alternative (p t)) => Syntax p t where
  pure :: Eq a => a -> p t a
  token :: p t t
