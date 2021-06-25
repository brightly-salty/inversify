module Inversify.Iso where

import Control.Category
import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import Prelude hiding (id, iterate, (.))

data Iso a b = Iso (a -> Maybe b) (b -> Maybe a)

instance Category Iso where
  g . f = Iso (apply f >=> apply g) (unapply g >=> unapply f)
  id = Iso Just Just

nil :: Iso () [a]
nil = Iso (const (Just [])) (\xs -> case xs of [] -> Just (); (x : xs) -> Nothing)

cons :: Iso (a, [a]) [a]
cons = Iso (\(x, xs) -> Just (x : xs)) (\xs -> case xs of [] -> Nothing; (x : xs) -> Just (x, xs))

inverse :: Iso a b -> Iso b a
inverse (Iso f g) = Iso g f

apply :: Iso a b -> a -> Maybe b
apply (Iso f g) = f

unapply :: Iso a b -> b -> Maybe a
unapply = apply . inverse

(***) :: Iso a b -> Iso c d -> Iso (a, c) (b, d)
i *** j = Iso f g
  where
    f (a, b) = liftM2 (,) (apply i a) (apply j b)
    g (c, d) = liftM2 (,) (unapply i c) (unapply j d)

associate :: Iso (a, (b, c)) ((a, b), c)
associate = Iso f g
  where
    f (a, (b, c)) = Just ((a, b), c)
    g ((a, b), c) = Just (a, (b, c))

commute :: Iso (a, b) (b, a)
commute = Iso f f
  where
    f (a, b) = Just (b, a)

unit :: Iso a (a, ())
unit = Iso (Just . (,())) (Just . fst)

element :: Eq a => a -> Iso () a
element x = Iso (const (Just x)) (\b -> if x == b then Just () else Nothing)

subset :: (a -> Bool) -> Iso a a
subset p = Iso f f
  where
    f x
      | p x = Just x
      | otherwise = Nothing

driver :: (a -> Maybe a) -> a -> a
driver step state = case step state of
  Just state' -> driver step state'
  Nothing -> state

iterate :: Iso a a -> Iso a a
iterate step = Iso f g
  where
    f = Just . driver (apply step)
    g = Just . driver (unapply step)

step i = (i *** id) . associate . (id *** inverse cons)

foldl :: Iso (a, b) a -> Iso (a, [b]) a
foldl i = inverse unit . (id *** inverse nil) . iterate (step i)

ignore :: a -> Iso a ()
ignore x = Iso f g
  where
    f _ = Just ()
    g () = Just x

nonEmpty :: Iso (a, [a]) (NonEmpty a)
nonEmpty = Iso f g
  where
    f (x, xs) = Just (x :| xs)
    g (x :| xs) = Just (x, xs)
