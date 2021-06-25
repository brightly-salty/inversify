module Inversify.Impl.Parser (Parser (..), parse) where

import Inversify.Classes
import Inversify.Iso

newtype Parser tok a = Parser ([tok] -> [(a, [tok])])

instance IsoFunctor (Parser tok) where
  iso <$> Parser p =
    Parser $
      concatMap
        ( \(x, s') ->
            ( case apply iso x of
                Just y -> [(y, s')]
                Nothing -> []
            )
        )
        . p

instance ProductFunctor (Parser tok) where
  Parser p <*> Parser q =
    Parser $
      concatMap (\(x, s') -> concatMap (\(y, s'') -> [((x, y), s'')]) (q s')) . p

instance Alternative (Parser tok) where
  Parser p <|> Parser q = Parser (\s -> p s <> q s)
  empty = Parser (const [])

instance Syntax Parser tok where
  pure x = Parser (\s -> [(x, s)])
  token = Parser f
    where
      f [] = []
      f (t : ts) = [(t, ts)]

parse :: Parser tok a -> [tok] -> [a]
parse (Parser p) s =
  concatMap
    ( \(x, xs) -> case xs of
        [] -> [x]
        _ -> []
    )
    (p s)
