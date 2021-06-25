module Inversify.Impl.Parser (Parser (..), parse) where

import Inversify.Classes
import Inversify.Iso

newtype Parser a = Parser ([Char] -> [(a, [Char])])

instance IsoFunctor Parser where
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

instance ProductFunctor Parser where
  Parser p <*> Parser q =
    Parser $
      concatMap (\(x, s') -> concatMap (\(y, s'') -> [((x, y), s'')]) (q s')) . p

instance Alternative Parser where
  Parser p <|> Parser q = Parser (\s -> p s <> q s)
  empty = Parser (const [])

instance Syntax Parser where
  pure x = Parser (\s -> [(x, s)])
  token = Parser f
    where
      f [] = []
      f (t : ts) = [(t, ts)]

parse :: Parser a -> String -> [a]
parse (Parser p) s =
  concatMap
    ( \(x, xs) -> case xs of
        [] -> [x]
        _ -> []
    )
    (p s)
