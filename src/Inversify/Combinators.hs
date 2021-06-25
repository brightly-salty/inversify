{-# LANGUAGE TemplateHaskell #-}

module Inversify.Combinators where

import Control.Category
import Data.List.NonEmpty (NonEmpty (..))
import Inversify.Classes
import Inversify.Iso
import Inversify.TH
import Prelude hiding (foldl, pure, ($>), (*>), (.), (<$), (<$>), (<*), (<*>))

$(defineIsomorphisms ''Either)

$(defineIsomorphisms ''Maybe)

eitherP :: Syntax p t => p t a -> p t b -> p t (Either a b)
eitherP p q = (left <$> p) <|> (right <$> q)

text :: Syntax p Char => String -> p Char ()
text [] = pure ()
text (c : cs) =
  inverse (element ((), ())) <$> (inverse (element c) <$> token)
    <*> text cs

orEmpty :: (Eq a, Syntax p t) => p t (NonEmpty a) -> p t [a]
orEmpty p = (Iso f g <$> p) <|> pure []
  where
    f (x :| xs) = Just (x : xs)
    g (x : xs) = Just (x :| xs)
    g [] = Nothing

many :: (Eq a, Syntax p t) => p t a -> p t [a]
many p = orEmpty (some p)

some :: (Eq a, Syntax p t) => p t a -> p t (NonEmpty a)
some p = nonEmpty <$> (p <*> many p)

endBy :: (Eq a, Syntax p t) => p t a -> p t () -> p t [a]
endBy p sep = many (p <* sep)

endBy1 :: (Eq a, Syntax p t) => p t a -> p t () -> p t (NonEmpty a)
endBy1 p sep = some (p <* sep)

manyTill :: (Eq a, Syntax p t) => p t a -> p t () -> p t [a]
manyTill p end = go
  where
    go = ([] <$ end) <|> (Iso f g <$> (p <*> go))

    f (x, xs) = Just (x : xs)
    g [] = Nothing
    g (x : xs) = Just (x, xs)

manyTill_ :: (Eq a, Syntax p t) => p t a -> p t end -> p t ([a], end)
manyTill_ p end = go
  where
    go = (Iso f g <$> end) <|> (Iso h i <$> (p <*> go))

    f x = Just ([], x)
    g ([], x) = Just x
    g _ = Nothing

    h (x, (xs, end)) = Just (x : xs, end)
    i (x : xs, end) = Just (x, (xs, end))
    i _ = Nothing

someTill :: (Eq a, Syntax p t) => p t a -> p t () -> p t (NonEmpty a)
someTill p end = nonEmpty <$> (p <*> manyTill p end)

someTill_ :: (Eq a, Syntax p t) => p t a -> p t end -> p t (NonEmpty a, end)
someTill_ p end = Iso f g <$> (p <*> manyTill_ p end)
  where
    f (x, (xs, end)) = Just (x :| xs, end)
    g (x :| xs, end) = Just (x, (xs, end))

sepBy :: (Eq a, Syntax p t) => p t a -> p t () -> p t [a]
sepBy p sep = orEmpty (sepBy1 p sep)

sepBy1 :: (Eq a, Syntax p t) => p t a -> p t () -> p t (NonEmpty a)
sepBy1 p sep = nonEmpty <$> (p <*> many (sep *> p))

sepEndBy :: (Eq a, Syntax p t) => p t a -> p t () -> p t [a]
sepEndBy p sep = orEmpty (sepEndBy1 p sep)

sepEndBy1 :: (Eq a, Syntax p t) => p t a -> p t () -> p t (NonEmpty a)
sepEndBy1 p sep = nonEmpty <$> (p <*> ((sep *> sepEndBy p sep) <|> pure []))

(*>) :: Syntax p t => p t () -> p t a -> p t a
p *> q = inverse unit . commute <$> p <*> q

(<*) :: Syntax p t => p t a -> p t () -> p t a
p <* q = inverse unit <$> p <*> q

($>) :: (Syntax p t, Eq a) => p t () -> a -> p t a
p $> q = p *> pure q

(<$) :: (Syntax p t, Eq a) => a -> p t () -> p t a
p <$ q = pure p <* q

between :: Syntax p t => p t () -> p t () -> p t a -> p t a
between open close p = open *> p <* close

chainl1 :: (Eq a, Eq b, Syntax p t) => p t a -> p t b -> Iso (a, (b, a)) a -> p t a
chainl1 arg op f = foldl f <$> arg <*> many (op <*> arg)

semi :: Syntax p Char => p Char ()
semi = text ";"

comma :: Syntax p Char => p Char ()
comma = text ","

colon :: Syntax p Char => p Char ()
colon = text ":"

space :: Syntax p Char => p Char ()
space = text " "

equals :: Syntax p Char => p Char ()
equals = text "="

lparen :: Syntax p Char => p Char ()
lparen = text "("

rparen :: Syntax p Char => p Char ()
rparen = text ")"

lbrack :: Syntax p Char => p Char ()
lbrack = text "["

rbrack :: Syntax p Char => p Char ()
rbrack = text "]"

lbrace :: Syntax p Char => p Char ()
lbrace = text "{"

rbrace :: Syntax p Char => p Char ()
rbrace = text "}"

skipSpace :: Syntax p Char => p Char ()
skipSpace = ignore [] <$> many space

optSpace :: Syntax p Char => p Char ()
optSpace = ignore [()] <$> many space

sepSpace :: Syntax p Char => p Char ()
sepSpace = space <* skipSpace

parens :: Syntax p Char => p Char a -> p Char a
parens = between lparen rparen

brackets :: Syntax p Char => p Char a -> p Char a
brackets = between lbrack rbrack

braces :: Syntax p Char => p Char a -> p Char a
braces = between lbrace rbrace

quotes :: Syntax p Char => p Char a -> p Char a
quotes = between (text "'") (text "'")

doubleQuotes :: Syntax p Char => p Char a -> p Char a
doubleQuotes = between (text "\"") (text "\"")

choice :: (Syntax p t, Foldable f) => f (p t a) -> p t a
choice = foldr (<|>) empty

optional :: (Eq a, Syntax p t) => p t a -> p t (Maybe a)
optional p = (nothing <$> pure ()) <|> (just <$> p)

option :: (Eq a, Syntax p t) => a -> p t a -> p t a
option x p = p <|> pure x
