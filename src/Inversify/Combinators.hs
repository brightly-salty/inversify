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

eitherP :: Syntax d => d a -> d b -> d (Either a b)
eitherP p q = (left <$> p) <|> (right <$> q)

text :: Syntax d => String -> d ()
text [] = pure ()
text (c : cs) =
  inverse (element ((), ())) <$> (inverse (element c) <$> token)
    <*> text cs

orEmpty :: (Eq a, Syntax d) => d (NonEmpty a) -> d [a]
orEmpty p = (Iso f g <$> p) <|> pure []
  where
    f (x :| xs) = Just (x : xs)
    g (x : xs) = Just (x :| xs)
    g [] = Nothing

many :: (Eq a, Syntax d) => d a -> d [a]
many p = orEmpty (some p)

some :: (Eq a, Syntax d) => d a -> d (NonEmpty a)
some p = nonEmpty <$> (p <*> many p)

endBy :: (Eq a, Syntax d) => d a -> d () -> d [a]
endBy p sep = many (p <* sep)

endBy1 :: (Eq a, Syntax d) => d a -> d () -> d (NonEmpty a)
endBy1 p sep = some (p <* sep)

manyTill :: (Eq a, Syntax d) => d a -> d () -> d [a]
manyTill p end = go
  where
    go = ([] <$ end) <|> (Iso f g <$> (p <*> go))

    f (x, xs) = Just (x : xs)
    g [] = Nothing
    g (x : xs) = Just (x, xs)

manyTill_ :: (Eq a, Syntax d) => d a -> d end -> d ([a], end)
manyTill_ p end = go
  where
    go = (Iso f g <$> end) <|> (Iso h i <$> (p <*> go))

    f x = Just ([], x)
    g ([], x) = Just x
    g _ = Nothing

    h (x, (xs, end)) = Just (x : xs, end)
    i (x : xs, end) = Just (x, (xs, end))
    i _ = Nothing

someTill :: (Eq a, Syntax d) => d a -> d () -> d (NonEmpty a)
someTill p end = nonEmpty <$> (p <*> manyTill p end)

someTill_ :: (Eq a, Syntax d) => d a -> d end -> d (NonEmpty a, end)
someTill_ p end = Iso f g <$> (p <*> manyTill_ p end)
  where
    f (x, (xs, end)) = Just (x :| xs, end)
    g (x :| xs, end) = Just (x, (xs, end))

sepBy :: (Eq a, Syntax d) => d a -> d () -> d [a]
sepBy p sep = orEmpty (sepBy1 p sep)

sepBy1 :: (Eq a, Syntax d) => d a -> d () -> d (NonEmpty a)
sepBy1 p sep = nonEmpty <$> (p <*> many (sep *> p))

sepEndBy :: (Eq a, Syntax d) => d a -> d () -> d [a]
sepEndBy p sep = orEmpty (sepEndBy1 p sep)

sepEndBy1 :: (Eq a, Syntax d) => d a -> d () -> d (NonEmpty a)
sepEndBy1 p sep = nonEmpty <$> (p <*> ((sep *> sepEndBy p sep) <|> pure []))

(*>) :: Syntax d => d () -> d a -> d a
p *> q = inverse unit . commute <$> p <*> q

(<*) :: Syntax d => d a -> d () -> d a
p <* q = inverse unit <$> p <*> q

($>) :: (Syntax d, Eq a) => d () -> a -> d a
p $> q = p *> pure q

(<$) :: (Syntax d, Eq a) => a -> d () -> d a
p <$ q = pure p <* q

between :: Syntax d => d () -> d () -> d a -> d a
between open close p = open *> p <* close

chainl1 :: (Eq a, Eq b, Syntax d) => d a -> d b -> Iso (a, (b, a)) a -> d a
chainl1 arg op f = foldl f <$> arg <*> many (op <*> arg)

semi :: Syntax d => d ()
semi = text ";"

comma :: Syntax d => d ()
comma = text ","

colon :: Syntax d => d ()
colon = text ":"

space :: Syntax d => d ()
space = text " "

equals :: Syntax d => d ()
equals = text "="

lparen :: Syntax d => d ()
lparen = text "("

rparen :: Syntax d => d ()
rparen = text ")"

lbrack :: Syntax d => d ()
lbrack = text "["

rbrack :: Syntax d => d ()
rbrack = text "]"

lbrace :: Syntax d => d ()
lbrace = text "{"

rbrace :: Syntax d => d ()
rbrace = text "}"

skipSpace :: Syntax d => d ()
skipSpace = ignore [] <$> many space

optSpace :: Syntax d => d ()
optSpace = ignore [()] <$> many space

sepSpace :: Syntax d => d ()
sepSpace = space <* skipSpace

parens :: Syntax d => d a -> d a
parens = between lparen rparen

brackets :: Syntax d => d a -> d a
brackets = between lbrack rbrack

braces :: Syntax d => d a -> d a
braces = between lbrace rbrace

quotes :: Syntax d => d a -> d a
quotes = between (text "'") (text "'")

doubleQuotes :: Syntax d => d a -> d a
doubleQuotes = between (text "\"") (text "\"")

choice :: (Syntax d, Foldable f) => f (d a) -> d a
choice = foldr (<|>) empty

optional :: (Eq a, Syntax d) => d a -> d (Maybe a)
optional p = (nothing <$> pure ()) <|> (just <$> p)

option :: (Eq a, Syntax d) => a -> d a -> d a
option x p = p <|> pure x
