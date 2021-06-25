module Inversify.Impl.Printer (Printer (..)) where

import Control.Monad
import Inversify.Classes
import Inversify.Iso
import Prelude hiding (print)

newtype Printer a = Printer {print :: a -> Maybe String}

instance IsoFunctor Printer where
  iso <$> Printer p = Printer (unapply iso >=> p)

instance ProductFunctor Printer where
  Printer p <*> Printer q =
    Printer (\(x, y) -> liftM2 (<>) (p x) (q y))

instance Alternative Printer where
  Printer p <|> Printer q =
    Printer (\s -> mplus (p s) (q s))
  empty = Printer (const Nothing)

instance Syntax Printer where
  pure x = Printer (\y -> if x == y then Just "" else Nothing)
  token = Printer (\t -> Just [t])
