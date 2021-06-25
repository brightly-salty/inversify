{-# LANGUAGE TemplateHaskell #-}

module Inversify.TH (defineIsomorphisms) where

import Control.Monad
import Data.Char (toLower)
import Data.List (find)
import Inversify.Iso hiding (foldl)
import Language.Haskell.TH

constructorIso c = do
  DataConI n _ d <- reify c
  TyConI ((DataD _ _ _ _ cs _)) <- reify d
  let Just con = find (\(NormalC n' _) -> n == n') cs
  isoFromCon con

defineIsomorphisms d = do
  TyConI (DataD _ _ _ _ cs _) <- reify d
  let rename n = mkName (toLower c : cs) where c : cs = nameBase n
      defFromCon con@(NormalC n _) = funD (rename n) [clause [] (normalB (isoFromCon con)) []]
  mapM defFromCon cs

isoFromCon (NormalC c fs) = do
  let n = length fs
  (ps, vs) <- genPE n
  v <- newName "x"
  let f = lamE [nested tupP ps] [|Just $(foldl appE (conE c) vs)|]
  let g = lamE [varP v] (caseE (varE v) [match (conP c ps) (normalB [|Just $(nested tupE vs)|]) [], match wildP (normalB [|Nothing|]) []])
  [|Iso $f $g|]

genPE n = do
  ids <- replicateM n (newName "x")
  pure (map varP ids, map varE ids)

nested tup [] = tup []
nested tup [x] = x
nested tup (x : xs) = tup [x, nested tup xs]
