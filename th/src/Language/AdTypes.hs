{-# LANGUAGE TemplateHaskell #-}

module Language.AdTypes
       where

import Language.Haskell.TH

data Tree = Leaf | Branch Tree Tree

data TreeD = TreeD1 Tree | TreeD2 Tree

x = [d| data Tree = Leaf | Branch Tree Tree |]

data Maybe' a = Just' (Maybe a) | Nothing'

diffType :: Name -> Q ()
diffType n = do
  TyConI (DataD ctx name binders) <- reify n
  return ()

sel :: Int -> Int -> Q Exp
sel i n = do
  info <- reify ''Maybe'
  loc <- location
  runIO $ print info >> print (loc_end loc)
  [| \x -> $(caseE [| x |] [alt]) |]
  where alt = match pat (normalB rhs) []
        pat = tupP $ map varP as
        rhs = varE $ as !! (i - 1)
        as = [mkName $ "a" ++ show i | i <- [1..n]]
