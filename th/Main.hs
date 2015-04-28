{-# LANGUAGE TemplateHaskell #-}

module Main 
       where

import Language.AdTypes

main :: IO ()
main = do
  putStrLn "foo"
  let t = (1, 2, 3)
  print $ $(sel 1 3) (1, 2, 3)
