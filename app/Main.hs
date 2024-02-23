module Main where

import qualified MyLib (someFunc, runHCat)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.runHCat
