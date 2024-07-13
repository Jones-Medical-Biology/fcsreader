{-# LANGUAGE FlexibleContexts #-}
import MyLib
import Fcs30
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec
import System.Environment (getArgs)

main :: IO ()
main = do
  c <- getContents
  case parse fcsFile "(stdin)" c of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> mapM_ print r
