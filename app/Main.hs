--{-# LANGUAGE FlexibleContexts #-}
import Lib
import Text.Parsec.Prim
import GHC.IO.Encoding
-- import Text.ParserCombinators.Parsec
import System.Environment (getArgs)

main :: IO ()
main = do
  setLocaleEncoding char8
  c <- getContents --handleArgs
  case parse fcsFile "(stdin)" c of
    Left e -> do putStrLn "Error parsing input:"
                 print e
    Right r -> mapM_ print r

-- handleArgs :: IO (Either String (IO String))
-- handleArgs =
--   parseArgs <$> getArgs
--   where
--     parseArgs argumentList =
--       case argumentList of
--         [] -> Left "Error: No arguments provided!"
--         (arg:args) -> Right (readFile arg)
