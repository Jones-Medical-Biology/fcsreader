{-# LANGUAGE FlexibleContexts #-}
module MyLib (someFunc
             ,runHCat
             ,fcsFile) where

import qualified System.Environment as Env
import Control.Monad
import System.FilePath
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Data.Text

import Fcs30

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- parseFcsFile :: Stream s m Char => ParsecT s u m [[Char]] -> Either ParseError [[Char]]
-- parseFcsFile = parse fcsFile "(stdin)"

fcsFile :: Stream s m Char => ParsecT s u m [[Char]]
fcsFile = endBy (many alphaNum) spaces

-- parseFcs :: FcsFile -> Either ParseError (GenParser Char st [String]) -- not sure about signature
-- parseFcs x = case fcsVersion of
--   "FCS3.0" -> parseFcs30
--   "FCS3.1" -> parseFcs31
--   "FCS3.2" -> parseFcs30
--   _ -> Error

runHCat :: IO ()
runHCat = handleArgs >>= displayMessage
  where
   displayMessage parsedArgument =
    case parsedArgument of
     Left errMessage ->
      putStrLn $ "Error: " <> errMessage
     Right filename ->
      readFile filename >>= putStrLn

handleArgs :: IO (Either String FilePath)
handleArgs =
  parseArgs <$> Env.getArgs
  where
   parseArgs argumentList =
    case argumentList of
     [fname] -> Right fname
     [] -> Left "Error: No arguments provided!"
     _ -> Left "garbage detected"

data FcsData30 = FcsData30 { -- punning
  beginstext :: Int
  , endstext :: Int
  , begindata :: Int
  , enddata :: Int
  , beginanalysis :: Int
  , endanalysis :: Int
  , tot :: Int
  , filver :: Int
  , fil :: Text
  , sys :: Text
  , mode :: Char
  , byteord :: [Int]
  , datatype :: Char
  , nextdata :: Int
  , cytexpertfil :: Bool
  , tbid :: Text -- should be UUID
  , tbnm :: Text
  , par :: Int
  , btim :: Text -- should be time
  , etim :: Text -- should be time
  , date :: Text -- should be date
  , cyt :: Text
  , rctot :: Int
  , usrctot :: Bool
  , cgnm :: Text
  , rctim :: Int
  , usrctim :: Bool
  , spillover :: Text -- should be complex table data structure
  , rcvol :: Int
  , usrcvol :: Bool
   -- need to figure out how to add an arbitrary number of detectors with p#n, p#s, p#r, p#b, p#e, p#g
  }
{-
parse x:xs |
  x == "^L" = parse xs
-}

--headerParser :: a -> b

