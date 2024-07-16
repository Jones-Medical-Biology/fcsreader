{-# LANGUAGE FlexibleContexts #-}
module Lib (parseFcs
           ,fcsFile
           ) where

-- import qualified System.Environment as Env
-- import Control.Monad
-- import System.FilePath
import qualified Text.ParserCombinators.Parsec as Comb
import Text.Parsec.Prim ( Stream, ParsecT )
import qualified Data.Text as T
import Data.Functor.Identity ( Identity )

dropLeadingSpaces = Prelude.dropWhile (' ' ==)

-- parse (Comb.count 8 (oneOf " 0123456789") >>= char 'a') "na" "     256"
-- parse (count 8 (many space <|> many Text.ParserCombinators.Parsec.digit)) "na" "     256"
-- dropLeadingSpaces <$> parse (string "FCS3.0" >> (Text.ParserCombinators.Parsec.count 4 space >> (Text.ParserCombinators.Parsec.count 8 (oneOf " 0123456789")))) "na" a
-- parse (choice [(string "FCS3.0" >> (Text.ParserCombinators.Parsec.count 4 space >> (Text.ParserCombinators.Parsec.count 8 (oneOf " 0123456789")))), string "FCS3.1", string "FCS3.2"]) "na" a
fcs30 = do
  versionid <- string "FCS3.0"
  count 4 space
  textstart <- count 8 (oneOf " 0123456789")
  textend <- count 8
  datastart <- count 8
  dataend <- count 8
  analysisstart <- count 8
  analysisend <- count 8
  result <- many segment
  eof
  return result

fcsFile = do
  c <- choice [fcs30
              ,string "FCS3.1"
              ,string "FCS3.2"]
    return c

fcs30 = do
  string "FCS3.0"
  count 4 space
  count 8 (oneOf " 0123456789")
  
fcsFile = endBy body eof
body = endBy line eol
line = sepBy entry (char '\f')
entry = many (noneOf "\f\r\n") 

feedforward :: Stream s m Char => ParsecT s u m Char
feedforward = char '\f'

parseFcs :: String -> Either ParseError [[String]]
parseFcs = parse fcsFile "(unknown)"
-- parseFcs x = case fcsVersion of
--   "FCS3.0" -> parseFcs30
--   "FCS3.1" -> parseFcs31
--   "FCS3.2" -> parseFcs30
--   _ -> Error


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

