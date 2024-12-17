{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}
module Lib (parseFcs
           ,fcsFile
           ) where

-- import qualified System.Environment as Env
-- import Control.Monad
-- import System.FilePath
import Text.ParserCombinators.Parsec
  ( ParseError,
    parse,
    many,
    anyChar,
    char,
    oneOf,
    space,
    string,
    choice,
    count,
    eof )
import Text.Parsec.Prim
  ( Stream, ParsecT )
import qualified Data.Text as T ( Text )
import Data.Functor.Identity ( Identity )

-- parse (Comb.count 8 (oneOf " 0123456789") >>= char 'a') "na" "     256"
-- parse (count 8 (many space <|> many Text.ParserCombinators.Parsec.digit)) "na" "     256"
-- dropLeadingSpaces <$> parse (string "FCS3.0" >> (Text.ParserCombinators.Parsec.count 4 space >> (Text.ParserCombinators.Parsec.count 8 (oneOf " 0123456789")))) "na" a
-- parse (choice [(string "FCS3.0" >> (Text.ParserCombinators.Parsec.count 4 space >> (Text.ParserCombinators.Parsec.count 8 (oneOf " 0123456789")))), string "FCS3.1", string "FCS3.2"]) "na" a

dropLeadingSpaces = Prelude.dropWhile (' ' ==)
toInt x = read x :: Int
byteIndexParse x = dropLeadingSpaces <$> count x (oneOf " 0123456789")

fcs30 = do
  versionid <- string "FCS3.0"
  count 4 space
  textstart <- byteIndexParse 8
  textend <- byteIndexParse 8
  datastart <- byteIndexParse 8
  dataend <- byteIndexParse 8
  analysisstart <- byteIndexParse 8
  analysisend <- byteIndexParse 8
  -- junk <- byteIndexParse (textstart - 58) textstart needs to become an integer
  -- textsegment <- byteIndexParse (textend - textstart) textend needs to become an integer
  -- result <- many segment
  many anyChar
  eof
  return FcsOut { versionid = Just (versionid :: [Char] )
                , textstart = Just textstart
                , textend = Just textend
                -- , datastart = Just datastart
                -- , dataend = Just dataend
                -- , analysisstart = Just analysisstart
                -- , analysisend = Just analysisend
                }

fcs31 = do
  versionid <- string "FCS3.1"
  many anyChar
  eof
  return FcsOut { versionid = Just (versionid :: [Char] )
                , textstart = Nothing
                , textend = Nothing }

fcs32 = do
  versionid <- string "FCS3.2"
  many anyChar
  eof
  return FcsOut { versionid = Just (versionid :: [Char] )
                , textstart = Nothing
                , textend = Nothing }


fcsFile = do choice [ fcs30
                    , fcs31
                    , fcs32
                    ]

-- fcs30 = do
--   string "FCS3.0"
--   count 4 space
--   count 8 (oneOf " 0123456789")
  
-- fcsFile = endBy body eof
-- body = endBy line eol
-- line = sepBy entry (char '\f')
-- entry = many (noneOf "\f\r\n") 

feedforward :: Stream s m Char => ParsecT s u m Char
feedforward = char '\f'

data FcsOut = FcsOut { versionid  :: Maybe [Char]
                     , textstart  :: Maybe [Char]
                     , textend    :: Maybe [Char]
                     } deriving Show

parseFcs :: String -> Either ParseError FcsOut
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

