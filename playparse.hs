{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad (void)
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word8 (isDigit)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString as B
import Data.Word (Word8)
import Data.Word8 (isSpace)

skipLine :: Parser ()
skipLine = skipWhile (not . isEOL) >> skipSpace

skipSpace :: Parser ()
skipSpace = skipWhile isSpace

skipEq :: Parser ()
skipEq = skipSpace >> skip isEq >> skipSpace

isEOL :: Word8 -> Bool
isEOL x = x == 10 || x == 13

isEq :: Word8 -> Bool
isEq = (== 61)

skipUnusedLine :: Parser ()
skipUnusedLine =
  (string "numberofentries" <|>
  string "NumberOfEntries" <|>
  string "version" <|>
  string "Version") >> skipLine

logFile :: FilePath
logFile = "play.pls"



data Format = PLS               -- ^ <http://en.wikipedia.org/wiki/PLS_(file_format)>
            | M3U               -- ^ M3U and M3U8. <http://en.wikipedia.org/wiki/M3U>
              deriving (Read, Show, Eq)

data Track = Track
    { trackURL   :: Text       -- ^ URL for a file or streaming resource.
    , trackTitle :: Maybe Text -- ^ Optional title.
    } deriving (Show, Eq)

type Playlist = [Track]

parseUrl :: Parser Text
parseUrl = decodeUtf8 <$> takeWhile1 (not . isEOL) <* skipSpace

parseFileN :: Parser (ByteString, Text)
parseFileN = do
  skipSpace
  n <- string "File" >> takeWhile1 isDigit
  skipEq
  url <- takeWhile1 (not . isEOL)
  return (n, decodeUtf8 url)

parseTitle :: ByteString -> Parser Text
parseTitle n = do
  skipSpace
  void (string "Title" >> string n)
  skipEq
  decodeUtf8 <$> takeWhile1 (not . isEOL)

parseHeader :: Parser ()
parseHeader = do
  skipSpace >> string "[playlist]" >> skipSpace
  skipMany skipUnusedLine

parseTrack :: Parser Track
parseTrack = do
  (n, url) <- parseFileN
  title    <- (Just <$> parseTitle n) <|> return Nothing

  (skipSpace >> string "Length" >> skipLine) <|> return ()

  return Track { trackURL = url
               , trackTitle = title
               }

parsePlayList :: Parser Playlist
parsePlayList = do
  parseHeader
  ts <- many1 parseTrack
  skipMany skipUnusedLine
  return ts




main :: IO ()
main = B.readFile logFile >>= print . parseOnly parsePlayList
