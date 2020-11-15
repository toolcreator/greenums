{-
  CSV parsing: https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/
-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Main where


-- base
import Control.Exception (IOException, catch)
import System.Environment (getArgs)

-- bytestring)
import qualified Data.ByteString.Lazy  as B
import qualified Data.ByteString.Lazy.Char8 as C

-- stringsearch
import qualified Data.ByteString.Lazy.Search as S

-- vector
import Data.Vector (Vector)

-- cassava
import Data.Csv

-- time
import qualified Data.Time as T

-- word8
import Data.Word8 (_semicolon)

-- containers
import qualified Data.HashMap.Strict as HM


data Transaction = Transaction {
  day :: T.Day,
  amount :: Float
} deriving (Show, Eq)
instance FromNamedRecord Transaction where
  parseNamedRecord m = Transaction
                        <$> m .: "Buchungstag"
                        <*> parseAmount m where
                          parseAmount :: NamedRecord -> Parser Float
                          parseAmount m = maybe zero (parseGerFloat . B.fromStrict) amountStr where
                            zero = return 0
                            amountStr = HM.lookup "Betrag" m
                            parseGerFloat :: B.ByteString -> Parser Float
                            parseGerFloat str = return (
                                read $ C.unpack $ S.replace "," ("." :: B.ByteString) str :: Float
                              )

instance FromField T.Day where
  parseField = T.parseTimeM True T.defaultTimeLocale "%d.%m.%Y" . C.unpack . B.fromStrict

decodeTransactions :: B.ByteString -> Either String (Vector Transaction)
decodeTransactions = fmap snd . decodeByNameWith (DecodeOptions _semicolon)

decodeTransactionsFromFile :: FilePath -> IO (Either String (Vector Transaction))
decodeTransactionsFromFile filePath = catchShowIO (C.readFile filePath)
                                      >>= return . either Left decodeTransactions

catchShowIO :: IO a -> IO (Either String a)
catchShowIO action = fmap Right action `catch` handleIOException where
                      handleIOException :: IOException -> IO (Either String a)
                      handleIOException = return . Left . show

main :: IO ()
main = do [filename] <- getArgs
          records <- decodeTransactionsFromFile filename
          print records

