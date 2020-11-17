{-
  CSV parsing: https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/
-}


{-# LANGUAGE OverloadedStrings #-}


module Main where


-- base
import Control.Exception (IOException, catch)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Data.List (sortBy)

-- bytestring
import qualified Data.ByteString.Lazy  as B
import qualified Data.ByteString.Lazy.Char8 as C

-- stringsearch
import qualified Data.ByteString.Lazy.Search as S

-- vector
import Data.Vector (Vector, toList)

-- cassava
import Data.Csv

-- time
import qualified Data.Time as T

-- word8
import Data.Word8 (_semicolon)

-- unordered-containers
import qualified Data.HashMap.Strict as HM

-- hashable
import Data.Hashable

-- extra
import Data.Tuple.Extra
import Numeric.Extra

-- interactive-plot
import Graphics.Dynamic.Plot.R2

-- color
import qualified Data.Colour.Names as Color


data MonthInYear = MonthInYear {
  year :: Integer,
  month :: Int
} deriving (Show, Eq)

instance FromField MonthInYear where
  parseField field = do t <- T.parseTimeM True T.defaultTimeLocale "%d.%m.%Y" $ C.unpack $ B.fromStrict field
                        let g = T.toGregorian t
                        return (MonthInYear (fst3 g) (snd3 g))

instance Hashable MonthInYear where
  hashWithSalt s (MonthInYear m y) = s `hashWithSalt` m `hashWithSalt` y `hashWithSalt` m

data Transaction = Transaction {
  monthInYear :: MonthInYear,
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

decodeTransactions :: B.ByteString -> Either String (Vector Transaction)
decodeTransactions = fmap snd . decodeByNameWith (DecodeOptions _semicolon)

decodeTransactionsFromFile :: FilePath -> IO (Either String (Vector Transaction))
decodeTransactionsFromFile filePath = catchShowIO (C.readFile filePath) >>= (return . either Left decodeTransactions)

catchShowIO :: IO a -> IO (Either String a)
catchShowIO action = fmap Right action `catch` handleIOException where
                      handleIOException :: IOException -> IO (Either String a)
                      handleIOException = return . Left . show

type MonthlySums = HM.HashMap MonthInYear Float

aggregateTransactions :: Vector Transaction -> MonthlySums
aggregateTransactions transactions = HM.fromList [(MonthInYear y m, s) |
    let transactionList = toList transactions,
    let _year = year . monthInYear,
    let _month = month . monthInYear,

    let ys = map _year transactionList,
    let ms = map _month transactionList,

    y <- spne ys,
    m <- spne ms,
    let s = sum [amount t | t <- transactionList, y == _year t, m == _month t]
  ] where
    -- Sequentially Pairwise Not Equal, i.e., no two neighbours in the resulting list are the same
    spne :: Eq a => [a] -> [a]
    spne xs = _spne [] xs where
      _spne :: Eq a => [a] -> [a] -> [a]
      _spne [] (x:xs) = x : _spne [x] xs
      _spne res [] = res
      _spne res xs = res' ++ _spne res' (tail xs) where
        res' = res ++ [head xs | last res /= head xs]

main = do [filename] <- getArgs
          transactions <- decodeTransactionsFromFile filename
          let monthlySums = either (const Nothing) (Just . aggregateTransactions) transactions
          let plotData = sortBy (\(a,_) (b,_) -> compare a b)
                [(x :: Double, y :: Double) |
                  m <- maybe [] HM.keys monthlySums,
                  let x = fromIntegral (year m) + fromIntegral (month m) / 12.0 :: Double,
                  let y = floatToDouble $ fromMaybe 0 $ maybe (Just 0) (HM.lookup m) monthlySums
                ]
          let total = sum [snd p | p <- plotData]
          let plot =  tint (if total >= 0 then Color.green else Color.red) $ lineSegPlot plotData
          plotWindow [plot]
