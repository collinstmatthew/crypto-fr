{-# LANGUAGE ScopedTypeVariables #-}
module Database(priceToDouble, writeQuote, Quote(..) ) where

import qualified Coinbase.Exchange.Types.Core    as Core
import qualified Database.SQLite.Simple          as SQL
import qualified Data.Time.Clock.POSIX           as POSIX
import qualified Data.Time.Clock                 as TIME
import qualified Data.Text                       as DT

-- quote of bitcoin price in the format seconds from epoch, price
data Quote = Quote{ time :: TIME.UTCTime, price :: Core.Price} deriving (Show)

writeQuote :: SQL.Connection -> Quote -> IO ()
writeQuote conn quote = do
  let Quote currentTime price = quote
      price' = priceToDouble price
      time :: Int = round $ toRational (POSIX.utcTimeToPOSIXSeconds currentTime)

  print $ "time = "++ show currentTime ++  ". best Bid = " ++ show price

  SQL.execute conn (SQL.Query $ DT.pack "INSERT INTO btcStream (quoteDate,quoteValue) VALUES (?,?)") (time, price')



-- helper function for sql database
priceToDouble :: Core.Price -> Double
priceToDouble = fromRational . toRational


-- the minimum order amount is for 0.001 bc
cbFee = True
