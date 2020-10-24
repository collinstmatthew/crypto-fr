{-# LANGUAGE ScopedTypeVariables #-}
module Market (getBid,defProduct) where

import qualified Coinbase.Exchange.MarketData    as MarketData
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Coinbase.Exchange.Types         as Types
import qualified Data.Text                       as DT
import qualified Coinbase.Exchange.Types.Core    as Core

import Database


-- define the product could make these currencies types
defProduct :: Core.ProductId
defProduct = Core.ProductId (DT.pack "BTC-GBP")

getBid :: Types.ExchangeConf -> IO (Maybe Quote)
getBid conf = do
  -- get top of the order book
  result <- Types.runExchange conf $ MarketData.getTopOfBook defProduct
  --currentTime :: Int <- round `fmap` getPOSIXTime
  currentTime  <- POSIX.getCurrentTime

  return $ case result of
                 Left r -> Nothing
                 Right s -> Just (Quote currentTime price) where
                   MarketData.BookItem price _ _ = head . MarketData.bookBids $ s


