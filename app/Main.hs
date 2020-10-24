{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified BackTest                   as BT
import qualified Coinbase.Exchange.Types    as Types
import qualified Database.SQLite.Simple     as SQL
import qualified Events                     as E
import qualified Portfolio                  as P
import qualified Reactive.Banana.Frameworks as FR
import qualified Strategy                   as S

-- first keep track of current portfolio with live trading
-- and then when this is done I can think about the backtesting environment
-- do unit tests as well

main :: IO ()
main = do
    -- open database connection
    -- this should be more generic for reading and writing
    conn <- SQL.open "priceData.db"

    -- read environment variablers and produce a configuration or throw an exception
    conf' <- Types.configure

    -- left as it san exchange config
    --let conf = Left conf'
    let conf = Right $ BT.BackTestConf{BT.val = True}

    -- set the strategy to be a simple moving average mean reversion
    let strategy = S.meanRevert

    -- make sure this is loaded properly currently intialize to no orders
    let portfolio = P.Portfolio {P._settledOrders = [], P._unSettledOrders = []}

    sources <- E.makeSources
    network <- FR.compile $ E.networkDescription conn conf strategy portfolio sources

    FR.actuate network

    E.eventLoop strategy sources

    -- close database connection
    SQL.close conn
