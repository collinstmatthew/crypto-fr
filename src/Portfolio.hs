{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Portfolio(Portfolio(..),sweepSettled,updateUnSettledOrder,updateAll) where

import qualified Coinbase.Exchange.MarketData    as MarketData
import qualified Coinbase.Exchange.Types         as Types
import qualified Coinbase.Exchange.Types.Core    as Core
import qualified Coinbase.Exchange.Types.Private as Private
import qualified Coinbase.Exchange.Private       as P
import qualified Data.Text                       as DT

import qualified BackTest                        as BT
import qualified Data.Set                        as Set

import Control.Lens
import Control.Monad.IO.Class(liftIO)

-- should be able to add two portfolio's together
-- mabye orders isn't the right type to use here trade_id?
data Portfolio = Portfolio { _settledOrders :: [P.Order], _unSettledOrders :: [Core.OrderId] } deriving (Show)

$(makeLenses ''Portfolio)

updateUnSettledOrder :: Core.OrderId -> Portfolio -> Portfolio
updateUnSettledOrder newOrder portfolio = over unSettledOrders (|> newOrder) portfolio

run_getOrder :: Types.ExchangeConf -> Core.OrderId -> IO P.Order
run_getOrder conf oid = onSuccess conf (P.getOrder oid) "Failed to get order info"

run_getOrderList :: Types.ExchangeConf -> [Core.OrderStatus] -> IO [P.Order]
run_getOrderList conf ss = onSuccess conf (P.getOrderList ss) "Failed to get order list"


onSuccess :: Types.ExchangeConf -> Types.Exchange a -> String -> IO a
onSuccess conf apicall errorstring = do
        r <- liftIO $ Types.runExchange conf apicall
        case r of
            Left  e -> print e >> error errorstring
            Right a -> return a

-- add the new completed orders to the existing orders and replace the unsettled orders in the orderId
updateAll :: ([P.Order],[Core.OrderId]) ->  Portfolio -> Portfolio
updateAll (recentSettledOrders,unSettledOrdersId) portfolio = portfolio & unSettledOrders .~ unSettledOrdersId & settledOrders %~ (recentSettledOrders ++ )


-- move all the trades that are settled into settledOrders to update the portfolio
sweepSettled :: Types.ExchangeConf -> Portfolio -> IO ([P.Order],[Core.OrderId])
sweepSettled conf portfolio = do
    unSettledOrdersG <- run_getOrderList conf [Core.Open, Core.Pending, Core.Active]

    let unSettledOrdersId = map Private.orderId unSettledOrdersG
        recentlySettledId = Set.difference (Set.fromList (portfolio ^. unSettledOrders )) (Set.fromList unSettledOrdersId)

    recentSettledOrders <- mapM (run_getOrder conf) (Set.toList recentlySettledId)
    return $ (recentSettledOrders, unSettledOrdersId)


-- for backtesting all orders execute immediately
sweepSettled' :: BT.BackTestConf -> Portfolio -> IO ([P.Order],[Core.OrderId])
sweepSettled conf portfolio = do
    -- all orders settle immediately
    let unSettledOrdersG = []

    let unSettledOrdersId = map Private.orderId unSettledOrdersG
    -- get the id of the all the recently settled orders
        recentlySettledId = Set.difference (Set.fromList (portfolio ^. unSettledOrders )) (Set.fromList unSettledOrdersId)

    -- get the order information from all recently settled orders
    recentSettledOrders <- mapM (run_getOrder conf) (Set.toList recentlySettledId)
    return $ (recentSettledOrders, unSettledOrdersId)


