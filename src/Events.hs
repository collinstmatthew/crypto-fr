{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Events (filterMapJust,makeSources,fire,addHandler,EventSource(..),networkDescription,eventLoop) where

import qualified Coinbase.Exchange.Types.Core    as Core
import qualified Coinbase.Exchange.Types         as Types
import qualified Coinbase.Exchange.Types.Private as Private
import qualified Coinbase.Exchange.Private       as Pr
import qualified Reactive.Banana.Frameworks      as FR
import qualified Reactive.Banana                 as R
import qualified Control.Concurrent.Async.Timer  as AT
import qualified Database.SQLite.Simple          as SQL
import qualified Market                          as M
import qualified Strategy                        as S
import qualified Database                        as D
import qualified Portfolio                       as P
import qualified BackTest                        as BT

import Control.Monad (forM_)

type EventSource a = (FR.AddHandler a, a -> IO ())

filterMapJust :: (a -> Maybe b) -> R.Event a -> R.Event b
filterMapJust f = R.filterJust . fmap f

addHandler :: EventSource a -> FR.AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd

-- Create event sources corresponding to get trade
makeSources = FR.newAddHandler

-- Read commands and fire corresponding events
-- figure out how I time stuff and then I think the structure should become clearer
-- here genreate events every 5 seconds or so
-- waiting 5 seconds until a trade is polled this should be part of the strategy?
-- probably not the strategy can lookat time stamps if it wants this is part of the
-- data parts
eventLoop :: p -> EventSource () -> IO ()
eventLoop strategy getMarket = do
      -- requires a polling rate because of rest API
      -- make this part of the config or strategy or something
      let conf = AT.setInterval (10000) AT.defaultConf

      AT.withAsyncTimer conf $ \timer ->
        forM_ [1..] $ \_ -> do
          AT.wait timer
          putStrLn "Tick"
          -- when not live testing this instead fires a different event
          fire getMarket()
      return ()

-- recursive do look at recursion in combinators recursive banana doc
-- this is the implementation
networkDescription :: SQL.Connection -> Either Types.ExchangeConf BT.BackTestConf -> S.Strategy FR.MomentIO D.Quote -> P.Portfolio -> EventSource () -> FR.MomentIO ()
networkDescription conn (Left conf) strategy initialPort getMarket = mdo

    -- obtain events corresponding to the getMarket  command
    tradeFired <- FR.fromAddHandler (addHandler getMarket)

    -- portfolio is updated when a new order is executed
    -- a portfolio is persistent as well so maybe this should be a behaviour?
    portfolio :: R.Event P.Portfolio <- R.accumE initialPort $ R.unions
                                              [  -- adds a new unsettled order
                                                 P.updateUnSettledOrder <$> neworderId
                                                -- moves the completed orders
                                                ,P.updateAll            <$> newOrders
                                              ]

    -- contains the new unSettledOrders that have been executed and the recently Settled Orders to add
    newOrders :: R.Event ([Pr.Order],[Core.OrderId])  <- FR.execute $ FR.liftIO  <$> P.sweepSettled conf <$> portfolio

    -- get the trade price and put into an event
    -- eventually be order book state
    tradeQuote :: R.Event (Maybe D.Quote) <- FR.execute (FR.liftIO (M.getBid conf) <$ tradeFired)

    -- strategy takes the quotes and the current portfolio
    signal <- strategy (R.filterJust tradeQuote) portfolio

    -- execute the order and put the orderId in the current portfolio state
    neworderId :: R.Event Core.OrderId <- FR.mapEventIO (S.executeOrder conf) signal

    -- make sure I save the orders into a file or something
    FR.reactimate $ D.writeQuote conn <$> (R.filterJust tradeQuote)
-- implementation for backtesting environment
networkDescription conn (Right conf) strategy initialPort getMarket = mdo

    -- obtain events corresponding to the getMarket  command
    tradeFired <- FR.fromAddHandler (addHandler getMarket)

    -- portfolio is updated when a new order is executed
    -- a portfolio is persistent as well so maybe this should be a behaviour?
    portfolio :: R.Event P.Portfolio <- R.accumE initialPort $ R.unions
                                              [  -- adds a new unsettled order
                                                 P.updateUnSettledOrder <$> neworderId
                                                -- moves the completed orders
                                                ,P.updateAll            <$> newOrders
                                              ]

    -- get the trade price and put into an event
    -- eventually be order book state
    tradeQuote :: R.Event (Maybe D.Quote) <- FR.execute (FR.liftIO (M.getBid conf) <$ tradeFired)

    -- strategy takes the quotes and the current portfolio
    signal <- strategy (R.filterJust tradeQuote) portfolio

    -- execute the order and put the orderId in the current portfolio state
    neworderId :: R.Event Core.OrderId <- FR.mapEventIO (S.executeOrder conf) signal

    -- make sure I save the orders into a file or something
    FR.reactimate $ D.writeQuote conn <$> (R.filterJust tradeQuote)

