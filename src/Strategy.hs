{- LANGUAGE ScopedTypeVariables #-}
module Strategy (meanRevert,executeOrder,Strategy) where

import qualified Coinbase.Exchange.Types.Core    as Core
import qualified Coinbase.Exchange.Types.Private as Private
import qualified Coinbase.Exchange.Types         as Types
import qualified Data.Scientific                 as DS
import qualified Portfolio                       as P
import qualified Reactive.Banana                 as R
import qualified Reactive.Banana.Frameworks      as FR
import qualified Trades                          as Trades
import qualified Data.Time.Clock                 as TIME
import qualified Database                        as D
import qualified Data.Sequence                   as Seq

import           Data.Tuple.Extra                (fst3)
import           Control.Monad.IO.Class          (liftIO)
import           GHC.Float                       (int2Double)
import           Coinbase.Exchange.Private

type Strategy m e = R.Event e -> R.Event P.Portfolio -> m (R.Event Private.NewOrder)

-- (Running total, Current Quotes In Queue, time period)
type MVAVG = (Rational, Seq.Seq D.Quote, TIME.NominalDiffTime)

-- add new quote to the moving average
addMVAVG :: D.Quote -> MVAVG -> MVAVG
addMVAVG quote (runningtotal, seq, time) = removeMVAVG res where
    res           = (runningtotal', quote Seq.<| seq, time) :: MVAVG
    runningtotal' = ((runningtotal*n) + price) /(n+1)
    n             = toRational $ Seq.length seq
    price         = toRational $ D.price quote

-- remove and update the stuff in the moving average
removeMVAVG :: MVAVG -> MVAVG
removeMVAVG (runningtotal, seq, time) | timediff < time = (runningtotal, seq, time)
                                      | otherwise       = removeMVAVG (runningtotal',Seq.deleteAt 0 seq ,time)
    where
  left  Seq.:<| _ = seq
  _ Seq.:|> right = seq
  -- time difference in seconds
  timediff        = TIME.diffUTCTime (D.time left) (D.time right)
  n               = toRational $ Seq.length seq
  runningtotal'   = (runningtotal*n - price) /(n-1)
  price           = toRational $ D.price left


-- take event stream of quotes and returns an event stream of moving averages
eMA  :: R.MonadMoment m => TIME.NominalDiffTime -> R.Event D.Quote -> m (R.Event Rational)
eMA time quote = do
  let initialMVAVG = (0,Seq.empty,time) :: MVAVG
  runningQuote <- R.accumE initialMVAVG (addMVAVG <$> quote)
  return $ fmap fst3 runningQuote

ratToCost :: Rational -> Core.Cost
ratToCost = Core.Cost . Core.CoinScientific . DS.unsafeFromRational
-- buy if the 1 minute average is 10% below the 2 minute average
-- sell when the 1minute average is 10$ above the 2 minute average
-- look at the trade timestamps instead of worrying about the ticks
-- how much to buy and sell
-- MondaMoment means there is memory

-- so the moving average calculations can be behaviors that I update and as they applicatives
-- I can compose, then an actual buy signal is an event

plainChanges :: R.Behavior a -> FR.MomentIO (R.Event a)
plainChanges b = do
    (e, handle) <- FR.newEvent
    eb <- FR.changes b
    FR.reactimate' $ (fmap handle) <$> eb
    return e

meanRevert :: Strategy FR.MomentIO D.Quote
meanRevert newQuote portfolio = do
  -- time Period in seconds, 10 days
  let time10Minute = 60*2 :: TIME.NominalDiffTime
      time2Minute  = 60  :: TIME.NominalDiffTime

  tenMinuteMA <- eMA time10Minute newQuote
  twoMinuteMA <- eMA time2Minute newQuote

  tenMinuteMA' <- R.stepper (toRational 0) $ tenMinuteMA
  twoMinuteMA' <- R.stepper (toRational 0) $ twoMinuteMA

  let combined210MA :: R.Behavior Core.Side = R.liftA2 (\x y -> if x > 1.1 * y then Core.Sell else if x < 0.9 * y then Core.Buy else Core.Hold) twoMinuteMA' tenMinuteMA'

  signal <- plainChanges combined210MA
  let signal'      = R.filterE (\x -> x /= Core.Hold) signal
      createOrder' = fmap (Trades.marketBTC 10) signal'

  return $ createOrder'

average :: (Real a, Foldable t) => t a -> Rational
average l = toRational (foldr (+) 0 l) / toRational (length l)

executeOrder :: Types.ExchangeConf -> Private.NewOrder -> IO (Core.OrderId)
executeOrder conf order = do
    ranOrder <- Types.runExchange conf $ createOrder order
    print "executing order"
    --left is error do some handling
    --right is success
    return $ case ranOrder of
                Left  e -> error "couldn't execute quit do proper error handling"
                Right r -> r
