module Trades (marketBTC) where

import qualified Coinbase.Exchange.MarketData    as MarketData
import qualified Coinbase.Exchange.Types         as Types
import qualified Coinbase.Exchange.Types.Core    as Core
import qualified Coinbase.Exchange.Types.Private as Private
import qualified Data.Text                       as DT

import Coinbase.Exchange.Private

-- market order for £10
marketBTC :: Core.Cost -> Core.Side -> Private.NewOrder
marketBTC amount side = Private.NewMarketOrder
    { Private.noProductId = Core.ProductId (DT.pack "BTC-GBP")
    , Private.noSide      = side
    --, Private.noSelfTrade = Private.CancelNewest
    , Private.noSelfTrade = Private.CancelNewest
    , Private.noClientOid = Nothing
    , Private.noSizeAndOrFunds = Right (Nothing, amount) -- this would mean buy bc for £10
    }

