{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString.Char8             as CBS
import           Network.HTTP.Client.TLS
import           Network.HTTP.Conduit
import           System.Environment
import           Test.Tasty

import           Coinbase.Exchange.Types
import           Coinbase.Exchange.Types.Core

import qualified Coinbase.Exchange.MarketData.Test as MarketData
import qualified Coinbase.Exchange.Private.Test    as Private
import qualified Coinbase.Exchange.Socket.Test     as Socket

main :: IO ()
main = do
        mgr     <- newManager tlsManagerSettings
        tKey    <- liftM CBS.pack $ getEnv "COINBASE_PRO_KEY"
        tSecret <- liftM CBS.pack $ getEnv "COINBASE_PRO_SECRET"
        tPass   <- liftM CBS.pack $ getEnv "COINBASE_PRO_PASSPHRASE"

        sbox    <- getEnv "COINBASE_PRO_SANDBOX"
        let apiType  = case sbox of
                        "FALSE" -> Live
                        "TRUE"  -> Sandbox
                        _       -> error "Coinbase sandbox option must be either: TRUE or FALSE (all caps)"

        case mkToken tKey tSecret tPass of
            Right tok -> defaultMain (tests $ ExchangeConf mgr (Just tok) apiType)
            Left   er -> error $ show er

tests :: ExchangeConf -> TestTree
tests conf = testGroup "Tests"
        [ MarketData.tests conf
        , Private.tests    conf
        , Socket.tests     conf products Socket.allChannels
        ]

-- TODO: Make this a list of all products
products = [ ProductId "ETH-BTC"
           , ProductId "ETH-EUR"
           , ProductId "ETH-USD"
           , ProductId "BTC-EUR"
           , ProductId "BTC-USD"
           ]
