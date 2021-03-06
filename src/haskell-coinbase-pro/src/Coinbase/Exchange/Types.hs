{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module Coinbase.Exchange.Types
    ( ApiType (..)
    , Endpoint
    , Path

    , website
    , sandboxRest
    , sandboxSocket
    , liveRest
    , liveSocket
    , liveRealCoinbaseRest
    , sandboxRealCoinbaseRest

    , Key
    , Secret
    , Passphrase

    , Token
    , key
    , secret
    , passphrase
    , mkToken

    , ExchangeConf (..)
    , configure
    , ExchangeFailure (..)

    , Exchange
    , ExchangeT
    , ExceptT
    , runExchange
    , runExchangeT
    , execExchange
    , execExchangeT

    , getManager
    ) where

import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Base64       as Base64
import qualified Data.ByteString.Char8        as Char8
import           Data.Data
import           Data.Text                    (Text)
import           GHC.Generics
import           Network.HTTP.Conduit
import           System.Environment           (getEnv)

-- API URLs

data ApiType
    = Sandbox
    | Live
    deriving (Show)

type Endpoint = String
type Path     = String

website :: Endpoint
website = "https://public.sandbox.pro.coinbase.com"

sandboxRest :: Endpoint
sandboxRest = "https://api-public.sandbox.pro.coinbase.com"

sandboxSocket :: Endpoint
sandboxSocket = "ws-feed-public.sandbox.pro.coinbase.com"

liveRest :: Endpoint
liveRest = "https://api.pro.coinbase.com"

liveSocket :: Endpoint
liveSocket = "ws-feed.pro.coinbase.com"

-- Coinbase needs to provide real BTC transfers through the exchange API soon,
-- making 2 API calls with 2 sets of authentication credentials is ridiculous.
liveRealCoinbaseRest :: Endpoint
liveRealCoinbaseRest = "https://api.coinbase.com"

sandboxRealCoinbaseRest :: Endpoint
sandboxRealCoinbaseRest = "https://api.sandbox.coinbase.com"

-- Monad Stack

type Key        = ByteString
type Secret     = ByteString
type Passphrase = ByteString

data Token
    = Token
        { key        :: ByteString
        , secret     :: ByteString
        , passphrase :: ByteString
        }

mkToken :: Key -> Secret -> Passphrase -> Either String Token
mkToken k s p = case Base64.decode s of
                    Right s' -> Right $ Token k s' p
                    Left  e  -> Left e

data ExchangeConf
    = ExchangeConf
        { manager   :: Manager
        , authToken :: Maybe Token
        , apiType   :: ApiType
        }

data ExchangeFailure = ParseFailure Text
                     | ApiFailure Text
                     | AuthenticationRequiredFailure Text
                     | AuthenticationRequiresByteStrings
                     deriving (Show, Data, Typeable, Generic)

instance Exception ExchangeFailure

type Exchange a = ExchangeT IO a

newtype ExchangeT m a = ExchangeT { unExchangeT :: ReaderT ExchangeConf (ExceptT ExchangeFailure m) a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadThrow
             , MonadError ExchangeFailure
             , MonadReader ExchangeConf
             )

deriving instance (MonadBase IO m) => MonadBase IO (ExchangeT m)

runExchange :: ExchangeConf -> Exchange a -> IO (Either ExchangeFailure a)
runExchange = runExchangeT

runExchangeT :: MonadBaseControl IO m => ExchangeConf -> ExchangeT m a -> m (Either ExchangeFailure a)
runExchangeT conf = runExceptT . flip runReaderT conf . unExchangeT

execExchange :: ExchangeConf -> Exchange a -> IO a
execExchange = execExchangeT

execExchangeT :: (MonadThrow m, MonadBaseControl IO m) => ExchangeConf -> ExchangeT m a -> m a
execExchangeT conf act = do
    v <- runExceptT . flip runReaderT conf . unExchangeT $ act
    case v of
        Left er -> throwM er
        Right v -> return v

-- Utils

getManager :: (MonadReader ExchangeConf m) => m Manager
getManager = do
        conf <- ask
        return $ manager conf

-- Read environment variables and produce a configuration using a new TLS
-- manager, or throw an exception.
configure :: IO ExchangeConf
configure = do
        mgr        <- newManager tlsManagerSettings

        sandbox    <- getEnv "COINBASE_PRO_SANDBOX"

        let getEnvChar8 = fmap Char8.pack . getEnv

        key        <- getEnvChar8 "COINBASE_PRO_KEY"
        secret     <- getEnvChar8 "COINBASE_PRO_SECRET"
        passphrase <- getEnvChar8 "COINBASE_PRO_PASSPHRASE"

        let api = case sandbox of
                    "FALSE" -> Live
                    "TRUE"  -> Sandbox
                    _       -> error "Set COINBASE_PRO_SANDBOX to TRUE to use \
                                     \the sandbox server or FALSE to use the \
                                     \live server."

        case mkToken key secret passphrase of
            Right token -> return $ ExchangeConf mgr (Just token) api
            Left  e     -> error e
