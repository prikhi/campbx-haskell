{-# LANGUAGE OverloadedStrings #-}
{-|
-
- This Module contains functions related to interacting with the CampBX
- server, such as creating a default configuration, querying urls and
- POSTing data.
-
-}
module Web.CampBX.Client
        ( CampBX
        , runCampBX
        , CampBXConfig(..)
        , defaultCampBXConfig
        , queryEndPoint
        ) where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.State
import Control.Monad.Logger
import Data.Aeson (eitherDecode, FromJSON)
import qualified Data.ByteString as B
import Data.Time.Clock.POSIX
import Network.HTTP.Conduit

import Web.CampBX.Types

-- | The CampBX Monad Holds the Configuration State and Allows Network and
-- Logging Actions
type CampBX a           = LoggingT (StateT CampBXConfig (ResourceT IO)) a

-- | Run a CampBX Action
runCampBX :: MonadIO m => CampBXConfig -> CampBX a -> m a
runCampBX config action = liftIO . runResourceT . flip evalStateT config .
                          runStderrLoggingT $ action

-- | Represents the Current CampBX Configuration State
data CampBXConfig       = CampBXConfig
                            { -- | The User to Login As
                              bxUser    :: B.ByteString
                              -- | The User's Password
                            , bxPass    :: B.ByteString
                              -- | The Desired API URL
                            , bxUrl     :: String
                              -- | The Connection Manager for All Requests
                            , bxManager :: Manager
                              -- | The POSIX Time of the Last Request
                              -- in Milliseconds. CampBX Allows 1 Request
                              -- Every 500 Milliseconds.
                            , lastReq   :: Int
                            }

-- | Creates a 'CampBXConfig' with a new Manager
defaultCampBXConfig :: MonadIO m => m CampBXConfig
defaultCampBXConfig     = do
        man <- liftIO $ newManager conduitManagerSettings
        return CampBXConfig { bxUser    = ""
                            , bxPass    = ""
                            , bxUrl     = "https://campbx.com/api/"
                            , bxManager = man
                            , lastReq   = 0 }


-- | Queries an API EndPoint, waiting at least 500 Milliseconds from the
-- previous request
queryEndPoint :: (FromJSON a) => EndPoint -> [(B.ByteString, B.ByteString)] ->
                                 CampBX (Either String a)
queryEndPoint ep postData = do
        config   <- get
        reqTime  <- getMilliseconds
        let timeDiff =  abs $ reqTime - lastReq config
        when (timeDiff < 500) (liftIO . threadDelay $ timeDiff)
        initReq  <- liftIO $ parseUrl $ makeURL (bxUrl config) ep
        let authReq = urlEncodedBody ([("user", bxUser config),
                                       ("pass", bxPass config)] ++ postData)
                                     initReq
        response <- httpLbs authReq $ bxManager config
        respTime <- getMilliseconds
        put config { lastReq  = respTime }
        return $ eitherDecode $ responseBody response
        where getMilliseconds = liftIO $ (round . (* 1000)) <$> getPOSIXTime

-- | Builds the URL for the EndPoint
makeURL :: String -> EndPoint -> String
makeURL url e         = url ++ endpoint e ++ ".php"
        where endpoint GetDepth      = "xdepth"
              endpoint GetTicker     = "xticker"
              endpoint GetFunds      = "myfunds"
              endpoint GetOrders     = "myorders"
              endpoint GetMargins    = "mymargins"
              endpoint GetBTCAddr    = "getbtcaddr"
              endpoint SendBTC       = "sendbtc"
              endpoint TradeCancel   = "tradecancel"
              endpoint TradeEnter    = "tradeenter"
              endpoint TradeAdvanced = "tradeadv"
