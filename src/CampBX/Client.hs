{-# LANGUAGE OverloadedStrings #-}
module CampBX.Client
        ( CampBX
        , runCampBX
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

import CampBX.Types

type CampBX a = LoggingT (StateT CampBXConfig (ResourceT IO)) a

runCampBX :: MonadIO m => CampBXConfig -> CampBX a -> m a
runCampBX config action = liftIO . runResourceT . fmap fst .
                          flip runStateT config . runStderrLoggingT $ action

-- | Represents the Current CampBX Configuration State
data CampBXConfig = CampBXConfig { bxUser    :: B.ByteString -- The User's Name
                                 , bxPass    :: B.ByteString -- The User's Password
                                 , bxUrl     :: String  -- The Desired API URL
                                 , bxManager :: Manager -- The Connection Manager
                                 , lastReq   :: Int -- The POSIX Time of the Last Request in Milliseconds
                                 }

-- | Creates a CampBXConfig with a new Manager
defaultCampBXConfig :: MonadIO m => m CampBXConfig
defaultCampBXConfig = do
        man <- liftIO $ newManager conduitManagerSettings
        return CampBXConfig { bxUser    = ""
                            , bxPass    = ""
                            , bxUrl     = "https://campbx.com/api/"
                            , bxManager = man
                            , lastReq   = 0 }


-- | Query an EndPoint, waiting at least 500 Milliseconds from the previous
-- request
queryEndPoint :: (FromJSON a) => EndPoint -> [(B.ByteString, B.ByteString)] ->
                                 CampBX (Either String a)
queryEndPoint ep postData = do
        config   <- get
        reqTime  <- getMilliseconds
        let timeDiff time =  abs $ time - lastReq config
        when (timeDiff reqTime < 500) (liftIO . threadDelay . timeDiff $ reqTime)

        initReq  <- liftIO $ parseUrl $ makeURL (bxUrl config) ep
        let authReq = urlEncodedBody
                            ([("user", bxUser config), ("pass", bxPass config)]
                             ++ postData) initReq
        response <- httpLbs authReq $ bxManager config
        respTime <- getMilliseconds
        put config { lastReq  = respTime }
        return $ eitherDecode $ responseBody response
        where getMilliseconds = liftIO $ (round . (* 1000)) <$> getPOSIXTime

-- | Build the URL for an EndPoint
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
