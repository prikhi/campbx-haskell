{-# LANGUAGE OverloadedStrings #-}
module CampBX.Client where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit

import CampBX.Types

-- | Make an Anonymous Request and Retrieve the Body of the Response
getResponseBody :: Manager -> EndPoint -> IO L.ByteString
getResponseBody manager ep           = runResourceT $ do
        reqBX <- liftIO $ makeBXReq ep
        res <- httpLbs reqBX manager
        return $ responseBody res

-- | Make an Authorized Request and Retrieve the Body of the Response
getAuthResponseBody :: Manager -> Auth -> EndPoint -> IO L.ByteString
getAuthResponseBody manager auth ep  = runResourceT $ do
        reqBX         <- liftIO $ makeBXReq ep
        let reqBXAuth = uncurry applyBasicAuth auth reqBX { method = "POST" }
        let reqAuth   = urlEncodedBody [("user", fst auth), ("pass", snd auth)] reqBXAuth
        res           <- httpLbs reqAuth manager
        return $ responseBody res

-- | Create a Secure Request w/ an Appropriate Timeout for CampBX to the
-- EndPoint
makeBXReq :: EndPoint -> IO Request
makeBXReq ep = do
        req           <- parseUrl $ makeURL ep
        let reqBX     = req { secure          = True
                            , responseTimeout = Just 240000000 }
        return reqBX

-- | Build the URL for an EndPoint
makeURL :: EndPoint -> String
makeURL e    = "https://campbx.com/api/" ++ endpoint e ++ ".php"
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
