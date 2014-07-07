{-# LANGUAGE OverloadedStrings #-}
module CampBX where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Conduit

import CampBX.Client
import CampBX.Types

main :: IO ()
main = do
        manager      <- liftIO $ newManager conduitManagerSettings
        --auth         <- getAuth
        let anonResp = getResponseBody manager
            --authResp = getAuthResponseBody manager auth
        ticker       <- getTicker anonResp
        print ticker
        depth        <- getDepth  anonResp
        print depth


-- | Get Authorization Details from StdIn
getAuth :: IO Auth
getAuth      = makeAuth <$> putStrGetBS "Username:" <*> putStrGetBS "Password:"
        where putStrGetBS s = putStr s >> BC.getLine

-- | Get all the Offers from the Depth List
getDepth :: BXDispatch -> IO (Maybe Depth)
getDepth f         = do
        r <- f GetDepth
        return $ decode r

-- | Get the Current Market Ticker
getTicker :: BXDispatch -> IO (Maybe Ticker)
getTicker f        = do
        r <- f GetTicker
        return $ decode r

-- | Get Pending/Placed Orders
getPendingOrders :: BXDispatch -> IO (Maybe OrderList)
getPendingOrders f = do
        r <- f GetOrders
        return $ decode r
