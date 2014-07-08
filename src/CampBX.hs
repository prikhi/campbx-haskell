{-# LANGUAGE OverloadedStrings #-}
module CampBX
        ( getAuth
        , getDepth
        , getTicker
        , getWallet
--        , getPendingOrders
        , getDepositAddress
        , getResponseBody
        , getAuthResponseBody ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BC

import CampBX.Client
import CampBX.Types

-- | Get Authorization Details from StdIn
getAuth :: IO Auth
getAuth            = makeAuth <$> putStrGetBS "Username: " <*> putStrGetBS "Password: "
        where putStrGetBS s = putStr s >> BC.getLine

-- | Get all the Offers from the Depth List
getDepth :: BXDispatch -> IO (Either String Depth)
getDepth f         = do
        r <- f GetDepth
        return $ eitherDecode r

-- | Get the Current Market Ticker
getTicker :: BXDispatch -> IO (Either String Ticker)
getTicker f        = do
        r <- f GetTicker
        return $ eitherDecode r

getWallet :: BXDispatch -> IO (Either String Wallet)
getWallet f = do
        r <- f GetFunds
        return $ eitherDecode r

-- | Get Pending/Placed Orders
--getPendingOrders :: BXDispatch -> IO (Either String OrderList)
--getPendingOrders f = do
--        r <- f GetOrders
--        print r
--        return $ eitherDecode r

-- | Get a Bitcoin Address to Make CampBX Deposits To
getDepositAddress :: BXDispatch -> IO (Either String DepositAddress)
getDepositAddress f = do
        r <- f GetBTCAddr
        return $ eitherDecode r
