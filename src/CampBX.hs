{-# LANGUAGE OverloadedStrings #-}
module CampBX
        ( runCampBX
        , defaultCampBXConfig
        , getDepth
        , getTicker
        , getWallet
        , getDepositAddress
        , getPendingOrders
        , sendBTC
        , Ticker(..)
        , Depth(..)
        , Wallet(..)
        , DepositAddress(..)
        , OrderList(..)
        ) where

import qualified Data.ByteString.Char8 as BC

import CampBX.Client
import CampBX.Types


-- | Get all the Offers from the Depth List
getDepth :: CampBX (Either String Depth)
getDepth = queryEndPoint GetDepth []

-- | Get the Current Market Ticker
getTicker :: CampBX (Either String Ticker)
getTicker = queryEndPoint GetTicker []

-- | Get the User's Total, Lquid and Marginal Funds
getWallet :: CampBX (Either String Wallet)
getWallet = queryEndPoint GetFunds []

-- | Get a Bitcoin Address for Depositing Funds into the User's CampBX
-- Account
getDepositAddress :: CampBX (Either String DepositAddress)
getDepositAddress = queryEndPoint GetBTCAddr []

-- | Get Pending/Placed Orders
getPendingOrders :: CampBX (Either String OrderList)
getPendingOrders = queryEndPoint GetOrders []

-- | Sends Bitcoins from the User's Account to an Address
sendBTC :: BTCAddress -> BTCAmount -> CampBX (Either String Integer)
sendBTC address amount = queryEndPoint SendBTC [("BTCTo", BC.pack address),
                                                ("BTCAmt", BC.pack $ show amount)]
