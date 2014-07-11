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
        , placeQuickBuy
        , placeQuickSell
        , cancelBuyOrder
        , cancelSellOrder
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
sendBTC address quantity = queryEndPoint SendBTC [("BTCTo", BC.pack address),
                                                  ("BTCAmt", BC.pack $ show quantity)]

-- | Place a Limit Order to Buy a Quantity of BTC at or Below a Given Price
placeQuickBuy :: BTCAmount -> BTCPrice -> CampBX (Either String (APIStatus Int))
placeQuickBuy quantity price = queryEndPoint TradeEnter
                               [ ("TradeMode", "QuickBuy")
                               , ("Quantity", BC.pack $ show quantity)
                               , ("Price", BC.pack $ show price)
                               ]

-- | Place a Limit Order to Sell a Quantity of BTC at or Above a Given Price
placeQuickSell :: BTCAmount -> BTCPrice -> CampBX (Either String (APIStatus Int))
placeQuickSell quantity price = queryEndPoint TradeEnter
                                [ ("TradeMode", "QuickSell")
                                , ("Quantity", BC.pack $ show quantity)
                                , ("Price", BC.pack $ show price)
                                ]

-- | Cancel the Buy Order with the Given Order ID
cancelBuyOrder :: Int -> CampBX (Either String (APIStatus Int))
cancelBuyOrder orderID = queryEndPoint TradeCancel
                         [ ("Type", "Buy")
                         , ("OrderID", BC.pack $ show orderID) ]

-- | Cancel the Sell Order with the Given Order ID
cancelSellOrder :: Int -> CampBX (Either String (APIStatus Int))
cancelSellOrder orderID = queryEndPoint TradeCancel
                         [ ("Type", "Sell")
                         , ("OrderID", BC.pack $ show orderID) ]
