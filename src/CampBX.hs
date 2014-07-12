{-# LANGUAGE OverloadedStrings #-}
{-|
Module          : CampBX
Description     : An API Library for the CampBX Bitcoin Market
Copyright       : (c) Pavan Rikhi, 2014
License         : GPL-3
Maintainer      : pavan@sleepanarchy.com
Stability       : experimental
Portability     : POSIX

CampBX is a library for interacting with the API for CampBX, a Bitcoing
Trading Market.

Actions are run in the 'CampBX' Monad, which should be supplied with
a 'CampBXConfig'. You will need to override the 'bxUser' and 'bxPass'
Config fields to use the 'Authorized Endpoints' functions.

The following actions are currently supported:

* Querying the Market Depth, Ticker and Account Balances
* Depositing & Withdrawing Funds from the CampBX Account
* Placing Quick & Advanced Buy/Sell Orders

-}
module CampBX
        (
        -- * CampBX Monad
          runCampBX
        , CampBXConfig(..)
        , defaultCampBXConfig
        -- * CampBX API Functions
        -- ** Anonymous Endpoints
        , getDepth
        , getTicker
        -- ** Authorized Endpoints
        , getWallet
        , getDepositAddress
        , getPendingOrders
        , sendBTC
        , placeQuickBuy
        , placeBuy
        , placeQuickSell
        , cancelBuyOrder
        , cancelSellOrder
          -- * Types
        , Ticker(..)
        , Depth(..)
        , Wallet(..)
        , DepositAddress(..)
        , OrderList(..)
        ) where

import qualified Data.ByteString.Char8 as BC

import CampBX.Client
import CampBX.Types


-- | Retrieves all the Buy & Sell Offers on the Market
getDepth :: CampBX (Either String Depth)
getDepth = queryEndPoint GetDepth []

-- | Retrieves the Current Market Ticker
getTicker :: CampBX (Either String Ticker)
getTicker = queryEndPoint GetTicker []

-- | Retrieves the User's Total, Liquid and Margin Account Funds
getWallet :: CampBX (Either String Wallet)
getWallet = queryEndPoint GetFunds []

-- | Retrieves a Bitcoin Address for Depositing Funds into the CampBX
-- Account
getDepositAddress :: CampBX (Either String DepositAddress)
getDepositAddress = queryEndPoint GetBTCAddr []

-- | Retrieves the Account's Open Orders
getPendingOrders :: CampBX (Either String OrderList)
getPendingOrders = queryEndPoint GetOrders []

-- | Sends Bitcoins from the CampBX Account to the Specified Address
sendBTC :: BTCAddress -> BTCAmount -> CampBX (Either String Integer)
sendBTC address quantity = queryEndPoint SendBTC
                                    [ ("BTCTo", BC.pack address)
                                    , ("BTCAmt", BC.pack $ show quantity)
                                    ]

-- | Places a Limit Order to Buy a Quantity of BTC at or Below a Given Price
placeQuickBuy :: BTCAmount -> BTCPrice -> CampBX (Either String (APIStatus Int))
placeQuickBuy quantity price = queryEndPoint TradeEnter
                                    [ ("TradeMode", "QuickBuy")
                                    , ("Quantity",  BC.pack $ show quantity)
                                    , ("Price",     BC.pack $ show price)
                                    ]

-- | Places an Advanced Buy Order With an Optional FillType, Dark Pool and
-- Expiration
placeBuy :: BTCAmount -> BTCPrice -> Maybe FillType -> Maybe DarkPool ->
            Maybe String -> CampBX (Either String (APIStatus Int))
placeBuy q p Nothing dp e  = placeBuy q p (Just Incremental) dp e
placeBuy q p ft Nothing e  = placeBuy q p ft (Just No) e
placeBuy q p ft dp Nothing = placeBuy q p ft dp (Just "")
placeBuy q p (Just ft) (Just dp) (Just e) = queryEndPoint TradeAdvanced
                                    [ ("TradeMode", "AdvancedBuy")
                                    , ("Quantity",  BC.pack $ show q)
                                    , ("Price",     BC.pack $ show p)
                                    , ("FillType",  BC.pack $ show ft)
                                    , ("DarkPool",  BC.pack $ show dp)
                                    , ("Expiry",    BC.pack e)
                                    ]

-- | Places a Limit Order to Sell a Quantity of BTC at or Above a Given Price
placeQuickSell :: BTCAmount -> BTCPrice -> CampBX (Either String (APIStatus Int))
placeQuickSell quantity price = queryEndPoint TradeEnter
                                    [ ("TradeMode", "QuickSell")
                                    , ("Quantity", BC.pack $ show quantity)
                                    , ("Price", BC.pack $ show price)
                                    ]

-- | Cancels the Buy Order with the Given Order ID
cancelBuyOrder :: Int -> CampBX (Either String (APIStatus Int))
cancelBuyOrder orderID = queryEndPoint TradeCancel
                                    [ ("Type", "Buy")
                                    , ("OrderID", BC.pack $ show orderID) ]

-- | Cancels the Sell Order with the Given Order ID
cancelSellOrder :: Int -> CampBX (Either String (APIStatus Int))
cancelSellOrder orderID = queryEndPoint TradeCancel
                                    [ ("Type", "Sell")
                                    , ("OrderID", BC.pack $ show orderID) ]
