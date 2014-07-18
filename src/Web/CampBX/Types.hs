{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
-
- This Module contains the CampBX-specific Types and their JSON parsing
- instances.
-
-}
module Web.CampBX.Types
        ( EndPoint(..)
        , APIStatus(..)
        , BTCAddress
        , BTCAmount
        , USDAmount
        , BTCPrice
        , Bid(..)
        , Ask(..)
        , Order(..)
        , OrderType(..)
        , PriceType(..)
        , FillType(..)
        , Margin(..)
        , DarkPool(..)
        , Depth(..)
        , Ticker(..)
        , Wallet(..)
        , OrderList(..)
        , DepositAddress(..)
        , TransferResponse(..)
        ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import GHC.Generics


-- | API EndPoints
data EndPoint    = GetDepth
                 | GetTicker
                 | GetFunds
                 | GetOrders
                 | GetMargins
                 | GetBTCAddr
                 | SendBTC
                 | TradeCancel
                 | TradeEnter
                 | TradeAdvanced
                 deriving (Show)

-- | API Response Statuses
data APIStatus a = Success a    -- ^ The Request was Successful
                 | Info a       -- ^ The Response Contains Information
                                -- About the Request
                 | Error a      -- ^ The Request Returned an Error
                 deriving (Show, Generic)


-- | A Bitcoin Wallet Address
type BTCAddress = String
-- | A Specific Quantity of Bitcoins
type BTCAmount  = Double
-- | A Specific Quantity of United States Dollars
type USDAmount  = Double
-- | The Price or Value of a Single Bitcoin in terms of USD
type BTCPrice   = USDAmount

-- | An Offer to buy BTC in exchange for USD
newtype Bid     = Bid (BTCPrice, BTCAmount) deriving (Show, Generic)
-- TODO: Using record syntax would be best, but have to figure out how to
-- write the parseJSON for Array -> record instead of Array -> pair
--data Bid        = Bid       { -- | The Maximum Price the Bidder is Willing to
--                              -- Pay
--                              bidPrice  :: BTCPrice
--                              -- | The Amount of BTC the Bidder Wishes to
--                              -- Purchase
--                            , bidAmount :: BTCAmount
--                            } deriving (Show, Generic)

-- | An Offer to Sell BTC in Exchange for USD
newtype Ask     = Ask (BTCPrice, BTCAmount) deriving (Show, Generic)
-- TODO: Using record syntax would be best, but have to figure out how to
-- write the parseJSON for Array -> record instead of Array -> pair, which
-- is derived automatically
--data Ask        = Ask       { -- | The Minimum Price the Asker Wants
--                              askPrice  :: BTCPrice
--                              -- | The Amuount of BTC the Asker is
--                              -- Willing to Sell
--                            , askAmount :: BTCAmount
--                            } deriving (Show, Generic)


-- | An OrderType Describe's Whether the Order is an Ask or a Bid
data OrderType  = Sell | QuickSell | Short | Buy | QuickBuy | Cover
                deriving (Show, Generic)

-- | The PriceType Dictates What Price The Order Should Trade At
data PriceType  = Market      -- ^ A Market Order Trades at the Best
                              -- Available Price
                | Limit       -- ^ A Limit Order Trades at a Specific Price
                              -- or Better
                | StopLimit   -- ^ A Stop Limit Order is a Trade that begins
                              -- as a Stop Order and turns into a Limit Order
                              -- when the Stop is reached.
                deriving (Show, Generic)
-- | The FillType Describes How the Order Should be Executed,
-- Incrementally, All-at-Once or Immediately.
data FillType   = Incremental -- ^ The Order Lasts Until Completed or
                              -- Cancelled. Partial Execution/Completion is
                              -- Allowed
                | FOK         -- ^ The Order Must be Executed Immediately
                | AON         -- ^ The Order Must be Executed in it's
                              -- Entirety, not Incrementally
                deriving (Show, Generic)

-- | The Percentage of the Order to Put on the Margin Account
data Margin     = Int         -- ^ The Percentage of the Order to Put on Margin
                | None        -- ^ None of the Order Should be on Margin
                deriving (Show, Generic)

-- | A DarkPool Order is not Shown in the Depth List
data DarkPool   = Yes         -- ^ Hide the Order from the Depth List
                | No          -- ^ Show the Order in the Depth List
                deriving (Show, Generic)

-- | An Order Represents a Single Bid or Ask
data Order      = Order OrderType PriceType FillType Margin DarkPool
                deriving (Show, Generic)



-- | The List of Current Asks and Bids on the Market
data Depth      = Depth     { -- | The Current Ask Offers
                              asks :: [Ask]
                              -- | The Current Bid Offers
                            , bids :: [Bid]
                            } deriving (Show, Generic)

-- | The Lowest Ask and Highest Bid Currently on the Market and the Price
-- of the Last Trade
data Ticker     = Ticker    { -- | The Lowest Sell Offer on the Market
                              bestAsk   :: BTCPrice
                              -- | The Highest Buy Offer on the Market
                            , bestBid   :: BTCPrice
                              -- | The Price in USD per BTC of the Last
                              -- Trade
                            , lastTrade :: BTCPrice } deriving (Show, Generic)

-- | A User's Wallet Contains Liquid and Marginal USD/BTC
data Wallet     = Wallet    { -- | The Account's Total USD
                              totalUSD  :: USDAmount
                              -- | The Account's Total BTC
                            , totalBTC  :: BTCAmount
                              -- | USD Available for New Orders
                            , liquidUSD :: USDAmount
                              -- | BTC Available for New Orders
                            , liquidBTC :: BTCAmount
                              -- | Margin Account's USD Balance
                            , marginUSD :: USDAmount
                              -- | Margin Account's BTC Balance
                            , marginBTC :: BTCAmount } deriving (Show, Generic)

-- | A List of the User's Pending Buy and Sell Orders
data OrderList  = OrderList { -- | The Account's Open Buy Orders
                              buys    :: [Order]
                              -- | The Account's Open Sell Orders
                            , sells   :: [Order] }
                            deriving (Show, Generic)

-- TODO: Figure out what the returned Integer represents...
-- | An Address to Deposit BTC into the CampBX Account
data DepositAddress   = DepositAddress
                            { -- | The Single-Use Bitcoin Wallet Address
                              -- for Adding BTC to the Account
                              btcAddress :: String
                              -- | The Expiration Date of the Deposit
                              -- Address
                            , expireDate :: Integer } deriving (Show, Generic)

-- | A TransferResponse is Received After Sending BTC to an External Wallet
data TransferResponse = TransferResponse { transaction :: Integer } deriving (Show, Generic)


-- JSON Parsing Instances
instance FromJSON Bid
instance FromJSON Ask
instance FromJSON OrderType
instance FromJSON PriceType
instance FromJSON FillType
instance FromJSON Margin
instance FromJSON DarkPool
instance FromJSON Order
instance FromJSON (APIStatus String)
instance FromJSON (APIStatus Int)

instance FromJSON Ticker where
        parseJSON (Object v) = do
            bid       <- read <$> v .: "Best Bid"
            ask       <- read <$> v .: "Best Ask"
            trade     <- read <$> v .: "Last Trade"
            return $ Ticker ask bid trade
        parseJSON _          = fail "Did not receive a valid Ticker JSON object."

instance FromJSON Depth where
        parseJSON (Object v) = do
            depthAsks <- v .: "Asks"
            depthBids <- v .: "Bids"
            return $ Depth depthAsks depthBids
        parseJSON _          = fail "Did not receive a valid Depth JSON object."

instance FromJSON Wallet where
        parseJSON (Object v) = do
            totalUsd  <- read <$> v .: "Total USD"
            totalBtc  <- read <$> v .: "Total BTC"
            liquidUsd <- read <$> v .: "Liquid USD"
            liquidBtc <- read <$> v .: "Liquid BTC"
            marginUsd <- read <$> v .: "Margin Account USD"
            marginBtc <- read <$> v .: "Margin Account BTC"
            return $ Wallet totalUsd totalBtc liquidUsd liquidBtc marginUsd marginBtc
        parseJSON _          = fail "Did not receive a valid Wallet JSON object."

instance FromJSON DepositAddress where
        parseJSON (Object v) = do
            address <- v .: "Success"
            expires <- v .: "Expiry"
            return $ DepositAddress address expires
        parseJSON _          = fail "Did not receive a valid BTC Deposit Address JSON object."

instance FromJSON TransferResponse where
        parseJSON (Object v) = do
            txId <- v .: "Success"
            return $ TransferResponse txId
        parseJSON _          = fail "Attempt to send BTC failed."

instance FromJSON OrderList where
        parseJSON (Object v) =
            OrderList <$> buyList <*> sellList
            where buyList    = case HM.lookup "Buy" v of
                      Just (Array a) -> case HM.lookup "Info" arrayObj of
                                            Just _  -> return []
                                            Nothing -> parseJSON (Array a)
                                            where Object arrayObj = head $ V.toList a
                      _              -> fail $ "Could not parse the Buy List in "
                                            ++ "the Pending Orders JSON Object."
                  sellList   = case HM.lookup "Buy" v of
                      Just (Array a) -> case HM.lookup "Info" arrayObj of
                                            Just _  -> return []
                                            Nothing -> parseJSON (Array a)
                                            where Object arrayObj = head $ V.toList a
                      _              -> fail $ "Could not parse the Sell List in "
                                            ++ "the Pending Orders JSON Object."
        parseJSON _          = fail "Did not receive a proper Pending Orders JSON Object."
