{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module CampBX.Types where

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
data APIStatus a = Success a | Info a | Error a deriving (Show, Generic)


-- | A Bitcoin Wallet Address
type BTCAddress = String
-- | A Quantity of Bitcoins
type BTCAmount  = Double
-- | A Quantity of USD
type USDAmount  = Double
-- | The Price in USD of 1 Bitcoin
type BTCPrice   = Double

-- | The Price and Quantity of a BTC Buy Offer
newtype Bid     = Bid (BTCPrice, BTCAmount) deriving (Show, Generic)
-- | The Price and Quantity of a BTC Sell Offer
newtype Ask     = Ask (BTCPrice, BTCAmount) deriving (Show, Generic)


-- Orders
data OrderType  = Sell | QuickSell | Short | Buy | QuickBuy | Cover
                deriving (Show, Generic)
data PriceType  = Market | Limit | StopLimit
                deriving (Show, Generic)
data FillType   = Incr | FOK | AON
                deriving (Show, Generic)
data Margin     = Int | None
                deriving (Show, Generic)
data DarkPool   = Yes | No
                deriving (Show, Generic)

data Order      = Order OrderType PriceType FillType Margin DarkPool
                deriving (Show, Generic)



-- | The List of Current Asks and Bids
data Depth      = Depth     { asks :: [Ask] , bids :: [Bid] } deriving (Show, Generic)

-- | The Lowest Ask and Highest Bid
data Ticker     = Ticker    { bestAsk   :: BTCAmount
                            , bestBid   :: BTCAmount
                            , lastTrade :: BTCAmount } deriving (Show, Generic)

-- | A User's Wallet Contains Liquid and Marginal USD/BTC
data Wallet     = Wallet    { totalUSD  :: USDAmount
                            , totalBTC  :: BTCAmount
                            , liquidUSD :: USDAmount
                            , liquidBTC :: BTCAmount
                            , marginUSD :: USDAmount
                            , marginBTC :: BTCAmount } deriving (Show, Generic)

-- | A List of the User's Pending Buy and Sell Orders
data OrderList  = OrderList { buys    :: [Order]
                            , sells   :: [Order] }
                            deriving (Show, Generic)

data DepositAddress   = DepositAddress { btcAddress :: String
                                       , expireDate :: Integer } deriving (Show, Generic)

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
            bid   <- fRead $ v .: "Best Bid"
            ask   <- fRead $ v .: "Best Ask"
            trade <- fRead $ v .: "Last Trade"
            return $ Ticker ask bid trade
            where fRead = fmap read
        parseJSON _          = fail "Did not receive a valid Ticker JSON object."

instance FromJSON Depth where
        parseJSON (Object v) =
            case HM.lookup "Asks" v  of
               (Just as) -> case HM.lookup "Bids" v of
                               (Just bs) -> Depth <$> parseJSON as
                                                  <*> parseJSON bs
                               _         -> fail "Could not parse Depth Bids"
               _ -> fail "Could not parse Depth Asks"
        parseJSON _          = fail "Did not receive a valid Depth JSON object."

instance FromJSON Wallet where
        parseJSON (Object v) = do
            totalUsd  <- fRead $ v .: "Total USD"
            totalBtc  <- fRead $ v .: "Total BTC"
            liquidUsd <- fRead $ v .: "Liquid USD"
            liquidBtc <- fRead $ v .: "Liquid BTC"
            marginUsd <- fRead $ v .: "Margin Account USD"
            marginBtc <- fRead $ v .: "Margin Account BTC"
            return $ Wallet totalUsd totalBtc liquidUsd liquidBtc marginUsd marginBtc
            where fRead = fmap read
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
                                        ++     "the Pending Orders JSON Object."
                  sellList   = case HM.lookup "Buy" v of
                      Just (Array a) -> case HM.lookup "Info" arrayObj of
                                     Just _  -> return []
                                     Nothing -> parseJSON (Array a)
                                     where Object arrayObj = head $ V.toList a
                      _              -> fail $ "Could not parse the Sell List in "
                                        ++     "the Pending Orders JSON Object."
        parseJSON _          = fail "Did not receive a proper Pending Orders JSON Object."
