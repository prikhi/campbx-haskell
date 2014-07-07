{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module CampBX.Types where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HM
import GHC.Generics

-- | A Quantity of Bitcoins
type BTCAmount = Double
-- | A Quantity of USD
type USDAmount = Double
-- | The Price in USD of 1 Bitcoin
type BTCPrice  = Double

-- | The Price and Quantity of a BTC Buy Offer
newtype Bid    = Bid (BTCPrice, BTCAmount) deriving (Show, Generic)
-- | The Price and Quantity of a BTC Sell Offer
newtype Ask    = Ask (BTCPrice, BTCAmount) deriving (Show, Generic)


-- | Orders

data OrderType = Sell
               | QuickSell
               | Short
               | Buy
               | QuickBuy
               | Cover deriving (Show, Generic)
data PriceType = Market
               | Limit
               | StopLimit deriving (Show, Generic)
data FillType  = Incr
               | FOK
               | AON deriving (Show, Generic)
data Margin    = Int | None deriving (Show, Generic)
data DarkPool  = Yes | No deriving (Show, Generic)

data Order     = Order OrderType PriceType FillType Margin DarkPool
               deriving (Show, Generic)



-- | The List of Current Asks and Bids
data Depth     = Depth      { asks :: [Ask] , bids :: [Bid] } deriving (Show, Generic)

-- | The Lowest Ask and Highest Bid
data Ticker    = Ticker    { bestAsk   :: BTCAmount
                           , bestBid   :: BTCAmount
                           , lastTrade :: BTCAmount } deriving (Show, Generic)

-- | A User's Wallet Contains Liquid and Marginal USD/BTC
data Wallet    = Wallet    { totalUSD  :: USDAmount
                           , totalBTC  :: BTCAmount
                           , liquidUSD :: USDAmount
                           , liquidBTC :: BTCAmount
                           , marginUSD :: USDAmount
                           , marginBTC :: BTCAmount } deriving (Show, Generic)

data OrderList = OrderList { buys      :: [Order]
                           , sells     :: [Order] } deriving (Show, Generic)


instance FromJSON Bid
instance FromJSON Ask

instance FromJSON Depth where
        parseJSON (Object v) =
            case HM.lookup "Asks" v  of
               (Just as) -> case HM.lookup "Bids" v of
                               (Just bs) -> Depth <$> parseJSON as
                                                  <*> parseJSON bs
                               _         -> fail "Could not parse Depth Bids"
               _ -> fail "Could not parse Depth Asks"
        parseJSON _          = fail "Did not receive Depth JSON object."

instance FromJSON Ticker where
        parseJSON (Object v) = do
            bid   <- fmap read $ v .: "Best Bid"
            ask   <- fmap read $ v .: "Best Ask"
            trade <- fmap read $ v .: "Last Trade"
            return $ Ticker ask bid trade
        parseJSON _          = fail "Did not receive Ticker JSON object."

instance FromJSON Wallet
instance FromJSON OrderType
instance FromJSON PriceType
instance FromJSON FillType
instance FromJSON Margin
instance FromJSON DarkPool
instance FromJSON Order
instance FromJSON OrderList




-- | API Authentication
type Auth = (B.ByteString, B.ByteString)
makeAuth :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
makeAuth = (,)

-- | API EndPoints
data EndPoint  = GetDepth
               | GetTicker
               | GetFunds
               | GetOrders
               | GetMargins
               | GetBTCAddr
               | SendInstant
               | SendBTC
               | TradeCancel
               | TradeEnter
               | TradeAdvanced
               deriving (Show)

-- | API EndPoint to Response Dispatch Function
type BXDispatch = (EndPoint -> IO L.ByteString)

-- | API Response Statuses
data APIStatus = Success String | Info String | Error String deriving (Show)
