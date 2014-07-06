module BXTrader where

-- | API Response Status
data APIStatus = Success String | Info String | Error String deriving (Show)


-- | API EndPoints
data EndPoint  = Depth
               | Ticker
               | Funds
               | Orders
               | Margins
               | BTCAddr
               | SendInstant
               | SendBTC
               | TradeCancel
               | TradeEnter
               | TradeAdvanced
               deriving (Show)


-- | Build the URL for an EndPoint
makeURL :: EndPoint -> String
makeURL e      = "https://campbx.com/apii/" ++ endpoint e ++ ".php"
        where endpoint Depth  = "xdepth"
              endpoint Ticker = "xticker"
        
