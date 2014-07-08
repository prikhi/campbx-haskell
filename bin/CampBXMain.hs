module Main where

import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Conduit

import CampBX

main :: IO ()
main = do
        manager      <- liftIO $ newManager conduitManagerSettings
        auth         <- getAuth
        let anonResp = getResponseBody manager
            authResp = getAuthResponseBody manager auth
        ticker       <- getTicker anonResp
        print ticker
        depth        <- getDepth  anonResp
        print depth
        orders       <- getWallet authResp
        print orders
        addy       <- getDepositAddress authResp
        print addy
