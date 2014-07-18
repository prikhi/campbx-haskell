{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)

import Web.CampBX

main :: IO ()
main = do
        cfg <- defaultCampBXConfig
        runCampBX cfg $ do
            d <- getDepth
            liftIO $ print d
            t <- getTicker
            liftIO $ print t
