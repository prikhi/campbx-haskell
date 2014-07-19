module Main where

import Control.Applicative    ((<$>))
import Control.Monad.IO.Class (liftIO)

import Web.CampBX


main :: IO ()
main = do
        cfg <- defaultCampBXConfig
        _   <- runCampBX cfg $ do
            d <- getDepth
            liftIO $ print d
            totalAskVolume <- calculateAskVolume <$> getDepth
            liftIO . putStrLn $ "Total Ask Volume: " ++ show totalAskVolume
            getTicker >>= liftIO . print
        return ()


calculateAskVolume :: Depth -> BTCAmount
calculateAskVolume depthList = sum . map askAmount . asks $ depthList
