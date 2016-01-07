{-# LANGUAGE TypeOperators   
           , DeriveGeneric   
           , OverloadedStrings
           , FlexibleContexts #-}

module Server where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import qualified GHC.Generics as GHC
import qualified Data.Text as Text
import Data.Aeson
import Network.Wai.Handler.Warp
import Network.Wai 
import Servant

import API

-- | Server configuration 
data ServerConfig = ServerConfig 
  { port :: Int
  , db :: FilePath -- ^ database file
  } deriving (GHC.Generic)

serverCfg :: ServerConfig
serverCfg = ServerConfig {port = 8083, db = "aux/data"}

instance FromJSON ServerConfig
instance ToJSON ServerConfig

startServer :: String -> IO ()
startServer cfgFileName = run (port serverCfg) app

app :: Application
app = serve api server

server :: Server API
server = enter monadNatTransform server'
  where 
    server' :: ServerT API (ReaderT ServerConfig IO)
    server' = getThings :<|> postThing 
      where
        getThings :: ReaderT ServerConfig IO [Thing]
        getThings = do
          cfg <- ask 
          liftIO $ map parseThing . lines <$> readFile (db cfg)
      
        postThing :: Thing -> ReaderT ServerConfig IO ()
        postThing thing = do
          cfg <- ask
          liftIO $ writeFile (db cfg) (serializeThing thing)

    monadNatTransform :: ReaderT ServerConfig IO :~> EitherT ServantErr IO
    monadNatTransform = Nat $ \r -> do
          t <- liftIO $ runReaderT r serverCfg
          return t
