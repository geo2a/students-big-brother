{-# LANGUAGE TypeOperators   
           , DeriveGeneric   
           , OverloadedStrings #-}

module Server where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import qualified GHC.Generics as GHC
import qualified Data.Text as Text
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import API

-- | App configuration 
data AppCfg = AppCfg 
  { db :: FilePath -- ^ database file
  } deriving (GHC.Generic)

instance FromJSON AppCfg
instance ToJSON AppCfg

startServer :: String -> IO ()
startServer cfgFileName = run 8083 app

app :: Application
app = serve api server

server :: Server API
server = enter monadNatTransform server'
  where 
    server' :: ServerT API (ReaderT FilePath IO)
    server' = getThings :<|> postThing 
      where
        getThings :: ReaderT FilePath IO [Thing]
        getThings = do
          path <- ask 
          liftIO $ map parseThing . lines <$> readFile path
      
        postThing :: Thing -> ReaderT FilePath IO ()
        postThing thing = do
          path <- ask
          liftIO $ writeFile path (serializeThing thing)

    monadNatTransform :: ReaderT FilePath IO :~> EitherT ServantErr IO
    monadNatTransform = Nat $ \r -> do
          t <- liftIO $ runReaderT r "aux/data"
          return t