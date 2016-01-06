{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes   #-}

module Lib where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import qualified GHC.Generics as GHC
import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

-- | App configuration 
data AppCfg = AppCfg 
  { db :: FilePath -- ^ database file
  } deriving (GHC.Generic)

instance FromJSON AppCfg
instance ToJSON AppCfg

data Thing = Thing
  { files        :: [FilePath]
  } deriving (Eq, Show, GHC.Generic)

parseThing :: String -> Thing
parseThing inp = let [tid] = words inp 
                in Thing (read tid)

serializeThing :: Thing -> String
serializeThing (Thing tid) = show tid ++ "\n"

instance FromJSON Thing
instance ToJSON Thing

type API = "things" :> 
  (Get '[JSON] [Thing] :<|> ReqBody '[JSON] Thing :> Post '[JSON] ()) 

startApp :: IO ()
startApp = run 8083 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

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
          liftIO $ appendFile path (serializeThing thing)

    monadNatTransform :: ReaderT FilePath IO :~> EitherT ServantErr IO
    monadNatTransform = Nat $ \r -> do
          t <- liftIO $ runReaderT r "aux/data"
          return t