{-# LANGUAGE DataKinds      
           , TypeOperators   
           , DeriveGeneric   
           , RankNTypes   
           , OverloadedStrings #-}

module Lib where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import qualified GHC.Generics as GHC
import qualified Data.Text as Text
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

-- | Text file with source code
data SourceFile = SourceFile FilePath Text.Text 
  deriving (Eq, Show, Read, GHC.Generic)

instance FromJSON SourceFile
instance ToJSON SourceFile

data Thing = Thing
  { files        :: [SourceFile]
  } deriving (Eq, Show, Read, GHC.Generic)

instance FromJSON Thing
instance ToJSON Thing

parseThing :: String -> Thing
parseThing = read
serializeThing :: Thing -> String
serializeThing ts = show ts ++ "\n"

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