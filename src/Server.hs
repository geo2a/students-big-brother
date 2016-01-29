{-# LANGUAGE TypeOperators   
           , DeriveGeneric   
           , OverloadedStrings
           , FlexibleContexts
           , TemplateHaskell
           , QuasiQuotes #-}

module Server where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import qualified GHC.Generics as GHC
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Network.Wai.Handler.Warp
import Network.Wai 
import Servant

import API
import DB
import Types

-- | Server configuration 
data ServerConfig = ServerConfig 
  { port :: Int
  , db :: DatabaseConfig -- ^ database connection params
  } deriving (Show, Eq, GHC.Generic)

serverCfg :: ServerConfig
serverCfg = ServerConfig {port = 8083, db = defaultDatabaseConfig}

readServerConfig :: FilePath -> IO ServerConfig
readServerConfig fname = do
  contents <- BS.readFile fname
  case eitherDecode contents of
    Left errMsg -> error errMsg
    Right cfg   -> return cfg

instance FromJSON ServerConfig
instance ToJSON ServerConfig

startServer :: String -> IO ()
startServer cfgFileName = do
  cfg <- readServerConfig cfgFileName
  run (port cfg) app

app :: Application
app = serve api server

server :: Server API
server = enter monadNatTransform server'
  where 
    server' :: ServerT API (ReaderT ServerConfig IO)
    server' = getFiles :<|> updateFilesList
      where
        getFiles :: ReaderT ServerConfig IO [OwnedSourceFile]
        getFiles = do
          cfg <- ask 
          dbConnection <- liftIO $ dbConnect $ db cfg
          rows <- liftIO $ selectAllFiles dbConnection
          liftIO $ dbDisconnect dbConnection
          return rows
        
        -- | Substitute existing files list with given 
        updateFilesList :: UserID -> [SourceFile] -> ReaderT ServerConfig IO ()
        updateFilesList uid files = do
          cfg <- ask
          dbConnection <- liftIO $ dbConnect $ db cfg
          liftIO $ updateClientFiles dbConnection uid files 
          liftIO $ dbDisconnect dbConnection

    monadNatTransform :: ReaderT ServerConfig IO :~> EitherT ServantErr IO
    monadNatTransform = Nat $ \r -> do
          t <- liftIO $ runReaderT r serverCfg
          return t