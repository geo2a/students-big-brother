{-# LANGUAGE TypeOperators
           , DataKinds
           , DeriveGeneric
           , OverloadedStrings
           , FlexibleContexts
           , TemplateHaskell
           , QuasiQuotes #-}

module Server where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader
import qualified GHC.Generics as GHC
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Data.Text.Encoding (decodeUtf8)
import Network.Wai.Handler.Warp
import Network.Wai
import Network.Wai.Middleware.Cors
import Servant

import API
import DB
import Types

-- | Server configuration
data ServerConfig = ServerConfig
  { port :: Int
  , db :: DatabaseConfig -- ^ database connection params
  } deriving (Show, Eq, GHC.Generic)

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
  run (port cfg) (app cfg)

app :: ServerConfig -> Application
app cfg = simpleCors $ serveWithContext api basicAuthServerContext (server cfg)

server :: ServerConfig -> Server API
server cfg = enter monadNatTransform server'
  where
    server' :: ServerT API (ReaderT ServerConfig IO)
    server' = getFiles :<|> updateFiles :<|> registerTeacher
      where
        -- | Obtain all files of all students
        getFiles :: Teacher -> ReaderT ServerConfig IO [OwnedSourceFile]
        getFiles teacher = do
          cfg <- ask
          dbConnection <- liftIO $ dbConnect $ db cfg
          rows <- liftIO $ dbSelectAllFiles dbConnection
          liftIO $ dbDisconnect dbConnection
          return rows

        -- | Substitute existing files list with given
        updateFiles :: StudentId ->
                           [SourceFile] -> ReaderT ServerConfig IO ()
        updateFiles uid files = do
          cfg <- ask
          dbConnection <- liftIO $ dbConnect $ db cfg
          liftIO $ dbUpdateFiles dbConnection uid files
          liftIO $ dbDisconnect dbConnection

        registerTeacher :: Credential -> ReaderT ServerConfig IO Teacher
        registerTeacher crd = do
          dbConnection <- liftIO $ dbConnect $ db cfg
          teacherId <- liftIO $ dbAddTeacher dbConnection crd
          liftIO $ dbDisconnect dbConnection
          return $ Teacher teacherId crd

    monadNatTransform :: ReaderT ServerConfig IO :~> ExceptT ServantErr IO
    monadNatTransform = Nat $ \r -> do
          t <- liftIO $ runReaderT r cfg
          return t

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: BasicAuthCheck Teacher
authCheck =
  let check (BasicAuthData username password) =
        if username == "teacher" && password == "teacher"
        then return (Authorized (Teacher 0
                                (Credential (FullName (decodeUtf8 username)
                                                      (decodeUtf8 username))
                                                        (decodeUtf8 password))))
        else return Unauthorized
  in BasicAuthCheck check

-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: Context (BasicAuthCheck Teacher ': '[])
basicAuthServerContext = authCheck :. EmptyContext
