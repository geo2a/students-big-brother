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
import qualified Network.Wai as WAI
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai
import Servant

import Types
import API
import DB

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
app cfg = cors (const $ Just corsResourcePolicy) $
            serveWithContext api (basicAuthServerContext cfg) (server cfg)
  where
    corsResourcePolicy = CorsResourcePolicy
      { corsOrigins = Nothing
      , corsMethods = ["GET", "POST"]
      , corsRequestHeaders = ["Authorization", "Content-Type"]
      , corsExposedHeaders = Nothing
      , corsMaxAge = Nothing
      , corsVaryOrigin = False
      , corsRequireOrigin = False
      , corsIgnoreFailures = False
      }

server :: ServerConfig -> Server API
server cfg = enter monadNatTransform server'
  where
    server' :: ServerT API (ReaderT ServerConfig IO)
    server' = getFiles
              :<|>
              (updateFiles :<|> registerStudent)
              :<|>
              (registerTeacher :<|> listTeachers :<|> deleteTeacher)
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
        updateFiles :: StudentId -> [SourceFile] -> ReaderT ServerConfig IO ()
        updateFiles uid files = do
          cfg <- ask
          dbConnection <- liftIO $ dbConnect $ db cfg
          liftIO $ dbUpdateFiles dbConnection uid files
          liftIO $ dbDisconnect dbConnection

        registerStudent :: Student -> ReaderT ServerConfig IO Student
        registerStudent student = do
          cfg <- ask
          dbConnection <- liftIO $ dbConnect $ db cfg
          teacherId <- liftIO $ dbAddStudent dbConnection student
          liftIO $ dbDisconnect dbConnection
          return student

        registerTeacher :: Admin -> Credential ->
                             ReaderT ServerConfig IO Teacher
        registerTeacher admin crd = do
          cfg <- ask
          dbConnection <- liftIO $ dbConnect $ db cfg
          teacherId <- liftIO $ dbAddTeacher dbConnection crd
          liftIO $ dbDisconnect dbConnection
          return $ Teacher teacherId crd

        listTeachers :: Admin -> ReaderT ServerConfig IO [Teacher]
        listTeachers admin = do
          cfg <- ask
          dbConnection <- liftIO $ dbConnect $ db cfg
          teachers <- liftIO $ dbListTeachers dbConnection
          liftIO $ dbDisconnect dbConnection
          return teachers

        deleteTeacher :: Admin -> Int -> ReaderT ServerConfig IO ()
        deleteTeacher admin tid = do
          cfg <- ask
          dbConnection <- liftIO $ dbConnect $ db cfg
          liftIO $ dbDeleteTeacher dbConnection tid
          liftIO $ dbDisconnect dbConnection

    monadNatTransform :: ReaderT ServerConfig IO :~> ExceptT ServantErr IO
    monadNatTransform = Nat $ \r -> do
          t <- liftIO $ runReaderT r cfg
          return t

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authTeacherCheck :: ServerConfig -> BasicAuthCheck Teacher
authTeacherCheck cfg =
  BasicAuthCheck check
  where
    check (BasicAuthData username password) = do
          let username' = decodeUtf8 username
              password' = decodeUtf8 password
          dbConnection <- liftIO $ dbConnect $ db cfg
          teacherIsPresent <- dbLookupTeacher dbConnection
                                              (Credential username' password')
          liftIO $ dbDisconnect dbConnection
          if teacherIsPresent
          then return (Authorized (Teacher 0
                                  (Credential username'
                                              password')))
          else return Unauthorized

authAdminCheck :: ServerConfig -> BasicAuthCheck Admin
authAdminCheck cfg =
  BasicAuthCheck check
  where
    check (BasicAuthData username password) = do
          let username' = decodeUtf8 username
              password' = decodeUtf8 password
          dbConnection <- liftIO $ dbConnect $ db cfg
          adminIsPresent <- dbLookupAdmin dbConnection
                                              (Credential username' password')
          liftIO $ dbDisconnect dbConnection
          if adminIsPresent
          then return (Authorized (Admin 0
                                  (Credential username'
                                              password')))
          else return Unauthorized


-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: ServerConfig ->
                          Context (BasicAuthCheck Teacher ': BasicAuthCheck Admin ': '[])
basicAuthServerContext cfg = authTeacherCheck cfg :. authAdminCheck cfg :. EmptyContext
