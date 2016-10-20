{-# LANGUAGE TypeOperators, DataKinds, DeriveGeneric,
    OverloadedStrings, FlexibleContexts #-}

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
-- import Network.Wai.Logger (withStdoutLogger, ApacheLogger)
import Network.Wai.Middleware.RequestLogger
import Network.Wai
import Servant
import System.IO.Unsafe (unsafePerformIO)
import Data.Default (Default (def))

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
  putStrLn $
    "Students Big Brother Server is listening on port " ++ show (port cfg)
  run (port cfg) (app cfg)

-- note that this is equivalent to Application -> Application
-- logAllMiddleware :: Application -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
-- logAllMiddleware app req respond = do
--     -- print =<< requestBody req
--     -- BS.appendFile "sbb.log" =<< strictRequestBody req
--     app req respond

app :: ServerConfig -> Application
app cfg = cors (const $ Just corsResourcePolicy) $
          -- logAllMiddleware $
          serveWithContext api (basicAuthServerContext cfg) (server cfg)
  where
    corsResourcePolicy = CorsResourcePolicy
      { corsOrigins = Nothing
      , corsMethods = ["GET", "POST"]
      , corsRequestHeaders = [ "Authorization"
                             , "Content-Type"
                             , "Access-Control-Allow-Origin"
                             ]
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
        -- TODO: This function needs heave refactoring (db request optimisation
        -- based on http query params)
        getFiles :: Teacher -> Maybe StudentID ->
                    ReaderT ServerConfig IO [OwnedSourceFile]
        getFiles teacher sid = do
          cfg <- ask
          dbConnection <- liftIO $ dbConnect $ db cfg
          rows <- liftIO $ dbSelectAllFiles dbConnection
          liftIO $ dbDisconnect dbConnection
          case sid of
            Just x -> return . filter (\t -> (student_id . student $ t) == x) $ rows
            Nothing -> return rows

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
    monadNatTransform = Nat $ \r -> liftIO $ runReaderT r cfg


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
