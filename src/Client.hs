{-# LANGUAGE GeneralizedNewtypeDeriving,
             DeriveDataTypeable,
             DeriveGeneric,
             FlexibleContexts,
             OverloadedStrings,
             ConstraintKinds,
             DataKinds,
             TypeOperators,
             RankNTypes,
             ScopedTypeVariables #-}

module Client where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Eff hiding ((:>))
import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Control.Eff.State.Lazy
import Control.Concurrent
import qualified GHC.Generics as GHC
import System.Environment
import System.Process
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy as BS
import Data.Time.Clock (UTCTime)
import Data.List
import Data.Typeable
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Servant
import Servant.Client
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import System.IO.Unsafe (unsafePerformIO)

import API
import Types

----------------------
---- Domain Types ----
----------------------

type RefreshInterval = Int

type ModificationTime = UTCTime

data ClientConfig = ClientConfig
  { student_id        :: StudentId
  , first_name        :: Text.Text
  , middle_name       :: Text.Text
  , last_name         :: Text.Text
  , directory         :: FilePath -- ^ path directory to watch
  , refresh_rate      :: RefreshInterval -- ^ refresh rate in seconds
  , ignore            :: [FilePath] -- ^ list of ignored files
  , server_hostname   :: Text.Text
  , server_port       :: Int
  } deriving (Typeable, Show, GHC.Generic)

type ClientState = [(SourceFile, ModificationTime)]

instance FromJSON ClientConfig
instance ToJSON ClientConfig

-- | Type alias for effects set, demanded by application
type DemandedEffects r =
  ( Member (Reader ClientConfig) r
  , Member (State ClientState)   r
  , SetMember Lift (Lift IO)  r
  )

------------------------------------------------------------
---- Auto generated functions to query servers HTTP API ----
------------------------------------------------------------

-- | Consult Server module (Server.hs) for API documentation
-- | TODO: restrict client API, client daemon doesn't need to have access to non-student API
updateFilesList :<|> registerStudent = client students_api

--------------------------------
---- Parsing Configurations ----
--------------------------------

readClientConfig :: FilePath -> IO ClientConfig
readClientConfig fname = do
  contents <- BS.readFile fname
  case eitherDecode contents of
    Left errMsg -> error errMsg
    Right cfg   -> return cfg

------------------------
---- Business Logic ----
------------------------

manager :: Manager
manager = unsafePerformIO $ newManager defaultManagerSettings

-- | Main loop
loop :: DemandedEffects r => Eff r ()
loop = do
  cfg <- ask

  let student = Student (student_id cfg) (first_name cfg)
                        (middle_name cfg) (last_name cfg)
      url = (BaseUrl Http (Text.unpack $ server_hostname cfg)
                          (server_port cfg) "")

  (state :: ClientState) <- get

  dirPath <- lift $ getCurrentDirectory
  currentFilesList <- (\\ ignore cfg) <$>
    (lift $ getDirectoryContents $ dirPath)
  modificationTimes <- lift $ mapM getModificationTime currentFilesList
  let timedCurrentFilesList = zip currentFilesList modificationTimes
  let timedPreviousFilesList = map (\(f, t) -> (path f, t)) state
  when (not . null $
    timedCurrentFilesList `symmetricDiff` timedPreviousFilesList) $ do
      lift $ putStrLn ("Current files list: " ++ show currentFilesList)
      filesContents <- lift $ mapM Text.IO.readFile currentFilesList
      let newState = zipWith SourceFile currentFilesList filesContents
      put (zip newState modificationTimes)
      lift $ runExceptT $
        updateFilesList (student_id cfg) newState manager url
      return ()
  lift $ threadDelay $ refresh_rate cfg
  loop
    where
      symmetricDiff :: Eq a => [a] -> [a] -> [a]
      symmetricDiff xs ys = (xs `union` ys) \\ (xs `intersect` ys)

----------------------------
------ Sevice functions ----
----------------------------

-- | Handles all effects produced by application.
runApp action cfg initState = do
  let student = Student (student_id cfg) (first_name cfg)
                        (middle_name cfg) (last_name cfg)
      url = (BaseUrl Http (Text.unpack $ server_hostname cfg)
                          (server_port cfg) "")

  runExceptT $ registerStudent student manager url

  runLift . runState initState . runReader action $ cfg

startClientDaemon :: String -> IO ()
startClientDaemon cfgFileName = do
  cfg <- readClientConfig cfgFileName
  setCurrentDirectory $ directory cfg
  runApp loop (cfg :: ClientConfig) (initState :: ClientState)
  return ()
    where
      initState = []
