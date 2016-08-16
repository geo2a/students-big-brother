{-# LANGUAGE GeneralizedNewtypeDeriving,
             DeriveDataTypeable,
             DeriveGeneric,
             FlexibleContexts,
             OverloadedStrings,
             ConstraintKinds,
             DataKinds,
             TypeOperators,
             RankNTypes,
             ScopedTypeVariables,
             DuplicateRecordFields #-}

module Client where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Eff hiding ((:>))
import Control.Eff.Lift
import Control.Eff.Reader.Lazy
import Control.Eff.State.Lazy
import Control.Concurrent
import Control.Exception
import qualified GHC.Generics as GHC
import System.Environment
import System.Process
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Typeable
import Data.Aeson
import Data.Monoid
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

data ClientConfig = ClientConfig
  { student           :: Student
  , directory         :: FilePath -- ^ path directory to watch
  , refresh_rate      :: RefreshInterval -- ^ refresh rate in seconds
  , ignore            :: [FilePath] -- ^ list of ignored files
  , server_hostname   :: Text.Text
  , server_port       :: Int
  } deriving (Typeable, Show, GHC.Generic)

type ClientState = [SourceFile]

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

-- | Consult API module (students-big-brother-types/API.hs)
-- | for API documentation
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

---------------------------
-- Git Commands Wrappers --
---------------------------

debugDir = "cd /home/geo2a/Desktop/students/student1 && "

-- gitStatus :: IO String
-- gitStatus = readCreateProcess (shell $ Text.unpack "git status") ""
--
-- gitLog :: IO String
-- gitLog = readCreateProcess (shell "git log") ""
--
-- gitDiff :: IO String
-- gitDiff = readCreateProcess (shell "git diff") ""
--
-- gitCommit :: Student -> Text.Text -> IO String
-- gitCommit student msg =
--   let author = first_name student <> " " <> last_name student <> "dummy@email.com"
--   in readCreateProcess (shell . Text.unpack $
--        "git commit --amend --author=" <> author <> " -m " <> msg) ""

-- data Command a where
--   Init :: Command Text.Text
--   CheckIfDirIsARepo :: Command Bool
--   Status :: Command Text.Text
--   Log :: Command Text.Text
--   Diff :: Command Text.Text
--   Commit :: Student -> Text.Text -> Command Text.Text
--
-- git :: Command a -> IO a
-- git command =
--   let shellCommand = commandToShell command
--   in return False
--   -- in case command of
--   --   CheckIfDirIsARepo -> do
--   --     result <- Text.pack $
--   --       readCreateProcess (shell . Text.unpack $ debugDir <> shellCommand)
--   --     if null result
--   --     then return False
--   --     else return True
--   --   _ -> (readCreateProcess (shell . Text.unpack $ debugDir <> shellCommand)) ""


data Command = Init
            --  | CheckIfDirIsARepo
             | Status
             | Log
             | Diff
             | Commit Student Text.Text

git :: Command -> IO String
git command =
  let shellCommand = commandToShell command
  in case command of
    -- CheckIfDirIsARepo -> return . not . null $
    --   readCreateProcess (shell . Text.unpack $ debugDir <> shellCommand)
    _ -> (readCreateProcess (shell . Text.unpack $ debugDir <> shellCommand)) ""


commandToShell :: Command -> Text.Text
commandToShell Status = "git status"
commandToShell Log    = "git log"
commandToShell Diff   = "git diff"
commandToShell (Commit student msg) =
  let author = first_name student <> " " <>
               last_name student <> "dummy@email.com"
  in "git commit --amend --author=" <> author <> " -m " <> msg
-- commandToShell CheckIfDirIsARepo =
--   "git rev-parse --is-inside-work-tree 2> /dev/null"

------------------------
---- Business Logic ----
------------------------

manager :: Manager
manager = unsafePerformIO $ newManager defaultManagerSettings

-- | Main loop
loop :: DemandedEffects r => Eff r ()
loop = do
  cfg <- ask
  let url = (BaseUrl Http (Text.unpack $ server_hostname cfg)
                          (server_port cfg) "")
  (state :: ClientState) <- get
  dirPath <- lift $ getCurrentDirectory
  currentFilesList <- (\\ ignore cfg) <$>
    (lift $ getDirectoryContents $ dirPath)
  modificationTimes <- lift $ mapM getModificationTime currentFilesList
  let timedCurrentFilesList = zip currentFilesList modificationTimes
  let timedPreviousFilesList = map (\(SourceFile p c t) -> (p, t)) state
  when (not . null $
    timedCurrentFilesList `symmetricDiff` timedPreviousFilesList) $ do
      lift $ putStrLn ("Current files list: " ++ show currentFilesList)
      filesContents <- lift $ mapM Text.IO.readFile currentFilesList
      let newState = zipWith3 SourceFile currentFilesList filesContents modificationTimes
      put newState
      lift $ runExceptT $
        updateFilesList (student_id $ student (cfg :: ClientConfig)) newState manager url
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
  let url = (BaseUrl Http (Text.unpack $ server_hostname cfg)
                          (server_port cfg) "")
  runExceptT $ registerStudent (student (cfg :: ClientConfig)) manager url
  runLift . runState initState . runReader action $ cfg

startClientDaemon :: String -> IO ()
startClientDaemon cfgFileName = do
  cfg <- readClientConfig cfgFileName
  setCurrentDirectory $ directory cfg
  runApp loop (cfg :: ClientConfig) (initState :: ClientState)
  return ()
    where
      initState = []
