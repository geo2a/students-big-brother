{-# LANGUAGE GeneralizedNewtypeDeriving,
             DeriveDataTypeable,
             DeriveGeneric,
             FlexibleContexts,
             OverloadedStrings,
             ConstraintKinds,
             DataKinds,
             TypeOperators,
             RankNTypes #-}

module Client where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Either
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
import Data.Time.Clock
import Data.List
import Data.Typeable
import Data.Aeson
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Servant
import Servant.Client

import API

----------------------
---- Domain Types ----
----------------------

type RefreshInterval = Int

data AppConfig = AppConfig 
  { directory         :: FilePath -- ^ path directory to watch 
  , refreshRate       :: RefreshInterval -- ^ refresh rate in seconds 
  , ignore            :: [FilePath] -- ^ list of ignored files
  } deriving (Typeable, Show, GHC.Generic)

instance FromJSON AppConfig
instance ToJSON AppConfig

--instance FromJSON Thing
--instance ToJSON Thing

-- | Type alias for effects set, demanded by application
type DemandedEffects r = 
  ( Member (Reader AppConfig) r
  , Member (State Thing)   r 
  , SetMember Lift (Lift IO)  r
  )

------------------------------------------------------------
---- Auto generated functions to query servers HTTP API ----
------------------------------------------------------------

getThings :<|> postThings = client api (BaseUrl Http "localhost" 8083)

--------------------------------
---- Parsing Configurations ----
--------------------------------

readConfig :: FilePath -> IO AppConfig
readConfig fname = do
  contents <- BS.readFile fname
  case eitherDecode contents of 
    Left errMsg -> error errMsg
    Right cfg   -> return cfg 

------------------------
---- Business Logic ----
------------------------

-- | Main loop
loop :: DemandedEffects r => Eff r ()
loop = do
  cfg <- ask
  st@(Thing prevFilesList) <- get  
  currentFilesList <- lift $ getDirectoryContents $ directory cfg
  let currentFilesList' = currentFilesList \\ ignore cfg
  filesContents <- lift $ mapM Text.IO.readFile currentFilesList'
  put $ Thing (zipWith SourceFile currentFilesList' filesContents)
  lift $ print currentFilesList'
  lift $ runEitherT $ postThings st
  lift $ threadDelay $ refreshRate cfg
  loop

-----------------------------
---- Loop Event handlers ----
-----------------------------

-- | Handles all effects produced by application. 
runApp action cfg initState = 
  runLift . runState initState . runReader action $ cfg

startClientDaemon :: String -> IO ()
startClientDaemon cfgFileName = do  
  --cfg <- readConfig cfgFileName
  let cfg = AppConfig "/home/geo2a/Desktop/t" 1000000 [".", ".."]
  setCurrentDirectory $ directory cfg 
  runApp loop cfg (Thing [])
  return ()
    where
      greetings =
        putStrLn "File Trigger, v0.1"
