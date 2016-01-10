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

data ClientConfig = ClientConfig 
  { userID            :: UserID
  , directory         :: FilePath -- ^ path directory to watch 
  , refreshRate       :: RefreshInterval -- ^ refresh rate in seconds 
  , ignore            :: [FilePath] -- ^ list of ignored files
  } deriving (Typeable, Show, GHC.Generic)

instance FromJSON ClientConfig
instance ToJSON ClientConfig

-- | Type alias for effects set, demanded by application
type DemandedEffects r = 
  ( Member (Reader ClientConfig) r
  , Member (State [SourceFile])   r 
  , SetMember Lift (Lift IO)  r
  )

------------------------------------------------------------
---- Auto generated functions to query servers HTTP API ----
------------------------------------------------------------

getFiles :<|> postFiles = client api (BaseUrl Http "localhost" 8083)

--------------------------------
---- Parsing Configurations ----
--------------------------------

readConfig :: FilePath -> IO ClientConfig
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
  (state :: [SourceFile]) <- get  
  
  currentFilesList <- (\\ ignore cfg) <$> (lift $ getDirectoryContents $ directory cfg) 
  let previousFilesList = map path state
  when (not . null $ currentFilesList `symmetricDiff` previousFilesList) $ do
      lift $ putStrLn ("Current files list: " ++ show currentFilesList)
      filesContents <- lift $ mapM Text.IO.readFile currentFilesList
      let newState = zipWith SourceFile currentFilesList filesContents
      put newState
      lift $ runEitherT $ postFiles (userID cfg) newState -- send files to server
      return ()
  lift $ threadDelay $ refreshRate cfg 
  loop
    where 
      symmetricDiff :: Eq a => [a] -> [a] -> [a]
      symmetricDiff xs ys = (xs `union` ys) \\ (xs `intersect` ys)
-----------------------------
---- Loop Event handlers ----
-----------------------------

-- | Handles all effects produced by application. 
runApp action cfg initState = 
  runLift . runState initState . runReader action $ cfg

startClientDaemon :: String -> IO ()
startClientDaemon cfgFileName = do  
  cfg <- readConfig cfgFileName
  setCurrentDirectory $ directory cfg 
  runApp loop (cfg :: ClientConfig) (initState :: [SourceFile])
  return ()
    where
      initState = []