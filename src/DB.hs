{-# LANGUAGE OverloadedStrings
           , FlexibleContexts
           , TemplateHaskell
           , QuasiQuotes
           , DeriveGeneric
           , StandaloneDeriving#-}

module DB where

import qualified GHC.Generics as GHC
import qualified Data.Text as Text
import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.SqlQQ

import API
import Types

--------------------------------------------
---- Types and instances for PostgreSQL ----
--------------------------------------------

deriving instance GHC.Generic ConnectInfo

instance ToJSON ConnectInfo
instance FromJSON ConnectInfo

type DatabaseConfig = ConnectInfo

type ClientID = Int

dbConnect :: DatabaseConfig -> IO Connection
dbConnect = connect 

dbDisconnect :: Connection -> IO ()
dbDisconnect = close 

selectAllFiles :: Connection -> IO [OwnedSourceFile] 
selectAllFiles conn = do
  rows <- query_ conn [sql| select * from files |] :: 
              IO [(Integer, ClientID, FilePath, Text.Text)]
  return $ map readRow rows
    where 
      readRow (_, cid, path, contents) = 
        OwnedSourceFile cid (SourceFile path contents)

insertFile :: Connection -> ClientID -> SourceFile -> IO ()
insertFile conn clientID (SourceFile path contents) =
  execute conn [sql| 
    INSERT INTO files(clientID, filePath, fileContents) VALUES 
      (?, ?, ?)
  |] (clientID, path, contents) >> return ()

updateClientFiles :: Connection -> ClientID -> [SourceFile] -> IO ()
updateClientFiles conn clientID files = do
  execute conn [sql| 
    DELETE FROM files WHERE clientid = ?
  |] (Only clientID)
  mapM_ (insertFile conn clientID) files 
  

