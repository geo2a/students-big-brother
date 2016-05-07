{-# LANGUAGE OverloadedStrings
           , FlexibleContexts
           , TemplateHaskell
           , QuasiQuotes
           , DeriveGeneric
           , StandaloneDeriving
           , ScopedTypeVariables #-}

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

dbSelectAllFiles :: Connection -> IO [OwnedSourceFile]
dbSelectAllFiles conn = do
  rows <- query_ conn [sql| select * from files |] ::
              IO [(Integer, ClientID, FilePath, Text.Text)]
  return $ map readRow rows
    where
      readRow (_, cid, path, contents) =
        OwnedSourceFile cid (SourceFile path contents)

dbInsertFile :: Connection -> ClientID -> SourceFile -> IO ()
dbInsertFile conn clientID (SourceFile path contents) =
  execute conn [sql|
    INSERT INTO files(clientID, filePath, fileContents) VALUES
      (?, ?, ?)
  |] (clientID, path, contents) >> return ()

dbUpdateFiles :: Connection -> ClientID -> [SourceFile] -> IO ()
dbUpdateFiles conn clientID files = do
  execute conn [sql|
    DELETE FROM files WHERE clientid = ?
  |] (Only clientID)
  mapM_ (dbInsertFile conn clientID) files

dbAddTeacher :: Connection -> Credential -> IO Int
dbAddTeacher conn (Credential (FullName fn ln) pwd) = do
  teacher_id :: [Only Int] <- query conn [sql|
    INSERT INTO teachers (teacher_id, first_name, last_name, password)
      VALUES (DEFAULT,?,?,?) RETURNING teacher_id
  |] (fn, ln, pwd)
  return . fromOnly . head $ teacher_id
