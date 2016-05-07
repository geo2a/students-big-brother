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

dbConnect :: DatabaseConfig -> IO Connection
dbConnect = connect

dbDisconnect :: Connection -> IO ()
dbDisconnect = close

selectAllFiles :: Connection -> IO [OwnedSourceFile]
selectAllFiles conn = do
  rows <- query_ conn [sql|
    SELECT student_id, file_id, file_path, file_contents FROM files;
  |] :: IO [(StudentId, FileId, FilePath, Text.Text)]
  return $ map readRow rows
    where
      readRow (_, cid, path, contents) =
        OwnedSourceFile cid (SourceFile path contents)

insertFile :: Connection -> StudentId -> SourceFile -> IO ()
insertFile conn student_id (SourceFile path contents) =
  execute conn [sql|
    INSERT INTO files (file_id, file_path, file_contents, student_id) VALUES
                      (DEFAULT, ?, ?, ?)
  |] (path, contents, student_id) >> return ()

-- | Update list of files for a specific student
-- TODO: Needs refactoring, now it just deletes all students files from db and puts new ones, this is unacceptable
updateFiles :: Connection -> StudentId -> [SourceFile] -> IO ()
updateFiles conn student_id files = do
  execute conn [sql|
    DELETE FROM files WHERE student_id = ?
  |] (Only student_id)
  mapM_ (insertFile conn student_id) files

-- | Delete all files of all students, use with care
cleanUp :: Connection -> IO ()
cleanUp conn =
  execute_ conn [sql|
    DELETE FROM files
  |] >> return ()
