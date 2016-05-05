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
    SELECT students.student_id, file_id, file_path, file_contents FROM 
      students INNER JOIN files ON students.student_id = files.student_id; 
  |] :: IO [(Integer, StudentId, FilePath, Text.Text)]
  return $ map readRow rows
    where 
      readRow (_, cid, path, contents) = 
        OwnedSourceFile cid (SourceFile path contents)

lookupStudent :: Connection -> Text.Text -> Text.Text -> IO Bool
lookupStudent conn first_name last_name = do
  rows <- query conn [sql|
    SELECT student_id FROM students WHERE first_name = ? AND last_name = ?
  |] (first_name, last_name) :: IO [Only StudentId]
  return . not . null $ rows 

insertStudent :: Connection -> Text.Text -> Text.Text -> IO StudentId
insertStudent conn first_name last_name = do
  alreadyThere <- lookupStudent conn first_name last_name
  if alreadyThere 
  then return (-1) 
  else do
    head <$> returning conn [sql| 
      INSERT INTO students (first_name, last_name) VALUES
                     (?, ?) RETURNING student_id;
    |] [(first_name, last_name)] :: IO [StudentId]

insertFile :: Connection -> StudentId -> SourceFile -> IO ()
insertFile conn student_id (SourceFile path contents) =
  execute conn [sql| 
    INSERT INTO files (file_id, file_path, file_contents, student_id) VALUES
                      (DEFAULT, ?, ?, ?)
  |] (path, contents, student_id) >> return ()

updateClientFiles :: Connection -> StudentId -> [SourceFile] -> IO ()
updateClientFiles conn student_id files = do
  execute conn [sql| 
    DELETE FROM files WHERE student_id = ?
  |] (Only student_id)
  mapM_ (insertFile conn student_id) files 
