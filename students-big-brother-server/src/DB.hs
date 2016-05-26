{-# LANGUAGE OverloadedStrings           , FlexibleContexts
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

type StudentID = Int

dbConnect :: DatabaseConfig -> IO Connection
dbConnect = connect

dbDisconnect :: Connection -> IO ()
dbDisconnect = close

dbSelectAllFiles :: Connection -> IO [OwnedSourceFile]
dbSelectAllFiles conn = do
  rows <- query_ conn [sql|
      select file_id, file_path, file_contents
             , students.student_id, first_name, middle_name, last_name
        from files
          inner join students on files.student_id = students.student_id;
    |] :: IO [( Integer, FilePath, Text.Text
              , StudentID, Text.Text, Text.Text, Text.Text)]
  return $ map readRow rows
    where
      readRow (_, path, contents, student_id, f_name, m_name, l_name) =
        OwnedSourceFile (Student student_id f_name m_name l_name)
                        (SourceFile path contents)

dbInsertFile :: Connection -> StudentID -> SourceFile -> IO ()
dbInsertFile conn student_id (SourceFile path contents) =
  execute conn [sql|
    INSERT INTO files(file_id, file_path, file_contents, student_id) VALUES
      (DEFAULT, ?, ?, ?)
  |] (path, contents, student_id) >> return ()

dbUpdateFiles :: Connection -> StudentID -> [SourceFile] -> IO ()
dbUpdateFiles conn student_id files = do
  execute conn [sql|
    DELETE FROM files WHERE student_id = ?
  |] (Only student_id)
  mapM_ (dbInsertFile conn student_id) files

dbAddStudent :: Connection -> Student -> IO Int
dbAddStudent conn (Student sId fname mname lname) = do
  student_id :: [Only Int] <- query conn [sql|
    INSERT INTO students (student_id, first_name, middle_name, last_name)
      VALUES (?,?,?,?) RETURNING student_id
  |] (sId, fname, mname, lname)
  return . fromOnly . head $ student_id

dbAddTeacher :: Connection -> Credential -> IO Int
dbAddTeacher conn (Credential uname pwd) = do
  teacher_id :: [Only Int] <- query conn [sql|
    INSERT INTO teachers (teacher_id, username, password)
      VALUES (DEFAULT,?,?) RETURNING teacher_id
  |] (uname, pwd)
  return . fromOnly . head $ teacher_id

dbLookupTeacher :: Connection -> Credential -> IO Bool
dbLookupTeacher conn (Credential uname pwd) = do
  student_id :: [Only Int] <- query conn [sql|
    SELECT teacher_id FROM teachers WHERE username = ? AND password = ?
  |] (uname, pwd)
  return . not . null $ student_id

dbListTeachers :: Connection -> IO [Teacher]
dbListTeachers conn = do
  teachers :: [(Int, Username, Password)] <- query_ conn [sql|
    SELECT * FROM teachers
  |]
  return . map (uncurry Teacher)
         . map (\(a,b,c) -> (a, Credential b c)) $ teachers

dbDeleteTeacher :: Connection -> Int -> IO ()
dbDeleteTeacher conn tid =
  execute conn [sql|
    DELETE FROM teachers WHERE teacher_id = ?
  |] (Only tid) >> return ()
