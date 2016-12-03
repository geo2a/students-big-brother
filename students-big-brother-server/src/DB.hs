{-# LANGUAGE OverloadedStrings           
           , FlexibleContexts
           , TemplateHaskell
           , QuasiQuotes
           , DeriveGeneric
           , StandaloneDeriving
           , ScopedTypeVariables
           , DuplicateRecordFields #-}

module DB where

import qualified GHC.Generics as GHC
import qualified Data.Text as Text
import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.SqlQQ
import Data.Time.Clock (UTCTime)

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

dbSelectFiles :: Connection -> TeacherId -> IO [OwnedSourceFile]
dbSelectFiles conn teacher_id = do
  rows <- query conn [sql|
      SELECT file_id, file_path, file_contents, modification_time
           , students.student_id, first_name, middle_name, last_name, teacher_id
      FROM files
          INNER JOIN students ON files.student_id = students.student_id
      WHERE teacher_id = ?;
    |] (Only teacher_id) :: IO [( Integer, FilePath, Text.Text, UTCTime
                                , StudentId, Text.Text, Text.Text
                                , Text.Text, TeacherId)]
  return $ map readRow rows
    where
      readRow (_, path, contents, modification_time
              , student_id, f_name, m_name, l_name, teacher_id) =
        OwnedSourceFile (Student student_id f_name m_name l_name teacher_id)
                        (SourceFile path contents modification_time)

dbSelectStudents :: Connection -> Teacher -> IO [Student]
dbSelectStudents conn (Teacher teacher_id _) = do
  rows <- query conn [sql|
      SELECT * FROM students WHERE teacher_id = ?
    |] (Only teacher_id) -- (Only (2 :: TeacherId)) 
  -- print $ map (\(sid, f, m, l, tid) ->  Student sid f m l tid) rows 
  return $ map (\(sid, f, m, l, tid) ->  Student sid f m l tid) rows

dbInsertFile :: Connection -> StudentId -> SourceFile -> IO ()
dbInsertFile conn student_id (SourceFile path contents modification_time) =
  execute conn [sql|
    INSERT INTO files(file_id, file_path, 
                file_contents, modification_time, student_id) VALUES
      (DEFAULT, ?, ?, ?, ?)
  |] (path, contents, modification_time, student_id) >> return ()

dbUpdateFiles :: Connection -> StudentId -> [SourceFile] -> IO ()
dbUpdateFiles conn student_id files = do
  execute conn [sql|
    DELETE FROM files WHERE student_id = ?
  |] (Only student_id)
  mapM_ (dbInsertFile conn student_id) files

dbAddStudent :: Connection -> Student -> IO Int
dbAddStudent conn (Student sId fname mname lname teacher_id) = do
  -- student_id :: [Only Int] <- query conn [sql|
  student_id <- query conn [sql|
    INSERT INTO students ( student_id, first_name
                         , middle_name, last_name, teacher_id)
      VALUES (?,?,?,?,?) RETURNING student_id
  |] (sId, fname, mname, lname, teacher_id)
  return . fromOnly . head $ student_id

dbAddTeacher :: Connection -> Credential -> IO Int
dbAddTeacher conn (Credential uname pwd) = do
  teacher_id :: [Only Int] <- query conn [sql|
    INSERT INTO users (user_id, username, password, role)
      VALUES (DEFAULT,?,?, 'teacher') RETURNING user_id
  |] (uname, pwd)
  return . fromOnly . head $ teacher_id

dbLookupTeacher :: Connection -> Credential -> IO TeacherId
dbLookupTeacher conn (Credential uname _) = do
  teacher_id <- query conn [sql|
    SELECT user_id FROM users
      WHERE username = ? AND role = 'teacher'
  |] (Only uname)
  case teacher_id of 
    [] -> return (-1)
    _  -> return . fromOnly . head $ teacher_id

dbLookupAdmin :: Connection -> Credential -> IO Bool
dbLookupAdmin conn (Credential uname _) = do
  user_id :: [Only Int] <- query conn [sql|
    SELECT user_id FROM users
      WHERE username = ? AND role = 'admin'
  |] (Only uname)
  return . not . null $ user_id

dbListTeachers :: Connection -> IO [Teacher]
dbListTeachers conn = do
  teachers :: [(Int, Username, Password)] <- query_ conn [sql|
    SELECT user_id, username, password FROM users
      WHERE role = 'teacher'
  |]
  return . map (uncurry Teacher)
         . map (\(a,b,c) -> (a, Credential b c)) $ teachers

dbDeleteTeacher :: Connection -> Int -> IO ()
dbDeleteTeacher conn tid =
  execute conn [sql|
    DELETE FROM users WHERE user_id = ?
  |] (Only tid) >> return ()
