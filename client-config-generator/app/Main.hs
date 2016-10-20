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

module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.ByteString.Lazy as BS
import qualified GHC.Generics as GHC
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Typeable
import Data.Char
import System.Environment
import System.Directory
import Control.Monad

type RefreshInterval = Int

type StudentId = Int

data StudentT = StudentT Text.Text Text.Text Text.Text
  deriving (Show, Eq)

parseStudentList :: Text.Text -> [StudentT]
parseStudentList = map makeStudent . map Text.words .
                  --  map (Text.dropWhile (not . isAlpha)) .
                  --  filter (isDigit . Text.head) .
                   filter (not . Text.null) . Text.lines
  where makeStudent [f,l] = StudentT f "" l
        -- makeStudent [f,m,l] = StudentT f m l

makeConfigs :: [StudentT] -> [ClientConfig]
makeConfigs = map makeClientConfig . zip [1..]
  where makeDir studentId  = "code"
        refreshRate        = 5000000
        ignore             = [".", ".."]
        serverHostname     = "ec2-54-213-202-245.us-west-2.compute.amazonaws.com"
        serverPort         = 8083
        makeClientConfig (sid, StudentT l f m) =
          ClientConfig (Student sid f m l) (makeDir sid)
                       refreshRate ignore serverHostname serverPort

data Student = Student { student_id   :: StudentId
                        , first_name :: Text.Text
                        , middle_name :: Text.Text
                        , last_name :: Text.Text
                        } deriving (Eq, Show, GHC.Generic)

instance FromJSON Student
instance ToJSON Student

data ClientConfig = ClientConfig
  { student           :: Student
  , directory         :: FilePath -- ^ path directory to watch
  , refresh_rate      :: RefreshInterval -- ^ refresh rate in seconds
  , ignore            :: [FilePath] -- ^ list of ignored files
  , server_hostname   :: Text.Text
  , server_port       :: Int
  } deriving (Typeable, Show, GHC.Generic)

instance FromJSON ClientConfig
instance ToJSON ClientConfig

main :: IO ()
main = do
  [fname] <- getArgs
  result <- makeConfigs . parseStudentList <$> Text.IO.readFile fname
  setCurrentDirectory "out"
  let dirNames = map (Text.unpack . last_name . student) $ result
      fnames   = repeat "cfg.json"
                --  map (++ ".json")
                --  . map (Text.unpack . last_name) $ result
      files    = map encode result
      exe      = "students-big-brother-client.exe"
      runner   = "run.bat"
  forM_ (zip3 dirNames fnames files) $ \(dir, fname, file) -> do
    createDirectory dir
    createDirectory (dir ++ "/" ++ "code")
    copyFile ("../" ++ exe) (dir ++ "/" ++ exe)
    copyFile ("../" ++ runner) (dir ++ "/" ++ runner)
    BS.writeFile (dir ++ "/" ++ fname) file

  -- let fnames = map ("out/" ++ ) . map (++ ".json")
  --                               . map (Text.unpack . last_name) $ result
  -- mapM_ (uncurry BS.writeFile) $ zip fnames (map encodePretty result)
