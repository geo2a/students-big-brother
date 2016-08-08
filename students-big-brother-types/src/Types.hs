{-# LANGUAGE DeriveGeneric,
             DeriveFunctor,
             FlexibleInstances,
             TypeFamilies,
             DataKinds,
             OverloadedStrings #-}

module Types where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Text (Text, pack)
import qualified GHC.Generics as GHC (Generic)
import Data.Typeable (Typeable)
import Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import Data.Time.Clock (UTCTime)

type StudentId = Int

data Student = Student { s_id   :: StudentId
                       , f_name :: Text
                       , m_name :: Text
                       , l_name :: Text
                       } deriving (Eq, Show, GHC.Generic)

instance FromJSON Student
instance ToJSON Student

type FileId    = Int

-- data FullName = FullName { firstName :: Text
--                          , lastName :: Text
--                          } deriving (Eq, Show, GHC.Generic)

-- instance FromJSON FullName
-- instance ToJSON FullName

type Password = Text

type Username = Text

data Credential = Credential { username :: Username
                             , password :: Password
                             } deriving (Eq, Show, GHC.Generic)

instance FromJSON Credential
instance ToJSON Credential

type ModificationTime = UTCTime

-- | Text file with source code
data SourceFile = SourceFile { path :: FilePath
                             , contents :: Text
                             , modificationTime :: ModificationTime
                             } deriving (Eq, Show, GHC.Generic)

instance FromJSON SourceFile
instance ToJSON SourceFile

-- | SourceFile with assigned user id
data OwnedSourceFile = OwnedSourceFile { student :: Student
                                       , file :: SourceFile
                                       } deriving (Eq, Show, GHC.Generic)

instance FromJSON OwnedSourceFile
instance ToJSON OwnedSourceFile

data Teacher = Teacher { teacher_id :: Int
                       , teacher_credential :: Credential
                       }
  deriving (Eq, Show, GHC.Generic)

instance FromJSON Teacher
instance ToJSON Teacher

data Admin = Admin { admin_id :: Int
                   , admin_credential :: Credential
                   }
  deriving (Eq, Show, GHC.Generic)

instance FromJSON Admin
instance ToJSON Admin
