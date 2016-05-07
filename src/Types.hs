{-# LANGUAGE DeriveGeneric,
             DeriveFunctor,
             FlexibleInstances,
             OverloadedStrings #-}

module Types where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Text (Text, pack)
import qualified GHC.Generics as GHC (Generic)
import Data.Typeable (Typeable)
import Servant.API.BasicAuth (BasicAuthData (BasicAuthData))

type StudentId = Int

type FileId    = Int

data FullName = FullName { firstName :: Text
                         , lastName :: Text
                         } deriving (Eq, Show, GHC.Generic)

instance FromJSON FullName
instance ToJSON FullName

type Password = Text

data Credential = Credential { name :: FullName
                             , password :: Password
                             } deriving (Eq, Show, GHC.Generic)

instance FromJSON Credential
instance ToJSON Credential

-- | Text file with source code
data SourceFile = SourceFile { path :: FilePath
                             , contents :: Text
                             } deriving (Eq, Show, GHC.Generic)

instance FromJSON SourceFile
instance ToJSON SourceFile

-- | SourceFile with assigned user id
data OwnedSourceFile = OwnedSourceFile { uid :: StudentId
                                       , file :: SourceFile
                                       } deriving (Eq, Show, GHC.Generic)

instance FromJSON OwnedSourceFile
instance ToJSON OwnedSourceFile

-- | A user we'll grab from the database when we authenticate someone
data Teacher = Teacher { teacher_id :: Int
                       , teacher_credential :: Credential
                       }
  deriving (Eq, Show, GHC.Generic)

instance FromJSON Teacher
instance ToJSON Teacher
