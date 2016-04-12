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

type UserID = Int

--instance ToHtml UserID where
--  toHtml = toHtml . show

-- | Text file with source code
data SourceFile = SourceFile { path :: FilePath
                             , contents :: Text
                             } deriving (Eq, Show, GHC.Generic)

instance FromJSON SourceFile
instance ToJSON SourceFile

---- HTML serialization of a single source code file
--instance ToHtml SourceFile where
--  toHtml file =
--    p_ $ do
--      h2_ (toHtml $ path file)
--      p_ (toHtml $ contents file)

--  -- do not worry too much about this
--  toHtmlRaw = toHtml

---- HTML serialization of a list of source code files
--instance ToHtml [SourceFile] where
--  toHtml files =
--    foldMap toHtml files

--  toHtmlRaw = toHtml

-- | SourceFile with assigned user id
data OwnedSourceFile = OwnedSourceFile { uid :: UserID
                                       , file :: SourceFile
                                       } deriving (Eq, Show, GHC.Generic)

instance FromJSON OwnedSourceFile
instance ToJSON OwnedSourceFile

-- | A user we'll grab from the database when we authenticate someone
newtype Teacher = Teacher { teacherName :: Text }
  deriving (Eq, Show)

---- | The result of authentication/authorization
--data BasicAuthResult usr
--  = Unauthorized
--  | BadPassword
--  | NoSuchUser
--  | Authorized usr
--  deriving (Eq, Show, Read, GHC.Generic, Typeable, Functor)

---- | Datatype wrapping a function used to check authentication.
--newtype BasicAuthCheck usr = BasicAuthCheck
--  { unBasicAuthCheck :: BasicAuthData
--                     -> IO (BasicAuthResult usr)
--  }
--  deriving (GHC.Generic, Typeable, Functor)

---- HTML serialization of a single source code file
--instance ToHtml OwnedSourceFile where
--  toHtml x =
--    tr_ $ do
--      td_ (toHtml $ uid x)
--      td_ (toHtml $ file x)

--  -- do not worry too much about this
--  toHtmlRaw = toHtml

---- HTML serialization of a list of source code files
--instance ToHtml [OwnedSourceFile] where
--  toHtml files =
--    foldMap toHtml files

--  toHtmlRaw = toHtml