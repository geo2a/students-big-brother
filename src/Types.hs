{-# LANGUAGE DeriveGeneric, 
             FlexibleInstances,
             OverloadedStrings #-}

module Types where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Text (Text, pack)
import qualified GHC.Generics as GHC (Generic)
import Lucid

-- | Text file with source code
data SourceFile = SourceFile { path :: FilePath
                             , contents :: Text
                             } deriving (Eq, Show, Read, GHC.Generic)

instance FromJSON SourceFile
instance ToJSON SourceFile

-- HTML serialization of a single source code file
instance ToHtml SourceFile where
  toHtml file =
    p_ $ do
      h2_ (toHtml $ path file)
      p_ (toHtml $ contents file)

  -- do not worry too much about this
  toHtmlRaw = toHtml

-- HTML serialization of a list of source code files
instance ToHtml [SourceFile] where
  toHtml files =
    foldMap toHtml files

  toHtmlRaw = toHtml

type UserID = Int