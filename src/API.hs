{-# LANGUAGE DataKinds      
           , TypeOperators   
           , DeriveGeneric #-}

module API where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Text (Text, pack)
import qualified GHC.Generics as GHC (Generic)
import Servant

-- | Text file with source code
data SourceFile = SourceFile { path :: FilePath
                             , contents :: Text
                             } deriving (Eq, Show, Read, GHC.Generic)

instance FromJSON SourceFile
instance ToJSON SourceFile

type UserID = Int

type API = 
  "files" :> Capture "userid" UserID :> Get '[JSON] [SourceFile]
  :<|> 
  "files" :> Capture "userid" UserID :> ReqBody '[JSON] [SourceFile] :> Post '[JSON] ()

--type API = "files" :> Capture "userid" UserID :>  
--  (Get '[JSON] [SourceFile] :<|> ReqBody '[JSON] [SourceFile] :> Post '[JSON] ()) 

api :: Proxy API
api = Proxy