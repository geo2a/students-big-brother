{-# LANGUAGE DataKinds      
           , TypeOperators   
           , DeriveGeneric #-}

module API where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified GHC.Generics as GHC (Generic)
import Servant

-- | Text file with source code
data SourceFile = SourceFile FilePath Text 
  deriving (Eq, Show, Read, GHC.Generic)

instance FromJSON SourceFile
instance ToJSON SourceFile

-- | Due to the prototype nature of this app, this type will be used as a 
-- | wrapper for various data until the development of stable API will be 
-- | in progress  
data Thing = Thing
  { files        :: [SourceFile]
  } deriving (Eq, Show, Read, GHC.Generic)

instance FromJSON Thing
instance ToJSON Thing

parseThing :: String -> Thing
parseThing = read

serializeThing :: Thing -> String
serializeThing ts = show ts ++ "\n"

type API = "things" :> 
  (Get '[JSON] [Thing] :<|> ReqBody '[JSON] Thing :> Post '[JSON] ()) 

api :: Proxy API
api = Proxy