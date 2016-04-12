{-# LANGUAGE DataKinds      
           , TypeOperators #-}

module API where

import Servant
import Servant.API.ResponseHeaders

import Types

type TeachersAPI = 
  "files" :> BasicAuth "foo-realm" Teacher :> Get '[JSON] [OwnedSourceFile]

type StudentsDaemonAPI = "files" :> 
  Capture "userid" UserID :> ReqBody '[JSON] [SourceFile] :> Post '[JSON] () 

type API = 
  TeachersAPI
  :<|> 
  StudentsDaemonAPI

--type API = "files" :> Capture "userid" UserID :>  
--  (Get '[JSON] [SourceFile] :<|> ReqBody '[JSON] [SourceFile] :> Post '[JSON] ()) 

api :: Proxy API
api = Proxy