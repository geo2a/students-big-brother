{-# LANGUAGE DataKinds      
           , TypeOperators #-}

module API where

import Servant
import Servant.HTML.Lucid (HTML)

import Types

type API = 
  "files" :> Get '[JSON, HTML] [OwnedSourceFile]
  :<|> 
  "files" :> Capture "userid" UserID :> ReqBody '[JSON] [SourceFile] :> Post '[JSON] ()

--type API = "files" :> Capture "userid" UserID :>  
--  (Get '[JSON] [SourceFile] :<|> ReqBody '[JSON] [SourceFile] :> Post '[JSON] ()) 

api :: Proxy API
api = Proxy