{-# LANGUAGE DataKinds      
           , TypeOperators #-}

module API where

import Servant
import Servant.API.ResponseHeaders
import qualified Data.Text as Text (Text)

import Types

type TeachersAPI = 
  "files" :> BasicAuth "foo-realm" Teacher :> Get '[JSON] [OwnedSourceFile]

type StudentsAPI = 
  "register_student" :> Capture "first_name" Text.Text 
                     :> Capture "last_name" Text.Text
                     :> Post '[JSON] StudentId
  :<|>
  "files" :> Capture "student_id" StudentId 
          :> ReqBody '[JSON] [SourceFile] 
          :> Post '[JSON] () 

type API = 
  TeachersAPI
  :<|> 
  StudentsAPI

api :: Proxy API
api = Proxy