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
  "files" :> Capture "student_id" StudentId
          :> ReqBody '[JSON] [SourceFile]
          :> Post '[JSON] ()

type AdminAPI =
  "register-teacher" :> ReqBody '[JSON] Credential :> Post '[JSON] Teacher

type API =
  TeachersAPI
  :<|>
  StudentsAPI
  :<|>
  AdminAPI

api :: Proxy API
api = Proxy
