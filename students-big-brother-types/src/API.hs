{-# LANGUAGE DataKinds
           , TypeOperators #-}

module API where

import Servant
import Servant.API.ResponseHeaders
import qualified Data.Text as Text (Text)

import Types

type TeachersAPI =
  "files" :> QueryParam "s_id" StudentId :> Get '[JSON] [OwnedSourceFile]
  :<|>
  "students" :> Get '[JSON] [Student]

type StudentsAPI =
  "files" :> Capture "student_id" StudentId
          :> ReqBody '[JSON] [SourceFile]
          :> Post '[JSON] ()
  :<|>
  "register-student" :> ReqBody '[JSON] Student
                     :> Post '[JSON] Student

type AdminAPI =
  "admin" :>
    ( BasicAuth "Admins-endpoint" Admin :> "register-teacher"
                                        :> ReqBody '[JSON] Credential
                                        :> Post '[JSON] Teacher
    :<|>
      BasicAuth "Admins-endpoint" Admin :> "list-teachers"
                                        :> Get '[JSON] [Teacher]
    :<|>
      BasicAuth "Admins-endpoint" Admin :> "delete-teacher"
                                        :> ReqBody '[JSON] Int
                                        :> Post '[JSON] ()
    )

type API =
  BasicAuth "Teachers-endpoint" Teacher :> TeachersAPI
  :<|>
  StudentsAPI
  :<|>
  AdminAPI

students_api :: Proxy StudentsAPI
students_api = Proxy

api :: Proxy API
api = Proxy
