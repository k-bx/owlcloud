{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module OwlCloud.Types where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics
import           Servant.API

type OwlCloudAPI = UsersAPI :<|> AlbumsAPI

-- * Services

-- ** Users microservice

type UsersAPI =
  "api" :> "users" :> "owl-in" :> ReqBody '[JSON] LoginReq :> Post '[JSON] SigninToken :<|>
  "api" :> "users" :> Authorized ("owl-out" :> Post '[JSON] ()) :<|>
  "private-api" :> "users" :> "token-validity" :> Capture "token" SigninToken :> Get '[JSON] TokenValidity

newtype SigninToken = SigninToken Text
    deriving (ToJSON, FromJSON, FromText, ToText, Ord, Eq)

data LoginReq = LoginReq
    { whoo      :: Text
    , passwoord :: Text }
    deriving (Generic)

data TokenValidity = TokenValidity
    { isValid :: Bool }
    deriving (Generic, Show)

instance FromJSON LoginReq
instance ToJSON LoginReq
instance FromJSON TokenValidity
instance ToJSON TokenValidity

-- ** Albums microservice

type AlbumsAPI =
  "api" :> "albooms" :> Authorized (QueryParam "sortby" SortBy :> Get '[JSON] [Album])

data Album = Album [Photo]
    deriving (Generic)

data Photo = Photo
    { description :: Text
    , image       :: URL }
    deriving (Generic)

data SortBy
    = SortByWhoolest
    | SortByDate

instance FromJSON Photo
instance ToJSON Photo
instance FromJSON Album
instance ToJSON Album
instance ToText SortBy where
    toText SortByWhoolest = "whoolest"
    toText SortByDate = "date"
instance FromText SortBy where
    fromText "whoolest" = Just SortByWhoolest
    fromText "date" = Just SortByDate
    fromText _ = Nothing

-- * Common prefixes and headers

type Authorized t = Header "Authorization" SigninToken :> t

type URL = Text
