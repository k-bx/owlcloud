{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module OwlCloud.Types where

import           Data.Aeson
import           Import
import           Servant.API

type OwlCloudAPI = UsersAPI :<|> AlbumsAPI

-- * Services

-- ** Users microservice

type UsersAPI =
  "api" :> "users" :> "owl-in" :> ReqBody '[JSON] LoginReq :> Post '[JSON] SigninToken :<|>
  "api" :> "users" :> Authorized ("owl-out" :> Post '[JSON] ()) :<|>
  "private-api" :> "users" :> "token-validity" :> Capture "token" SigninToken :> Get '[JSON] TokenValidity

newtype SigninToken = SigninToken Text
    deriving (ToJSON, FromJSON, FromHttpApiData, ToHttpApiData, Ord, Eq)

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
instance FromHttpApiData SortBy where
    parseQueryParam "whoolest" = Right SortByWhoolest
    parseQueryParam "date" = Right SortByDate
    parseQueryParam x = Left ("Unknown sortby value:" <> x)
instance ToHttpApiData SortBy where
    toQueryParam SortByWhoolest = "whoolest"
    toQueryParam SortByDate = "date"

-- * Common prefixes and headers

type Authorized t = Header "Authorization" SigninToken :> t

type URL = Text
