{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Import
import           Network.Wai
import           Network.Wai.Handler.Warp
import           OwlCloud
import           Servant

server :: Server AlbumsAPI
server = albums

albumsAPI :: Proxy AlbumsAPI
albumsAPI = Proxy

app :: Application
app = serve albumsAPI server

albums :: Maybe SigninToken -> Maybe SortBy -> EitherT ServantErr IO [Album]
albums mt sortBy = do
    checkValidity mt
    state <- liftIO $ atomically $ readTVar db
    return (albumsList state)

main :: IO ()
main = run 8083 app
