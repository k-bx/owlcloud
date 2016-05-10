{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Import
import           Network.HTTP.Client      (Manager, defaultManagerSettings,
                                           newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           OwlCloud
import           Servant

server :: Manager -> Server AlbumsAPI
server = albums

albumsAPI :: Proxy AlbumsAPI
albumsAPI = Proxy

app :: Manager -> Application
app mgr = serve albumsAPI (server mgr)

albums :: Manager -> Maybe SigninToken -> Maybe SortBy -> ExceptT ServantErr IO [Album]
albums mgr mt sortBy = do
    checkValidity mgr mt
    state <- liftIO $ atomically $ readTVar db
    return (albumsList state)

main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    run 8083 (app mgr)
