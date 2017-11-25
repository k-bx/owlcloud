{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Catch (throwM)
import qualified Data.Set as Set
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Import
import Network.Wai
import Network.Wai.Handler.Warp
import OwlCloud
import Servant

server :: Server UsersAPI
server = owlIn :<|> owlOut :<|> tokenValidity

usersAPI :: Proxy UsersAPI
usersAPI = Proxy

app :: Application
app = serve usersAPI server

owlIn :: LoginReq -> Handler SigninToken
owlIn LoginReq {..} =
  case (whoo, passwoord) of
    ("great horned owl", "tiger") -> do
      uuid <- liftIO UUID.nextRandom
      let token = SigninToken (UUID.toText uuid)
      liftIO $
        atomically $
        modifyTVar db $ \s -> s {validTokens = Set.insert token (validTokens s)}
      return token
    _ -> throwM (ServantErr 400 "Username/password pair did not match" "" [])

owlOut :: Maybe SigninToken -> Handler ()
owlOut mt = do
  checkAuth mt
  maybe (return ()) out mt
  where
    out token =
      liftIO $
      atomically $
      modifyTVar db $ \s -> s {validTokens = Set.delete token (validTokens s)}

tokenValidity :: SigninToken -> Handler TokenValidity
tokenValidity token = do
  state <- liftIO $ atomically $ readTVar db
  return (TokenValidity (Set.member token (validTokens state)))

-- Business-logic and utils
checkAuth :: Maybe SigninToken -> Handler ()
checkAuth = maybe unauthorized runCheck
  where
    runCheck (SigninToken token) = do
      state <- liftIO $ atomically $ readTVar db
      let isMember = Set.member (SigninToken token) (validTokens state)
      unless isMember unauthorized
    unauthorized =
      throwM (ServantErr 401 "You are not authenticated. Please sign-in" "" [])

main :: IO ()
main = run 8082 app
