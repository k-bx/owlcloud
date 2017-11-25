{-# LANGUAGE OverloadedStrings #-}

module OwlCloud.Common where

import Control.Monad.Catch (throwM)
import Control.Monad.Trans.Class (lift)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import Import
import Network.HTTP.Client (Manager)
import OwlCloud.Types
import Servant
import Servant.Client
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)

-- | Database
data State = State
  { validTokens :: Set SigninToken
  , albumsList :: [Album]
  }

db :: TVar State
db =
  unsafePerformIO
    (unsafeInterleaveIO (newTVarIO (State Set.empty initialAlbums)))
  where
    initialAlbums =
      [ Album
          [ Photo "Scating" "http://i.imgur.com/PuhhmQi.jpg"
          , Photo "Taking shower" "http://i.imgur.com/v5kqUIM.jpg"
          ]
      , Album
          [ Photo "About to fly" "http://i.imgur.com/3hRAGWJ.png"
          , Photo "Selfie" "http://i.imgur.com/ArZrhR6.jpg"
          ]
      ]

{-# NOINLINE db #-}
-- | Request-ready microservices API
-- Users API
apiUsersTokenValidity :: SigninToken -> ClientM TokenValidity
apiUsersOwlIn :<|> apiUsersOwlOut :<|> apiUsersTokenValidity =
  client (Proxy :: Proxy UsersAPI)

usersBaseUrl :: BaseUrl
usersBaseUrl = BaseUrl Http "localhost" 8082 ""

-- Albums API
apiAlbumsList = client (Proxy :: Proxy AlbumsAPI)

albumsBaseUrl = BaseUrl Http "localhost" 8083 ""

-- | Utils
fly :: (Show b) => IO (Either ServantError b) -> Handler b
fly apiReq = do
  res <- liftIO apiReq
  either logAndFail return res
  where
    logAndFail e = do
      liftIO (putStrLn ("Got internal-api error: " ++ show e))
      throwM internalError
    internalError = ServantErr 500 "CyberInternal MicroServer MicroError" "" []

checkValidity :: Manager -> Maybe SigninToken -> Handler ()
checkValidity mgr =
  maybe
    (throwM (ServantErr 400 "Please, provide an authorization token" "" []))
    (\t ->
       fly (runClientM (apiUsersTokenValidity t) (ClientEnv mgr usersBaseUrl)) >>=
       handleValidity)
  where
    handleValidity (TokenValidity True) = return ()
    handleValidity (TokenValidity False) =
      throwM (ServantErr 400 "Your authorization token is invalid" "" [])
