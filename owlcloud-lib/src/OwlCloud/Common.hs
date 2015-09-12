{-# LANGUAGE OverloadedStrings #-}

module OwlCloud.Common where

import           Control.Monad    (liftM)
import           Data.Proxy
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Import
import           OwlCloud.Types
import           Servant
import           Servant.Client
import           System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)

-- | Database

data State = State
    { validTokens :: Set SigninToken
    , albumsList  :: [Album] }

db :: TVar State
db = unsafePerformIO (unsafeInterleaveIO (newTVarIO (State Set.empty initialAlbums)))
  where
    initialAlbums = [Album [Photo "Scating" "http://i.imgur.com/PuhhmQi.jpg"
                           ,Photo "Taking shower" "http://i.imgur.com/v5kqUIM.jpg"]
                    ,Album [Photo "About to fly" "http://i.imgur.com/3hRAGWJ.png"
                           ,Photo "Selfie" "http://i.imgur.com/ArZrhR6.jpg"]]
{-# NOINLINE db #-}

-- | Request-ready microservices API

-- Users API

apiUsersOwlIn :<|> apiUsersOwlOut :<|> apiUsersTokenValidity =
    client (Proxy::Proxy UsersAPI) (BaseUrl Http "localhost" 8082)

-- Albums API

apiAlbumsList =
    client (Proxy::Proxy AlbumsAPI) (BaseUrl Http "localhost" 8083)

-- | Utils

fly :: (Show b, MonadIO m)
    => EitherT ServantError m b
    -> EitherT ServantErr m b
fly apiReq =
    either logAndFail return =<< EitherT (liftM Right (runEitherT apiReq))
  where
    logAndFail e = do
        liftIO (putStrLn ("Got internal-api error: " ++ show e))
        left internalError
    internalError = ServantErr 500 "CyberInternal MicroServer MicroError" "" []

checkValidity :: Maybe SigninToken
              -> EitherT ServantErr IO ()
checkValidity =
    maybe (left (ServantErr 400 "Please, provide an authorization token" "" []))
          (\t -> fly (apiUsersTokenValidity t) >>= handleValidity)
  where
    handleValidity (TokenValidity True) = return ()
    handleValidity (TokenValidity False) =
        left (ServantErr 400 "Your authorization token is invalid" "" [])
