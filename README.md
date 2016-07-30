Type-Safe Microservices in Haskell with Servant
===============================================

10.05.2016 NOTE! The post was updated with the latest servant-0.7
code, but some bugs might be still left. PRs are welcome!

Microservices were becoming a hot thing few years ago and it seems
[they are still on the rise](http://philcalcado.com/2015/09/08/how_we_ended_up_with_microservices.html).

It always surprised me how brave developers are: at one moment they
just decide to introduce hundreds contracts of network-separated APIs
without having any static proof that it "works well together". I have
no idea how others are solving this problem, but was interested in
trying it out in Haskell.

This repository is a tutorial (with working code) of super-small
service called OwlCloud. It is implemented mostly via
[Servant](http://haskell-servant.github.io/) framework.

I will try to explain how things look like for those who don't do
Haskell every day (I believe most Haskellers could easily do the same
anyway).

WARNING! While being small, the service is still almost real-world,
contains a lot of features, so it has quite a lot of boilerplate. The
purpose is to show that it's not THAT much boilerplate as for a
service having all these features.

![woop](http://i.imgur.com/1CnO4rN.jpg)

Architecture overview
---------------------

```
                                      +--------------------------------------+   +----------------+
                                 +--->+ Users microservice (/api/users/*)    +---> Users storage  |
                                 |    +-------------+------------------------+   +----------------+
                                 |                  ^
+-----------+      +-------------+                  |
|  Request  +----->+ Front End  ||                  |
+-----------+      +-------------+                  |
                                 |                  |
                                 |    +-------------+------------------------+   +----------------+
                                 +--->+ Albums microservice (/api/albooms/*) +---> Albums storage |
                                      +--------------------------------------+   +----------------+
```

Request hits a Front-End. Front-end will act as a proxy which only
knows prefix of each microservice's public part of API (starts with
`/api/`), and proxies request to it. It won't try to do any other job.

Each microservice is a REST app, which has public (`/api/*`) and
private (`/private-api/*`) parts. Private parts don't do any
security-checks regarding their requestor, as they're assumed to be
inside secured network. Additional security could be done in future if
needed.

Users microservice will have these end-points:

- `/api/users/owl-in/` -- accept POST-request with a json-document
  like `{"whoo": "user", "passwoord": "who?"}`, and return signin
  token (a string)
- `/api/users/owl-out` -- accepts HTTP header `Authorization` with
  token to check a user, and signs them out (forgets this token)
- `/private-api/users/token-validity/:token` -- returns a validity
  response if token is still good or not. This is an example of inner
  API, used by other microservices to not work with database directly,
  but rather ask this microservice if user's token is valid

Albums microservice will look quite simple:

- `/api/albooms/` -- will check user for a correct `Authorization`
  header via Users microservice, and in case of success will render
  list of albums with photos inside (should I create a microservice
  for photos to make things more interesting?)

  Should accept `sortby` query parameter, which is either `whoolest`
  (owl for "coolest"), or `date`.

Installation and Running
------------------------

If you're curious to play a bit with this repo's code, here are the
instructions.

1. Install [haskell stack tool](https://github.com/commercialhaskell/stack).
2. Run `stack build` inside project root (right next to this README)

To run, open 3 terminals and run these commands in them:

```
stack exec owlcloud-front
stack exec owlcloud-users
stack exec owlcloud-albums
```

Alternatively, if you have my [par](https://github.com/k-bx/par) tool
installed, you can just run:

```
par "stack exec owlcloud-front" "stack exec owlcloud-users" "stack exec owlcloud-albums"
```

If stack doesn't work for you, you can try [cabal-only-instructions](https://gist.github.com/k-bx/ff100755eaa12f950e9a).

Code overview: projects layout
------------------------------

There are 4 cabal projects in root of this project: `owlcloud-front`,
`owlcloud-lib`, `owlcloud-users` and `owlcloud-albums`.

`-front` will correspond to front-end proxy, `-lib` contains shared
code like routes, types, and common utilities.

`-users` and `-albums` are purely microservices.

Routes
------

In Servant, type-safety of your REST API is taken to the extreme. This
means that type expresses not only path of your route, but also types
of its dynamic pieces, query-parameters, type of data passed through
request-body, expected headers, format and type of
return-value. Sounds impressive, isn't it?

But how does it look like, exactly? Well, it's far from looking as an
intuitive DSL, but it's good-enough to not want to write one, imho.

The code for routes resides in
[owlcloud-lib/src/OwlCloud/Types.hs](owlcloud-lib/src/OwlCloud/Types.hs).

Servant API expects you to first describe routes "in types", and then
connect them with your routes where you want to. So, you can
"implement" your routes several times, just as you can write multiple
functions of some type. This lets us describe API for each
microservice, and then combine them in bigger API.

```haskell
type OwlCloudAPI = UsersAPI :<|> AlbumsAPI
```

This is a central type, which describes all our APIs. We have two
type-synonyms for each microservice, and then smash them together with
a type-level combinator `:<|>`. Using type-synonyms means that our
type-errors might (and will!) become nasty, and rather suitable for
experienced haskeller's brain, but nothing very special to Servant is
needed, just a general Haskell type-error-resolving experience.

We don't actually use this type, since our front-end proxy just blindly
proxies requests by their prefixes, but having `OwlCloudAPI` might be
useful for documentation and type-checking purposes. You might also
combine your microservices in one big service for purposes of testing
locally, but it's not in scope of this tutorial.

So, types for Users microservice will look like this:

```haskell
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
```

That's a big piece of code. Let's look closer.

Again, we have individual routes combined together with `:<|>` at the
end of each line.

First route looks like this:

```haskell
  "api" :> "users" :> "owl-in" :> ReqBody '[JSON] LoginReq :> Post '[JSON] SigninToken
```

It corresponds (as you might have guessed) to route
`/api/users/owl-in`. `ReqBody '[JSON] LoginReq` tells that Servant
will take a request body, requiring a `Content-Type: application/json`
header in your request, decode it as a `JSON` decoder (it can support
multiple, if you put more in list) into `LoginReq` type, and pass it
as a parameter to your handler, which we'll see later.

`Post '[JSON] SigninToken` tells us that we'll respond to
`POST`-reuqest, we'll respond in `JSON` with a `SigninToken` datatype.

Wow, whole bunch of information about our route, and all that encoded
in mostly-readable type-level representation. Neat!

Second route:

```haskell
  "api" :> "users" :> Authorized ("owl-out" :> Post '[JSON] ())
```

Everything should be clear except that `Authorized` function-like
thing. What's that? It's a type-synonym I defined at the bottom of the
same file:

```haskell
type Authorized t = Header "Authorization" SigninToken :> t
```

So, if you replace a type-synonym, your route will look like:

```haskell
  "api" :> "users" :> Header "Authorization" SigninToken :> "owl-out" :> Post '[JSON] ()
```

Every resource, which wants to access data of some user, will have to
send a `SigninToken`, which is just a newtype around `Text`, and put
it under `Authorization` header.

Last route is:


```haskell
  "private-api" :> "users" :> "token-validity" :> Capture "token" SigninToken :> Get '[JSON] TokenValidity
```

New part is `Capture` here. It just tells that we have a dynamic part
of a route `/private-api/users/token-validity/<dynamic-part-here>`,
which will be captured and passed as a param into our handler.

Albums microservice types should look quite familiar now:

```haskell
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

```

We capture a `sortby` param, which you will pass as `?sortby=date` at
the end of your url.

Here you can also see manual implementation of `FromHttpApiData`
type-class: it's used to encode/decode url piece value into your
datatype.

Handlers
--------

We've covered routes, now we can show how our handlers look
like. Let's begin with a Users microservice.

Code resides at
[./owlcloud-users/src/Main.hs](owlcloud-users/src/Main.hs) file.

Let's begin with some machinery to combine individual handlers into
`UsersAPI` type, and then generation of a
[wai](http://hackage.haskell.org/package/wai) `Application` type. WAI
is a set of contracts, which describe a "reusable haskell web
application interface". It's similar to Python's WSGI, if you're
familiar with that. After you have a WAI `Application`, you can run it
with a haskell web-server of your choice. We'll use
[warp](http://hackage.haskell.org/package/warp), which is as fast as
nginx (and sometimes faster!).

```haskell
server :: Server UsersAPI
server = owlIn :<|> owlOut :<|> tokenValidity

usersAPI :: Proxy UsersAPI
usersAPI = Proxy

app :: Application
app = serve usersAPI server
```

First thing to notice is that we use a new operator `:<|>` from
Servant, which is a value-level operator (never confuse with `:<|>`,
haha). It combines individual handlers together, and type-system then
checks that type of overall expression matches `UsersAPI`. Errors are
somewhat big, as type-synonyms are expanded with not too much help to
us, but if you'll look careful enough -- you'll be able to figure
thing out.

Now, to individual handlers:

```haskell
owlIn :: LoginReq -> ExceptT ServantErr IO SigninToken
owlIn LoginReq{..} =
    case (whoo, passwoord) of
      ("great horned owl", "tiger") -> do
          uuid <- liftIO UUID.nextRandom
          let token = SigninToken (UUID.toText uuid)
          liftIO $ atomically $
              modifyTVar db $ \s ->
                s { validTokens = Set.insert token (validTokens s) }
          return token
      _ -> throwE (ServantErr 400 "Username/password pair did not match" "" [])
```

They start with our `/api/users/owl-in` handler. We begin with
something which amazes me about Servant already: you get your
route-parameters as...function parameters!

So, no more silly manual extraction of data from some big `Request`
type: you get what you asked for, and you get it via function
parameters. Servant handles the rest for you.

Now, since we don't use a real database for purpose of this tutorial,
we'll just allow single login/password pair: ("great horned owl",
"tiger"). If it matches, we are generating a `SigninToken` and put it
into a global STM-variable `db`. It resides inside
[Common.hs](./owlcloud-lib/src/OwlCloud/Common.hs), if you're interested
looking at actual implementation, but in real-world app it'll probably
just be a database. For curious, type of our database is this:

```haskell
data State = State
    { validTokens :: Set SigninToken
    , albumsList  :: [Album] }

db :: TVar State
db = unsafePerformIO (unsafeInterleaveIO (newTVarIO (State Set.empty initialAlbums)))
```

Yes, we use a global variable in Haskell, and sometimes it makes
sense, and it's dangerous (as indicated by the scary names).

If you enter wrong credentials, we will respond with a `400`-code
error, and a help-message describing the reason. You can add some
response body, and additional headers if you want to, but I don't.

Error is returned in this interesting way:

```haskell
throwE (ServantErr 400 "Username/password pair did not match" "" [])
```

This
[throwE](http://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Except.html#v:throwE)
combinator from `transformers` package, is something which converts
some error-type `e` into a `ExceptT e m a` type. The reason we're
using it is because Servant uses type `ExceptT ServantErr IO a` for
our handlers. It's a small
[Monad Transformer](https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md)
stack on top of IO, which allows explicit short-circuiting via
`ServantErr` type, denoting failure.

Our `owl-out` handler just removes your token from our imaginary database:

```haskell
owlOut :: Maybe SigninToken -> ExceptT ServantErr IO ()
owlOut mt = do
    checkAuth mt
    maybe (return ()) out mt
  where
    out token = liftIO $ atomically $ modifyTVar db $ \s ->
                  s { validTokens = Set.delete token (validTokens s) }

checkAuth :: Maybe SigninToken -> ExceptT ServantErr IO ()
checkAuth = maybe unauthorized runCheck
  where
    runCheck (SigninToken token) = do
        state <- liftIO $ atomically $ readTVar db
        let isMember = Set.member (SigninToken token) (validTokens state)
        unless isMember unauthorized
    unauthorized =
        throwE (ServantErr 401 "You are not authenticated. Please sign-in" "" [])
```

Last handler is an inner API to check token validity:

```haskell
tokenValidity :: SigninToken -> ExceptT ServantErr IO TokenValidity
tokenValidity token = do
    state <- liftIO $ atomically $ readTVar db
    return (TokenValidity (Set.member token (validTokens state)))
```

Finally, we run our app on port `8082`. Of course, this should be
stored in some environment variable or config in real-world:

```haskell
main :: IO ()
main = run 8082 app
```

We use `run` from Warp web-server mentioned before.

The Albums microservice shouldn't be much harder to understand. Just
one end-point, no need for glueing with `:<|>` operator:

```haskell
server :: Manager -> Server AlbumsAPI
server = albums
```

Also notice that we'll need to pass a `Manager` value in order to have
connection-pooling and caching when we speak to other
microservices. It's created in main and just passed in parameters.

Handler:

```haskell
albums :: Manager -> Maybe SigninToken -> Maybe SortBy -> ExceptT ServantErr IO [Album]
albums mgr mt sortBy = do
    checkValidity mgr mt
    state <- liftIO $ atomically $ readTVar db
    return (albumsList state)
```

We don't do any actual sorting here, GHC will tell us about this via
Warning of unused `sortBy` variable (how many frameworks tell you your
GET-parameters from your API description are not used?).

Now, the interesting part is the `checkValidity` function. We put it
in `Common.hs`, since it'd be reused by other microservices in the
future. It will do a request to the Users microservice, check the validity of
a token, and show an error if needed.

```haskell
checkValidity :: Manager
              -> Maybe SigninToken
              -> ExceptT ServantErr IO ()
checkValidity mgr =
    maybe (throwE (ServantErr 400 "Please, provide an authorization token" "" []))
          (\t -> fly (apiUsersTokenValidity t mgr usersBaseUrl) >>= handleValidity)
  where
    handleValidity (TokenValidity True) = return ()
    handleValidity (TokenValidity False) =
        throwE (ServantErr 400 "Your authorization token is invalid" "" [])
```

You already understand all the `left ...` parts which just return
errors. But what's `fly (apiUsersTokenValidity t)`, exactly?

Let's make a new sub-header in this tutorial so it's easier to find.

Requesting other microservices
------------------------------

Servant gives you a mechanism to request other services in a type-safe
manner. What you need to do, is to "unpack" your type-level API
definition into individual request-routes (also in [Common.hs](./owlcloud-lib/src/OwlCloud/Common.hs)):

```haskell
apiUsersOwlIn :<|> apiUsersOwlOut :<|> apiUsersTokenValidity =
    client (Proxy::Proxy UsersAPI)

apiAlbumsList =
    client (Proxy::Proxy AlbumsAPI)
```

The scary `Proxy::Proxy UsersAPI` part is just to move things from
type-level to value-level world. This is usually done when you're
already able to extract all needed information from just a type, but
you need to do some actions with it at the value-level world.

So, we deconstructed some special-built structure (via `client`
function) into individual routines, which are able to request other
microservices.

Their types take usual route arguments, and are returning something of
type `ExceptT ServantError m a`. So, these values are not some
descriptions, but rather actions themselves, and they do the hard job
of requesting microservices for you. Cool!

Notice the
[`ServantError`](hackage.haskell.org/package/servant-client/docs/Servant-Client.html#t:ServantError)
type. It's not the
[`ServantErr`](http://hackage.haskell.org/package/servant-server/docs/Servant-Server-Internal-ServantErr.html)
type we've seen before, used to short-circuit from handler. Rather,
it's a REST-client-response error, which might happen if, say, your
microservice is down or responded with an error.

So we implement a special `fly` function, which will convert the response
from one possible error (microservice-request error) into another: the one
which we will send to our users, plus some logging.

```haskell
fly :: (Show b, MonadIO m)
    => ExceptT ServantError m b
    -> ExceptT ServantErr m b
fly apiReq = do
  res <- lift (runExceptT apiReq)
  either logAndFail return res
  where
    logAndFail e = do
        liftIO (putStrLn ("Got internal-api error: " ++ show e))
        throwE internalError
    internalError = ServantErr 500 "CyberInternal MicroServer MicroError" "" []
```

There you go, now you know how that type-safe microservice-requesting
machinery works. Wasn't that hard, wasn't it!

Last bit: front-end
-------------------

Now, the last bit is to write a front-end. Code is located at
[owlcloud-front/src/Main.hs](./owlcloud-front/src/Main.hs) file.

I admit, I didn't implement a bullet-proof fully-functional proxy
which handles everything in a streaming fashion (sombody, please do
so, would be a useful tutorial, and shouldn't take too much code),
it's just not what I intend to do in this tutorial, but this one
shouldn't be bad in terms of performance.

We define our app as:

```haskell
app :: Manager -> Application
app mgr req respond =
    case pathInfo req of
        ("api":"users":_) -> microservice "http://localhost:8082/"
        ("api":"albooms":_) -> microservice "http://localhost:8083/"
        _ -> respond (responseLBS status404 [] ",,,(o,o),,,\n ';:`-':;' \n   -\"-\"-   \n")
  where
    microservice = microserviceProxy mgr req respond
```

We look at request-path, if it begins with `/api/users`, we
micro-forward it to "http://localhost:8082/" (hardcode!). Same for
`/api/albooms` end-point.

We use a [wreq](http://hackage.haskell.org/package/wreq) package to do
actual requests:

```haskell
microserviceProxy :: Manager -> Request -> (Network.Wai.Response -> IO b) -> Text
                  -> IO b
microserviceProxy mgr req respond basePath = do
    let opts = W.defaults & W.manager .~ Right mgr
                          & W.headers .~ requestHeaders req
                          & W.params .~ getReqParams req
        url = basePath <> T.intercalate "/" (pathInfo req)
    tryProxying opts url `catch` onErr
  where
    tryProxying opts url = do
      r <- case requestMethod req of
             "GET" -> W.getWith opts (toString url)
             "POST" -> requestBody req >>= W.postWith opts (toString url)
      respond (responseLBS (r ^. W.responseStatus) (r ^. W.responseHeaders)
                 (r ^. W.responseBody))
    onErr (StatusCodeException s hdrs _) = respond (responseLBS s hdrs "")
    onErr e = do
      putStrLn ("Internal error: " ++ show e)
      respond (responseLBS status500 [] "Internal server error")
```

We just re-build a request from the request we received ourselves, and
then respond with the response we receive. We implement `GET` and
`POST` methods only, but you've got the idea for others.

Last bit -- running our front-end:

```haskell
main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    run 8081 (app mgr)
```

We create a `wreq` manager, which handles keep-alived connections (to
not re-connect to microservice on each request) for us, and just run
the web-server.

That's it. That was easy, wasn't it?

Testing
-------

Let us look how it works.

```
➜  ~  curl -i -XGET -H "Content-Type: application/json" -H "Authorization: badtoken" localhost:8083/api/albooms/
HTTP/1.1 400 Your authorization token is invalid
Transfer-Encoding: chunked
Date: Sat, 12 Sep 2015 09:07:19 GMT
Server: Warp/3.1.3

➜  ~  curl -i -XPOST -H "Content-Type: application/json" --data '{"whoo": "great horned owl", "passwoord": "tiger"}' localhost:8081/api/users/owl-in
HTTP/1.1 201 Created
Transfer-Encoding: chunked
Transfer-Encoding: chunked
Date: Sat, 12 Sep 2015 09:07:36 GMT
Server: Warp/3.1.3
Content-Type: application/json

"88255ebf-2dca-4638-b037-639fb762f6e0"

➜  ~  curl -i -XGET -H "Content-Type: application/json" -H "Authorization: 88255ebf-2dca-4638-b037-639fb762f6e0" localhost:8083/api/albooms/
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Sat, 12 Sep 2015 09:07:48 GMT
Server: Warp/3.1.3
Content-Type: application/json

[[{"image":"http://i.imgur.com/PuhhmQi.jpg","description":"Scating"},{"image":"http://i.imgur.com/v5kqUIM.jpg","description":"Taking shower"}],[{"image":"http://i.imgur.com/3hRAGWJ.png","description":"About to fly"},{"image":"http://i.imgur.com/ArZrhR6.jpg","description":"Selfie"}]]
```

Conclusion
----------

We just saw how easy it is to write some boilerplate to use The
Microservice Architecture, keeping our type-safety for us and our
future team mates happy.

I hope you enjoyed it.

Please, send your PRs improving both code and tutorial if you feel
like doing that.

![LLAP](http://i.imgur.com/OBxk06B.jpg)
