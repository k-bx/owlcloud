Type-Safe Microservices in Haskell with Servant
===============================================

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
knows prefix of ech microservice's public part of API (starts with
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
  API, used by other microservices to not touch database, but rather
  ask this microservice if user's token is valid

Albums microservice will look quite simple:

- `/api/albooms/` -- will check user for a correct `Authorization`
  header via Users microservice, and in case of success will render
  list of albums with photos inside (should I create a microservice
  for photos to make things more interesting?)

  Should accept `sortby` query parameter, which is either `whoolest`
  (owl for "coolest"), or `date`.

