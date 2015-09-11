Type-Safe Microservices with Servant
====================================

Microservices were becoming a hot thing few years ago and seems
[they are still on the rise](http://philcalcado.com/2015/09/08/how_we_ended_up_with_microservices.html).

It always surprised me how brave developers are: at one moment they
just decide to introduce thousands of contracts of network-separated
APIs without having any static proof that it "works well together". I
have no idea how others are solving this problem, but was interested
in trying it out in Haskell.

This repository is a tutorial (with working code) of super-small
service called OwlCloud. It is implemented mostly via
[Servant](http://haskell-servant.github.io/) framework.

I will try to explain how things look like for those who don't do
Haskell every day (I believe most Haskellers could easily do the same
anyway).
