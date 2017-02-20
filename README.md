# HaskServer
## Simple web server written in Haskell

## Installation
Unpack the project (if you're reading this, you probably already did)
run:
    cabal install

This should install the server in your cabal /bin directory, usually '~/.cabal/bin/'

## Running tests:
    cabal install --only-dependencies --enable-tests
    cabal configure --enable-tests
    cabal build
    cabal test

## Project description

This simple server will receive a request at a time, will process it in a separate thread
and will return a response.

The processing involves parsing the request as an HTTP request, and passing it into an
a function of type `HTTPRequest -> HTTPResponse`, and finally sending the response and
then closing the connection.

Currently the only `RequestHandler` defined is the `echo` handler that will return 200
for every request with the content of the request as it's body.

The project also include unit tests, using Hspec.

Approximately 200 lines of code (with tests)

