module Test.Main where

import Prelude

import Bucketchain.Stream (convertToString)
import Bucketchain.Test (request)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, message)
import Foreign.Object (singleton)
import Main (server)
import Node.HTTP (Server, listen, close)
import Node.HTTP.Client as C
import Test.Assert (assert)

main :: Effect Unit
main = do
  s <- server
  listen s opts $ runAff_ (handleAff s) do
    testNoAccept
    testPreferJSON
    testDirectFileRequest
    testPOST
    testFallback
  where
    opts =
      { hostname: "localhost"
      , port: 3000
      , backlog: Nothing
      }

handleAff :: Server -> Either Error Unit -> Effect Unit
handleAff _ (Left err) = do
  log $ message err
  assert false
handleAff s _ = close s $ pure unit

testNoAccept :: Aff Unit
testNoAccept = do
  res <- request opts
  liftEffect $ assert $ C.statusCode res == 404
  where
    opts = C.port := 3000
        <> C.method := "GET"
        <> C.path := "/"

testPreferJSON :: Aff Unit
testPreferJSON = do
  res <- request opts
  body <- convertToString $ C.responseAsStream res
  liftEffect do
    assert $ body == "{\"code\": 12001}"
    assert $ C.statusCode res == 200
  where
    opts = C.port := 3000
        <> C.method := "GET"
        <> C.path := "/items"
        <> C.headers := headers
    headers = C.RequestHeaders $ singleton "Accept" "application/json, */*"

testDirectFileRequest :: Aff Unit
testDirectFileRequest = do
  res <- request opts
  body <- convertToString $ C.responseAsStream res
  liftEffect do
    assert $ body == "Pong!"
    assert $ C.statusCode res == 200
  where
    opts = C.port := 3000
        <> C.method := "GET"
        <> C.path := "/ping.txt"
        <> C.headers := headers
    headers = C.RequestHeaders $ singleton "Accept" "*/*"

testPOST :: Aff Unit
testPOST = do
  res <- request opts
  body <- convertToString $ C.responseAsStream res
  liftEffect do
    assert $ body == "Created Item"
    assert $ C.statusCode res == 201
  where
    opts = C.port := 3000
        <> C.method := "POST"
        <> C.path := "/items"
        <> C.headers := headers
    headers = C.RequestHeaders $ singleton "Accept" "*/*"

testFallback :: Aff Unit
testFallback = do
  res <- request opts
  body <- convertToString $ C.responseAsStream res
  liftEffect do
    assert $ body == "Hello World :)"
    assert $ C.statusCode res == 200
  where
    opts = C.port := 3000
        <> C.method := "GET"
        <> C.path := "/items"
        <> C.headers := headers
    headers = C.RequestHeaders $ singleton "Accept" "*/*"
