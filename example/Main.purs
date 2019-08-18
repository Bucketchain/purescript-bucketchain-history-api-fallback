module Main where

import Prelude

import Bucketchain (createServer, listen)
import Bucketchain.HistoryAPIFallback (defaultIndexDetector, withHistoryAPIFallback)
import Bucketchain.Http (requestMethod, requestURL, setHeader, setStatusCode)
import Bucketchain.Middleware (Middleware)
import Bucketchain.ResponseBody (body)
import Control.Monad.Reader (ask)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Node.HTTP (ListenOptions, Server)

main :: Effect Unit
main = server >>= listen opts

server :: Effect Server
server = createServer $ withHistoryAPIFallback defaultIndexDetector
  <<< middleware1
  <<< middleware2
  <<< middleware3
  <<< middleware4

opts :: ListenOptions
opts =
  { hostname: "127.0.0.1"
  , port: 3000
  , backlog: Nothing
  }

middleware1 :: Middleware
middleware1 next = do
  http <- ask
  if requestMethod http == "GET" && requestURL http == "/index.html"
    then liftEffect do
      setHeader http "Content-Type" "text/plain; charset=utf-8"
      setStatusCode http 200
      Just <$> body "Hello World :)"
    else next

middleware2 :: Middleware
middleware2 next = do
  http <- ask
  if requestMethod http == "GET" && requestURL http == "/items"
    then liftEffect do
      setHeader http "Content-Type" "application/json; charset=utf-8"
      setStatusCode http 200
      Just <$> body "{\"code\": 12001}"
    else next

middleware3 :: Middleware
middleware3 next = do
  http <- ask
  if requestMethod http == "GET" && requestURL http == "/ping.txt"
    then liftEffect do
      setHeader http "Content-Type" "text/plain; charset=utf-8"
      setStatusCode http 200
      Just <$> body "Pong!"
    else next

middleware4 :: Middleware
middleware4 next = do
  http <- ask
  if requestMethod http == "POST" && requestURL http == "/items"
    then liftEffect do
      setHeader http "Content-Type" "text/plain; charset=utf-8"
      setStatusCode http 201
      Just <$> body "Created Item"
    else next
