module Bucketchain.HistoryAPIFallback
  ( IndexDetector
  , defaultIndexDetector
  , withHistoryAPIFallback
  ) where

import Prelude

import Bucketchain.Http (Http, requestHeaders, requestMethod, requestURL, setRequestURL)
import Bucketchain.Middleware (Middleware)
import Control.Monad.Reader (ask)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.String (Pattern(..), indexOf, lastIndexOf)
import Effect.Class (liftEffect)
import Foreign.Object (lookup)
import Node.URL as URL

-- | A type of the index detector.
type IndexDetector = Http -> String

-- | Always use `/index.html`.
defaultIndexDetector :: IndexDetector
defaultIndexDetector = const "/index.html"

-- | Enable fallback for History API.
withHistoryAPIFallback :: IndexDetector -> Middleware
withHistoryAPIFallback indexDetector next = do
  http <- ask
  liftEffect case fallbackPath indexDetector http of
    Nothing -> pure unit
    Just url -> setRequestURL http url
  next

fallbackPath :: IndexDetector -> Http -> Maybe String
fallbackPath indexDetector http =
  (lookup "accept" $ requestHeaders http) >>= indexPath indexDetector http

indexPath :: IndexDetector -> Http -> String -> Maybe String
indexPath indexDetector http accept =
  if shouldFallback then Just $ indexDetector http else Nothing
  where
    shouldFallback =
      requestMethod http == "GET"
        && (not $ preferJSON accept)
        && acceptHTML accept
        && (not $ directFileRequest $ originalPath http)

originalPath :: Http -> String
originalPath http =
  fromMaybe "/"
    $ toMaybe
    $ _.pathname
    $ URL.parse
    $ requestURL http

preferJSON :: String -> Boolean
preferJSON accept =
  case indexOf (Pattern "application/json") accept of
    Just 0 -> true
    _ -> false

acceptHTML :: String -> Boolean
acceptHTML accept =
  case indexOf (Pattern "text/html") accept, indexOf (Pattern "*/*") accept of
    Nothing, Nothing -> false
    _, _ -> true

directFileRequest :: String -> Boolean
directFileRequest path =
  case lastIndexOf (Pattern ".") path, lastIndexOf (Pattern "/") path of
    Just i1, Just i2 | i1 > i2 -> true
    Just i1, Nothing -> true
    _, _ -> false
