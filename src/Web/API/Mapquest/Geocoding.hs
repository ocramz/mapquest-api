{-# language OverloadedStrings, DataKinds #-}
module Web.API.Mapquest.Geocoding where

import Data.Monoid

import Network.HTTP.Req
import qualified Data.Text as T

import Control.Monad.Catch

import Data.Aeson

-- https://developer.mapquest.com/documentation/geocoding-api/address/get/

apiRootPath :: Url 'Http
apiRootPath = http "www.mapquestapi.com" /: "geocoding" /: "v1" /: "address"


-- example request :
-- GET http://www.mapquestapi.com/geocoding/v1/address?key=KEY&location=Washington,DC

instance MonadHttp IO where
  handleHttpException = throwM

request :: FromJSON a => T.Text -> Option 'Http -> IO a
request apikey opts = do
  r <- req GET apiRootPath NoReqBody jsonResponse opts'
  return $ responseBody r where
    opts' = opts <> ("key" =: apikey) <> ("outFormat" =: ("json" :: T.Text))

-- data ApiOptions = ApiOptions {
--     aoLocation :: T.Text
--                              } deriving (Eq, Show)

data GeocodingQuery = GeocodingQuery {
    gqCity :: T.Text
  , gqCountry :: T.Text
                                 } deriving (Eq, Show)


options :: Foldable t => t (T.Text, T.Text) -> Option 'Http
options = foldr (\(k, v) acc  -> (k =: v) <> acc ) mempty


