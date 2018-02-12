{-# language OverloadedStrings, DataKinds #-}
module Web.API.Mapquest.Geocoding where

import Data.List (intersperse)
import Data.Monoid (mempty, (<>))

import Network.HTTP.Req
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

import Control.Monad.Catch

import Data.Aeson

-- https://developer.mapquest.com/documentation/geocoding-api/address/get/

apiRootPath :: Url 'Http
apiRootPath = http "www.mapquestapi.com" /: "geocoding" /: "v1" /: "address"


-- example request :
-- GET http://www.mapquestapi.com/geocoding/v1/address?key=KEY&location=Washington,DC

instance MonadHttp IO where
  handleHttpException = throwM

request ::
     T.Text
  -> Option 'Http
  -> GeoQuery
  -> IO LBS.ByteString
request apikey opts q = do
  r <- req GET apiRootPath NoReqBody lbsResponse opts'
  return $ responseBody r where
    opts' = opts <>
      ("key" =: apikey) <>
      ("outFormat" =: ("json" :: T.Text)) <>
      ("location" =: renderGeoQuery q)


data GeoQuery = GQ {
    gqStreet :: T.Text
  , gqCity :: T.Text
  , gqCountry :: T.Text
                                 } deriving (Eq, Show)

renderGeoQuery :: GeoQuery -> T.Text
renderGeoQuery (GQ addr city country) =
  T.concat $ intersperse ", " [addr, city, country]

options :: Foldable t => t (T.Text, T.Text) -> Option 'Http
options = foldr (\(k, v) acc  -> (k =: v) <> acc ) mempty


