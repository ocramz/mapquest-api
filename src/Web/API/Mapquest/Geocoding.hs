{-# language OverloadedStrings, DataKinds, DeriveGeneric #-}
module Web.API.Mapquest.Geocoding (request, GeoQuery(..), Coords(..))where

import Data.List (intersperse)
import Data.Monoid (mempty, (<>))

import Network.HTTP.Req
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy as LBS

import GHC.Generics

import Control.Monad.Catch

import Data.Aeson
import Data.Aeson.Types (Parser(..), parseMaybe, parseEither)

-- https://developer.mapquest.com/documentation/geocoding-api/address/get/

apiRootPath :: Url 'Http
apiRootPath = http "www.mapquestapi.com" /: "geocoding" /: "v1" /: "address"


-- example request :
-- GET http://www.mapquestapi.com/geocoding/v1/address?key=KEY&location=Washington,DC

instance MonadHttp IO where
  handleHttpException = throwM

-- | Call the MapQuest Geocoding API with a given address and extract the coordinates from the parsed result
request ::
     T.Text
  -> GeoQuery
  -> IO (Maybe (Coords Float))
request apikey q = do
  r <- req GET apiRootPath NoReqBody lbsResponse opts'
  return $ decoder1 $ responseBody r where
    opts' = 
      ("key" =: apikey) <>
      ("outFormat" =: ("json" :: T.Text)) <>
      ("location" =: renderGeoQuery q)



decoder1 :: LBS.ByteString -> Maybe (Coords Float)
decoder1 dat = do
  r <- decode dat
  flip parseMaybe r $ \obj -> do 
    locp <- decodeLocation obj
    decodeLatLong locp

decodeLocation :: FromJSON a => Object -> Parser a
decodeLocation obj = do
    (res0 : _) <- obj .: "results"
    (loc0 : _) <- res0 .: "locations"
    return loc0

decodeLatLong :: Object -> Parser (Coords Float)
decodeLatLong loc = do
    ll <- loc .: "latLng"
    Coords <$> ll .: "lat" <*> ll .: "lng"
    
-- | Coordinates
data Coords a = Coords {
    lat :: a -- ^ Latitude
  , long :: a -- ^ Longitude
  } deriving (Eq, Show, Generic)

-- instance Functor Coords where
--   fmap f (Coords x y) = Coords (f x) (f y)

-- instance FromJSON a => FromJSON (Coords a)


-- | Geocoding query 
data GeoQuery = GQ {
    gqStreet :: T.Text -- ^ Street address
  , gqCity :: T.Text   -- ^ City 
  , gqCountry :: T.Text -- ^ Country
  } deriving (Eq, Show)

renderGeoQuery :: GeoQuery -> T.Text
renderGeoQuery (GQ addr city country) =
  T.concat $ intersperse ", " [addr, city, country]

-- options :: Foldable t => t (T.Text, T.Text) -> Option 'Http
-- options = foldr (\(k, v) acc  -> (k =: v) <> acc ) mempty



