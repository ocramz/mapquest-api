{-# language OverloadedStrings, DataKinds, DeriveGeneric #-}
{-# language DeriveFunctor, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
{-# language PackageImports #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}

{-|
Module      : Web.API.MapQuest.Geocoding
Description : Geocoding interface
Copyright   : (c) Marco Zocca, 2018
License     : GPL-3
Maintainer  : zocca.marco gmail
Stability   : experimental
Portability : POSIX
-}
module Web.API.MapQuest.Geocoding
  (runRequest, Creds(..), GeoQuery(..), Coords(..))
   where

import Data.List (intersperse)
import Data.Monoid ((<>))

import "mtl" Control.Monad.Reader.Class
import Network.HTTP.Req
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS

import GHC.Generics

import Control.Monad.Catch

import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)

import Network.Goggles

-- https://developer.mapquest.com/documentation/geocoding-api/address/get/

apiRootPath :: Url 'Http
apiRootPath = http "www.mapquestapi.com" /: "geocoding" /: "v1" /: "address"

-- | The user's API credential (i.e. the MapQuest "consumer key", visible at <https://developer.mapquest.com/user/me/apps> )
newtype Creds = Creds { apiKey :: T.Text } deriving (Eq, Show)

data MapQuest

instance HasCredentials MapQuest where
  type Credentials MapQuest = Creds
  type Options MapQuest = ()      -- NB: Options and TokenContent are moved to HasToken in goggles-0.3
  type TokenContent MapQuest = ()
  tokenFetch = undefined

instance MonadHttp (WebApiM MapQuest) where
  handleHttpException = throwM

-- example request :
-- GET http://www.mapquestapi.com/geocoding/v1/address?key=KEY&location=Washington,DC





request ::
     -- T.Text  -- ^ API key (available for free on <https://developer.mapquest.com>)
     GeoQuery -- ^ Query address
  -> WebApiM MapQuest (Maybe (Coords Float))
request q = do
  key <- asks (apiKey . credentials) 
  r <- req GET apiRootPath NoReqBody lbsResponse (opts' key)
  return $ decoder1 $ responseBody r where
    opts' k = 
      ("key" =: k) <>
      ("outFormat" =: ("json" :: T.Text)) <>
      ("location" =: renderGeoQuery q)

-- | Call the MapQuest Geocoding API with a given address and extract the coordinates from the parsed result.
--
-- Example usage :
-- assuming the user has bound /key/ to hold the API key string, the following can be run in a GHCi shell :
--
-- >>> runRequest key (GQ "Via Irnerio" "Bologna" "Italy")
-- Just (Coords {lat = 44.49897, long = 11.34503})
--
-- >>> runRequest key (GQFree "Ngong Ping 360, Hong Kong")
-- Just (Coords {lat = 22.264412, long = 114.16706})
runRequest ::
  Creds -> GeoQuery -> IO (Maybe (Coords Float))
runRequest k q = do
  h <- createHandle k ()
  evalWebApiIO h (request q)


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


-- | Geocoding query parameters
data GeoQuery = GQ {
    gqStreet :: T.Text -- ^ Street address (e.g. \"Via Irnerio\")
  , gqCity :: T.Text   -- ^ City (e.g. \"Bologna\")
  , gqCountry :: T.Text -- ^ Country (e.g. \"Italy\")
  }
  | GQFree T.Text -- ^ Free-text query (must be a valid address or location e.g. \"Ngong Ping 360, Hong Kong\")
  deriving (Eq, Show)

renderGeoQuery :: GeoQuery -> T.Text
renderGeoQuery q = case q of
  (GQ addr city country) -> T.concat $ intersperse ", " [addr, city, country]
  (GQFree t) -> t 

-- options :: Foldable t => t (T.Text, T.Text) -> Option 'Http
-- options = foldr (\(k, v) acc  -> (k =: v) <> acc ) mempty



