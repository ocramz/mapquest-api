{-# language OverloadedStrings, DataKinds #-}
module Web.API.Mapquest.Geocoding where

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

request :: Option 'Http -> IO (JsonResponse a0)
request = req GET apiRootPath NoReqBody jsonResponse
