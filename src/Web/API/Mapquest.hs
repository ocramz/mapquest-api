{-|
Module      : Web.API.MapQuest
Description : MapQuest
Copyright   : (c) Marco Zocca, 2018
License     : GPL-3
Maintainer  : zocca.marco gmail
Stability   : experimental
Portability : POSIX
-}
module Web.API.MapQuest (
  -- * Geocoding
  G.request,
  -- ** Parameters
  G.GeoQuery(..),
  -- ** Output
  G.Coords(..)
  ) where

import qualified Web.API.MapQuest.Geocoding as G





