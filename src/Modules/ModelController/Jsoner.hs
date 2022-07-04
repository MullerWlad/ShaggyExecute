module Modules.ModelController.Jsoner ( 
    searchThrowObj ) where

import Data.Aeson.KeyMap as KM ( 
    lookup )

import Data.Aeson.Key ( 
    fromString )

import qualified Data.Aeson as JSON (
    Object (..),
    Value (..) )

-- searcher in object
searchThrowObj :: String -> JSON.Object -> Maybe JSON.Value
searchThrowObj keyy =
    KM.lookup (fromString keyy)
