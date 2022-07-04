{-# LANGUAGE DeriveGeneric #-}

module Modules.ModelManager.Models.Sys ( 
    Config (..) ) where

import GHC.Generics (
    Generic )

import qualified Data.Aeson as JSON ( 
    FromJSON )

-- configuration type
data Config = Config {
    dbHost :: String,
    dbPort :: Int,
    apiPort :: Int,
    token :: String
} deriving (Show, Eq, Generic)
instance JSON.FromJSON Config
