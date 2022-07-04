{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Monad law, left identity" #-}
module Modules.ModelManager.Yamler ( 
    catchConfig ) where

import Modules.ModelManager.Models.Sys ( 
    Config (..) )

import qualified Data.ByteString.Char8 as B8 (
    readFile,
    ByteString (..) )

import Control.Exception (
    Exception,
    SomeException (..),
    try )

import qualified Data.Text as T (
    pack )

import qualified Data.Yaml as Y ( 
    decode )

-- to catch configuratuion of api structures
catchConfig :: String -> IO (Maybe Config)
catchConfig pth =
    (try (B8.readFile pth) :: IO (Either SomeException B8.ByteString)) >>= \tryReadCong ->
    case tryReadCong of
    Left e ->
        putStrLn "could not read config" >>
        return Nothing
    Right readedBytes ->
        return (Y.decode readedBytes :: Maybe Config) >>= \dt ->
        case dt of
        Nothing ->
            putStrLn "could not parse config" >>
            return dt
        Just smth ->
            return dt