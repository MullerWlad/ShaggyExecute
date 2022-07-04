{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Modules.Global.Logging (
    writeLog ) where

import Data.DateTime ( 
    getCurrentTime,
    addSeconds )

import System.Directory (
    createDirectory )

import Control.Exception (
    Exception,
    SomeException (..),
    try )

import qualified Data.Text as T (
    unpack,
    pack,
    Text (..) )

-- to log writing
writeLog :: T.Text -> IO ()
writeLog log =
    (try (
    getCurrentTime >>= (return . addSeconds 10800) >>= \timeOpen ->
    appendFile "./resource/logs.log" (show timeOpen ++ ": " ++ T.unpack log ++ "\n")
    ) :: IO (Either SomeException ())) >>= \solve ->
    case solve of
    Left e ->
        putStrLn "WARNING: could not write log" >>
        createDirectory "./resource" >>
        writeLog (T.pack "resource created")
    Right () ->
        return ()