{-# LANGUAGE DeriveGeneric #-}

module Modules.ModelManager.Models.Reqs ( 
    Done (..),
    RtootReq (..),
    PrjctReq (..),
    TskReq (..),
    Verify (..),
    EmptyReq ) where

import GHC.Generics ( 
    Generic )

import qualified Data.Aeson as JSON ( 
    FromJSON,
    ToJSON )

import Modules.ModelManager.Models.Basic ( 
    OptionType (..),
    Option (..),
    RoleType (..),
    Project (..),
    ProjectType (..),
    Task (..),
    TaskType (..) )

data Verify tp = Verify {
    totalKey :: String,
    datatype :: tp
} deriving Generic
instance JSON.ToJSON tp => JSON.ToJSON (Verify tp)

data EmptyReq = EmptyReq deriving Generic
instance JSON.ToJSON EmptyReq

newtype Done = Done {
    done :: Bool
} deriving Generic
instance JSON.ToJSON Done

data RtootReq = RtootReq {
    optTp :: OptionType,
    opt :: Option,
    roleTp :: RoleType
} deriving Generic
instance JSON.ToJSON RtootReq

data PrjctReq = PrjctReq {
    prj :: Project,
    prjTp :: ProjectType
} deriving Generic
instance JSON.ToJSON PrjctReq

data TskReq = TskReq {
    tsk :: Task,
    tskTp :: TaskType
} deriving Generic
instance JSON.ToJSON TskReq
