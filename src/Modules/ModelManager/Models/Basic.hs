{-# LANGUAGE DeriveGeneric #-}

module Modules.ModelManager.Models.Basic ( 
    OptionType (..),
    Option (..),
    RoleType (..),
    Role (..),
    Project (..),
    ProjectType (..),
    User (..),
    Teamate (..),
    TaskType (..),
    Task (..),
    File (..),
    FileToTask (..) ) where

import GHC.Generics ( 
    Generic )

import qualified Data.Aeson as JSON ( 
    FromJSON,
    ToJSON )

import qualified Data.Text as T ( 
    Text (..) )

-- all Database types
data OptionType = OptionType {
    typeOT :: T.Text,
    descriptionOT :: Maybe T.Text
} deriving (Show, Eq, Generic)
instance JSON.FromJSON OptionType
instance JSON.ToJSON OptionType

data Option = Option {
    typeOTO :: T.Text,
    typeRTO :: T.Text
} deriving (Show, Eq, Generic)
instance JSON.FromJSON Option
instance JSON.ToJSON Option

data RoleType = RoleType {
    typeRT :: T.Text,
    descriptionRT :: Maybe T.Text
} deriving (Show, Eq, Generic)
instance JSON.FromJSON RoleType
instance JSON.ToJSON RoleType

data Role = Role {
    idPR :: T.Text,
    positionR :: Int,
    typeRTR :: T.Text,
    nameR :: T.Text,
    descriptionR :: T.Text
} deriving (Show, Eq, Generic)
instance JSON.FromJSON Role
instance JSON.ToJSON Role

data Project = Project {
    idP :: T.Text,
    typeP :: T.Text,
    descriptionP :: T.Text,
    nameP :: T.Text,
    priceP :: Maybe Double,
    openDateP :: T.Text,
    progressP :: Double
} deriving (Show, Eq, Generic)
instance JSON.FromJSON Project
instance JSON.ToJSON Project

data ProjectType = ProjectType {
    typePT :: T.Text,
    descriptionPT :: Maybe T.Text,
    periodPT :: Maybe Double,
    priceFromPT :: Maybe Double
} deriving (Show, Eq, Generic)
instance JSON.FromJSON ProjectType
instance JSON.ToJSON ProjectType

data User = User {
    chatIdU :: Int,
    usernameU :: T.Text
} deriving (Show, Eq, Generic)
instance JSON.FromJSON User
instance JSON.ToJSON User

data Teamate = Teamate {
    idPRTmt :: Int,
    typeRTTmt :: T.Text,
    chatIdUTmt :: Int,
    positionRTmt :: Int
} deriving (Show, Eq, Generic)
instance JSON.FromJSON Teamate
instance JSON.ToJSON Teamate

data TaskType = TaskType {
    typeTT :: T.Text,
    descriptionTT :: Maybe T.Text,
    periodTT :: Double,
    priceFromTT :: Maybe Double
} deriving (Show, Eq, Generic)
instance JSON.FromJSON TaskType
instance JSON.ToJSON TaskType

data Task = Task {
    idTsk :: Int,
    typeTTTsk :: T.Text,
    idPRTsk :: Int,
    typeRTTsk :: T.Text,
    chatIdUTsk :: Int,
    positionRTsk :: Int,
    descriptionTsk :: Maybe T.Text,
    nameTsk :: String,
    priceTsk :: Maybe Double,
    openDateTsk :: T.Text,
    statusTsk :: Bool
} deriving (Show, Eq, Generic)
instance JSON.FromJSON Task
instance JSON.ToJSON Task

newtype File = File {
    nameF :: T.Text
} deriving (Show, Eq, Generic)
instance JSON.FromJSON File
instance JSON.ToJSON File

data FileToTask = FileToTask {
    nameFFtf :: T.Text,
    idTskFtf :: Int
} deriving (Show, Eq, Generic)
instance JSON.FromJSON FileToTask
instance JSON.ToJSON FileToTask
