{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Modules.GeneralRoutes () where

import Servant ( 
    (:>),
    (:<|>) (..),
    ReqBody (..),
    Get,
    Post,
    JSON,
    Server,
    Handler  )

import Modules.ModelManager.Models.Basic ( 
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
    FileToTask (..) )

import Modules.ModelManager.Models.Reqs ( 
    Done (..),
    RtootReq (..),
    PrjctReq (..),
    TskReq (..),
    Verify (..),
    EmptyReq )

-- endpoints

-- special root types to add
-- user
-- roleType, option, optionType
-- project, projectType
-- role
-- teamate
-- task, taskType
-- fileToTask
-- file
type Adding = 
    "add" :> "user" :> ReqBody '[JSON] (Verify User) :> Post '[JSON] Done
    :<|>
    "add" :> "role-type" :> ReqBody '[JSON] (Verify RtootReq) :> Post '[JSON] Done
    :<|>
    "add" :> "project" :> ReqBody '[JSON] (Verify PrjctReq) :> Post '[JSON] Done
    :<|>
    "add" :> "role" :> ReqBody '[JSON] (Verify Role) :> Post '[JSON] Done
    :<|>
    "add" :> "teamate" :> ReqBody '[JSON] (Verify Teamate) :> Post '[JSON] Done
    :<|>
    "add" :> "task" :> ReqBody '[JSON] (Verify TskReq) :> Post '[JSON] Done
    :<|>
    "add" :> "file-to-task" :> ReqBody '[JSON] (Verify FileToTask) :> Post '[JSON] Done
    :<|>
    "add" :> "file" :> ReqBody '[JSON] (Verify File) :> Post '[JSON] Done

addServer :: Server Adding
addServer = 
    addUser :<|> 
    addRoleType :<|> 
    addProject :<|> 
    addRole :<|> 
    addTeamate :<|>
    addTask :<|>
    addFileToTask :<|>
    addFile
    where
        addUser :: Verify User -> Handler Done
        addUser (Verify token (User chatId username)) =
            return (Done True)

        addRoleType :: Verify PrjctReq -> Handler Done
        addRoleType (Verify token (PrjctReq (Project idP tp desc name price opendate progress) (ProjectType tpPt descPt period priceFrom))) =
            return (Done True)

        addProject :: Verify RtootReq -> Handler Done
        addProject (Verify token (RtootReq (OptionType typeOt descOt) (Option typeOto typeRto) (RoleType typeRt descRt))) =
            return (Done True)

        addRole :: Verify Role -> Handler Done
        addRole (Verify token (Role prjId posR typeRtr name desc)) =
            return (Done True)

        addTeamate :: Verify Teamate -> Handler Done
        addTeamate (Verify token (Teamate tmId typeRt chatId posRt)) =
            return (Done True)

        addTask :: Verify TskReq -> Handler Done
        addTask (Verify token (TskReq (Task tId typeTttsk projId roleTtype chatId pos desc name price openD status) (TaskType typeTt descTt period priceFrom))) =
            return (Done True)

        addFileToTask :: Verify FileToTask -> Handler Done
        addFileToTask (Verify token (FileToTask filename tskId)) =
            return (Done True)

        addFile :: Verify File -> Handler Done
        addFile (Verify token (File filename)) =
            return (Done True)


-- special root types to get
-- all users
-- rolesTypes, 
-- options, 
-- optionTypes, 
-- roles,
-- projects, 
-- projectTypes,
-- teamates
-- tasks, 
-- filesToTasks, 
-- files
type Getting = 
    "get" :> "users" :> ReqBody '[JSON] (Verify EmptyReq) :> Get '[JSON] [User]
    :<|>
    "get" :> "role-types" :> ReqBody '[JSON] (Verify EmptyReq) :> Get '[JSON] [RoleType]
    :<|>
    "get" :> "options" :> ReqBody '[JSON] (Verify EmptyReq) :> Get '[JSON] [Option]
    :<|>
    "get" :> "option-types" :> ReqBody '[JSON] (Verify EmptyReq) :> Get '[JSON] [OptionType]
    :<|>
    "get" :> "roles" :> ReqBody '[JSON] (Verify EmptyReq) :> Get '[JSON] [Role]
    :<|>
    "get" :> "projects" :> ReqBody '[JSON] (Verify EmptyReq) :> Get '[JSON] [Project]
    :<|>
    "get" :> "project-types" :> ReqBody '[JSON] (Verify EmptyReq) :> Get '[JSON] [ProjectType]
    :<|> 
    "get" :> "teamates" :> ReqBody '[JSON] (Verify EmptyReq) :> Get '[JSON] [Teamate]
    :<|>
    "get" :> "tasks" :> ReqBody '[JSON] (Verify EmptyReq) :> Get '[JSON] [Task]
    :<|>
    "get" :> "task-types" :> ReqBody '[JSON] (Verify EmptyReq) :> Get '[JSON] [TaskType]
    :<|>
    "get" :> "files-to-tasks" :> ReqBody '[JSON] (Verify EmptyReq) :> Get '[JSON] [FileToTask]
    :<|>
    "get" :> "files" :> ReqBody '[JSON] (Verify EmptyReq) :> Get '[JSON] [File]

getServer :: Server Getting
getServer =
    getUsers :<|>
    getRoleTypes :<|>
    getOptions :<|>
    getOptionTypes :<|>
    getRoles :<|>
    getProjects :<|>
    getProjectTypes :<|>
    getTeamates :<|>
    getTasks :<|>
    getTaskTypes :<|>
    getFilesToTasks :<|>
    getFiles
    where
        getUsers :: Verify EmptyReq -> Handler [User]
        getUsers (Verify token EmptyReq)
        getRoleTypes :: Verify EmptyReq -> Handler [RoleType]
        getOptions :: Verify EmptyReq -> Handler [Option]
        getOptionTypes :: Verify EmptyReq -> Handler [OptionType]
        getRoles :: Verify EmptyReq -> Handler [Role]
        getProjects :: Verify EmptyReq -> Handler [Project]
        getProjectTypes :: Verify EmptyReq -> Handler [ProjectTypes]
        getTeamates :: Verify EmptyReq -> Handler [Teamate]
        getTasks :: Verify EmptyReq -> Handler [Task]
        getTaskTypes :: Verify EmptyReq -> Handler [TaskType]
        getFilesToTasks :: Verify EmptyReq -> Handler [FileToTask]
        getFiles :: Verif EmptyReq -> Handler [File]

{-
-- special root types to remove
-- removes optionTypes and all options
-- removes option
-- removes roleType, roles, options, teamates, tasks, filesToTasks
-- removes role, teamates, tasks, filesToTasks
-- removes projectType, projects, roles, teamates, tasks, filesToTasks
-- removes project, roles, teamates, tasks, filesToTasks
-- removes user, teamates, tasks, filesToTasks
-- removes teamate of project, tasks, fileToTasks
-- removes task of teamate, fileToTasks
-- remove taskType, tasks, files to tasks
-- remove fileToTask
-- remove file, fileToTasks
type Removing = 
    "remove" :> "option-deps"
    :<|>
    "remove" :> "option"
    :<|>
    "remove" :> "role-type"
    :<|>
    "remove" :> "role"
    :<|>
    "remove" :> "project-type"
    :<|>
    "remove" :> "project"
    :<|>
    "remove" :> "user"
    :<|>
    "remove" :> "teamate"
    :<|>
    "remove" :> "task"
    :<|>
    "remove" :> "task-type"
    :<|>
    "remove" :> "file-to-task"
    :<|>
    "remove" :> "file"

-- special root types to update
-- updates project progress
-- updates task status
type Updating = 
    "update" :> "progress"
    :<|>
    "update" :> "status"

-- special root types to ads
-- ads new teamate of project
-- ads task of teamate
-- ads project of team
-- ads user role of team
type Adsing = 
    "ads" :> "teamate"
    :<|>
    "ads" :> "task-status"
    :<|>
    "ads" :> "project"
    :<|>
    "ads" :> "user-role"
-}