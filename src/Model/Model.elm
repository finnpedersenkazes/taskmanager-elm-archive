module Model.Model exposing (Id, Model(..), Msg(..), TaskEntity, TaskEntityList, init, initTaskEntity)

import Http


type alias Id =
    Int


type Msg
    = GetTaskEntity Id
    | GetTaskId String
    | GetTaskEntityList
    | AfterGetTaskEntity (Result Http.Error TaskEntity)
    | AfterGetTaskEntityList (Result Http.Error TaskEntityList)
    | CreateTaskEntity
    | AfterCreateTaskEntity (Result Http.Error TaskEntity)
    | EditTaskEntity TaskEntity
    | AfterSaveTaskEntity (Result Http.Error TaskEntity)
    | DeleteTaskEntity Id
    | AfterDeleteTaskEntry (Result Http.Error ())


type Model
    = Failure
    | NotImplementedYet
    | Typing Id
    | Loading Id
    | LoadingList
    | Deleting Id
    | CreatingEntity TaskEntity
    | EditingEntity TaskEntity
    | SuccessEntity TaskEntity
    | SuccessEntityList TaskEntityList



-- https://github.com/finnpedersenkazes/taskmanager02


type alias TaskEntityList =
    List TaskEntity


type alias TaskEntity =
    { id : Id
    , title : String
    , description : String
    , urgency : Int
    , durationMinutes : Int
    , attentionDate : String
    , deadline : String
    , plannedDate : String
    , plannedStartingTime : String
    , status : Int
    , createdAt : String
    , updatedAt : String
    }


initTaskEntity : TaskEntity
initTaskEntity =
    TaskEntity 0 "Type your Title here" "" 0 0 "" "" "" "" 0 "" ""


init : () -> ( Model, Cmd Msg )
init _ =
    ( Typing 26
    , Cmd.none
    )
