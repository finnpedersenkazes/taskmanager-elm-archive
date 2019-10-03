module Model.Model exposing (Id, Model(..), Msg(..), TaskEntity, TaskEntityList, init)

import Http


type alias Id =
    Int


type Msg
    = GetTaskEntity Id
    | GetTaskId String
    | GetTaskEntityList
    | NewTaskEntity (Result Http.Error TaskEntity)
    | NewTaskEntityList (Result Http.Error TaskEntityList)
    | DeleteTaskEntity Id
    | AfterDeleteTaskEntry (Result Http.Error ())


type Model
    = Failure
    | Typing Id
    | Loading Id
    | LoadingList
    | Deleting Id
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( Typing 26
    , Cmd.none
    )
