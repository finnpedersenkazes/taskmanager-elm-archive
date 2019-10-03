module Update.Update exposing (api, getTaskEntity, getTaskEntityList, taskEntityDecoder, taskEntityListDecoder, update)

import Debug exposing (log)
import Http
import Json.Decode as Decode exposing (Decoder, andThen, fail, field, float, int, list, map, map2, map5, string, succeed)
import Json.Decode.Pipeline as Pipeline exposing (custom, hardcoded, optional, required)
import Model.Model exposing (..)
import Update.Secrets exposing (appId)


api : String
api =
    "https://taskmanager01-api.herokuapp.com/tasks"



{-
   https://taskmanager01-api.herokuapp.com/tasks
   https://test-app-vue-01.herokuapp.com/api/v1/tasks/26
-}


deleteTaskEntity : Id -> Cmd Msg
deleteTaskEntity taskId =
    let
        urlTaskEntity =
            api ++ "/" ++ String.fromInt taskId
    in
    Http.request
        { method = "DELETE"
        , headers =
            [ Http.header "X-User-Email" "finn@gmail.com"
            , Http.header "X-User-Token" "o747qePsDnFn8KsjCaAn"
            ]
        , url = urlTaskEntity
        , body = Http.emptyBody
        , expect = Http.expectWhatever AfterDeleteTaskEntry
        , timeout = Nothing
        , tracker = Nothing
        }


getTaskEntity : Id -> Cmd Msg
getTaskEntity taskId =
    let
        urlTaskEntity =
            api ++ "/" ++ String.fromInt taskId
    in
    Http.get
        { url = urlTaskEntity
        , expect = Http.expectJson NewTaskEntity taskEntityDecoder
        }


getTaskEntityList : Cmd Msg
getTaskEntityList =
    let
        urlTaskEntity =
            api
    in
    Http.get
        { url = urlTaskEntity
        , expect = Http.expectJson NewTaskEntityList taskEntityListDecoder
        }


taskEntityListDecoder : Decoder TaskEntityList
taskEntityListDecoder =
    Decode.list taskEntityDecoder


taskEntityDecoder : Decoder TaskEntity
taskEntityDecoder =
    Decode.succeed TaskEntity
        |> required "id" int
        |> required "title" string
        |> optional "description" string ""
        |> optional "urgency" int 0
        |> optional "duration_minutes" int 0
        |> optional "attention_date" string ""
        |> optional "deadline" string ""
        |> optional "planned_date" string ""
        |> optional "planned_starting_time" string ""
        |> optional "status" int 0
        |> required "created_at" string
        |> required "updated_at" string


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTaskId idString ->
            let
                id =
                    Maybe.withDefault 0 (String.toInt idString)
            in
            ( Typing id, Cmd.none )

        GetTaskEntity id ->
            if id == 0 then
                ( Typing 0, Cmd.none )

            else
                ( Loading id, getTaskEntity id )

        DeleteTaskEntity id ->
            if id == 0 then
                ( Failure, Cmd.none )

            else
                ( Deleting id, deleteTaskEntity id )

        GetTaskEntityList ->
            ( LoadingList, getTaskEntityList )

        NewTaskEntity (Ok newTaskEntity) ->
            ( SuccessEntity newTaskEntity, Cmd.none )

        NewTaskEntity (Err _) ->
            ( Failure, Cmd.none )

        NewTaskEntityList (Ok newTaskEntityList) ->
            ( SuccessEntityList newTaskEntityList, Cmd.none )

        NewTaskEntityList (Err _) ->
            ( Failure, Cmd.none )

        AfterDeleteTaskEntry (Ok _) ->
            ( LoadingList, getTaskEntityList )

        AfterDeleteTaskEntry (Err _) ->
            ( Failure, Cmd.none )
