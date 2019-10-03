module View.View exposing (view)

import Array exposing (..)
import Debug
import FontAwesome exposing (icon, search)
import Html exposing (Html, a, br, button, div, h1, h2, h3, h4, h5, i, img, input, li, p, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, href, id, placeholder, property, scope, src, style, type_, value)
import Html.Attributes.Aria exposing (ariaExpanded, ariaHasPopup, ariaHidden, ariaLabel, ariaLabelledby)
import Html.Events exposing (onClick, onInput)
import Iso8601
import Model.Model exposing (..)
import Round
import Time exposing (Posix, millisToPosix, toHour, toMinute, utc)


viewTaskEntity : Model -> Html Msg
viewTaskEntity model =
    case model of
        Failure ->
            div [ class "card", style "width" "18rem" ]
                [ div [ class "card-body" ]
                    [ h5 [ class "card-title text-primary" ] [ text "Something went wrong." ]
                    ]
                ]

        Typing taskId ->
            viewInput taskId

        Loading taskId ->
            div [ class "card", style "width" "18rem" ]
                [ div [ class "card-body" ]
                    [ h5 [ class "card-title text-primary" ] [ text ("Loading Task No.: " ++ String.fromInt taskId) ]
                    ]
                ]

        Deleting taskId ->
            div [ class "card", style "width" "18rem" ]
                [ div [ class "card-body" ]
                    [ h5 [ class "card-title text-primary" ] [ text ("Deleting Task No.: " ++ String.fromInt taskId) ]
                    ]
                ]

        LoadingList ->
            div [ class "card", style "width" "18rem" ]
                [ div [ class "card-body" ]
                    [ h5 [ class "card-title text-primary" ] [ text "Loading All Tasks" ]
                    ]
                ]

        SuccessEntity taskEntity ->
            div [ class "card", style "width" "18rem" ]
                [ div [ class "card-body" ]
                    [ h5 [ class "card-title text-success" ] [ text (String.concat [ taskEntity.title, " (", String.fromInt taskEntity.id, ")" ]) ]
                    , div [ class "card-title text-success" ] [ text (String.concat [ taskEntity.description ]) ]
                    , div [ class "card-text text-secondary" ] [ text (String.concat [ "Urgency: ", viewUrgency taskEntity.urgency ]) ]
                    , div [ class "card-text text-secondary" ] [ text (String.concat [ "Duration: ", String.fromInt taskEntity.durationMinutes, " minutes" ]) ]
                    , div [ class "card-text text-secondary" ] [ text (String.concat [ "Attention Date: ", taskEntity.attentionDate ]) ]
                    , div [ class "card-text text-secondary" ] [ text (String.concat [ "Deadline: ", taskEntity.deadline ]) ]
                    , div [ class "card-text text-secondary" ] [ text (String.concat [ "Planned Date: ", taskEntity.plannedDate ]) ]
                    , div [ class "card-text text-secondary" ] [ text (String.concat [ "Planned Starting Time: ", iso8601ToHoursMinutes taskEntity.plannedStartingTime ]) ]
                    , div [ class "card-text text-secondary" ] [ text (String.concat [ "Status: ", viewStatus taskEntity.status ]) ]
                    , div [ class "card-text text-secondary" ] [ text (String.concat [ "Created  at: ", iso8601ToDateTime taskEntity.createdAt ]) ]
                    , div [ class "card-text text-secondary" ] [ text (String.concat [ "Updated  at: ", iso8601ToDateTime taskEntity.updatedAt ]) ]
                    ]
                ]

        SuccessEntityList taskEntityList ->
            div [ class "card", style "width" "22rem" ]
                [ div [ class "card-body" ]
                    [ h5 [ class "card-title text-success" ] [ text (String.concat [ "Task List" ]) ]
                    , table [ class "table" ]
                        [ thead []
                            [ tr []
                                [ th [ scope "col" ] [ text "id" ]
                                , th [ scope "col" ] [ text "title" ]
                                , th [ scope "col" ] [ text "status" ]
                                ]
                            ]
                        , tbody [] (List.map (\taskEntity -> viewTaskEntityLine taskEntity) taskEntityList)
                        ]
                    ]
                ]


viewTaskEntityLine : TaskEntity -> Html Msg
viewTaskEntityLine taskEntity =
    tr []
        [ th [ scope "row" ]
            [ button
                [ type_ "button"
                , class "btn btn-link"
                , onClick
                    (GetTaskEntity taskEntity.id)
                ]
                [ text (String.fromInt taskEntity.id) ]
            ]
        , td [] [ text taskEntity.title ]
        , td [] [ text (viewStatus taskEntity.status) ]
        ]


viewStatus : Int -> String
viewStatus status =
    case status of
        0 ->
            "Unplanned"

        1 ->
            "Planned"

        2 ->
            "Done"

        3 ->
            "Deleted"

        _ ->
            "unknown status"



--  enum status: [:unplanned, :planned, :done, :deleted]


viewUrgency : Int -> String
viewUrgency urgency =
    case urgency of
        0 ->
            "Just do it"

        1 ->
            "Plan it"

        2 ->
            "Delegate it"

        3 ->
            "Don't do it"

        _ ->
            "unknown urgency"



--  enum urgency: [:just_do_it, :plan_it, :delegate_it, :dont_do_it]


iso8601ToHoursMinutes : String -> String
iso8601ToHoursMinutes jsonDateTime =
    String.slice 11 16 jsonDateTime


iso8601ToDateTime : String -> String
iso8601ToDateTime jsonDateTime =
    String.slice 0 10 jsonDateTime



-- 2012-04-23T18:25:43.511Z
-- 012345678901234567890123
--           1         2


timeToString : Int -> Int -> String
timeToString time timezone =
    let
        milliseconds =
            (time + timezone) * 1000

        posix =
            Time.millisToPosix milliseconds

        hours =
            Time.toHour utc posix

        minutes =
            Time.toMinute utc posix

        seconds =
            Time.toSecond utc posix

        hoursString =
            String.fromInt hours

        minutesString =
            (if minutes < 10 then
                "0"

             else
                ""
            )
                ++ String.fromInt minutes

        secondsString =
            (if seconds < 10 then
                "0"

             else
                ""
            )
                ++ String.fromInt seconds
    in
    String.concat [ hoursString, ":", minutesString, ":", secondsString ]


viewInput : Id -> Html Msg
viewInput taskId =
    div [ class "card", style "width" "18rem" ]
        [ div [ class "card-body" ]
            [ h5 [ class "card-title text-primary" ] [ text "Task Manager" ]
            , p [ class "card-title" ]
                [ span [ class "text-secondary" ] [ text "Enter a task id and then press " ]
                , span
                    [ class "fa fa-search text-primary"
                    , ariaHidden True
                    , ariaLabel "Search"
                    ]
                    []
                , span [ class "text-primary" ] [ text " Search" ]
                , span [ class "text-secondary" ] [ text " to get the weather." ]
                ]
            , div [ class "input-group mb-3" ]
                [ input
                    [ value (String.fromInt taskId)
                    , onInput GetTaskId
                    , type_ "number"
                    , class "form-control"
                    , placeholder "Try \"Rome\""
                    ]
                    []
                , div [ class "input-group-append" ]
                    [ button
                        [ class "btn btn-outline-primary"
                        , type_ "button"
                        , ariaLabel "Left Align"
                        , onClick (GetTaskEntity taskId)
                        ]
                        [ span
                            [ class "fa fa-search text-primary"
                            , ariaHidden True
                            , ariaLabel "Search"
                            ]
                            []
                        , text " Search"
                        ]
                    ]
                ]
            ]
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    let
        taskId =
            case model of
                SuccessEntity taskEntity ->
                    taskEntity.id

                _ ->
                    0
    in
    div [ class "dropdown" ]
        [ button
            [ class "btn btn-outline-primary dropdown-toggle"
            , type_ "button"
            , id "dropdownMenuButton"
            , attribute "data-toggle" "dropdown"
            , ariaHasPopup "menu"
            , ariaExpanded "false"
            ]
            [ text "Task Functions" ]
        , div
            [ class "dropdown-menu"
            , ariaLabelledby "dropdownMenuButton"
            ]
            [ a
                [ class "dropdown-item"
                , href "#"
                ]
                [ text "New Task" ]
            , a
                [ class
                    ("dropdown-item"
                        ++ (if taskId == 0 then
                                " disabled"

                            else
                                ""
                           )
                    )
                , href "#"
                ]
                [ text "Edit Task" ]
            , a
                [ class
                    ("dropdown-item"
                        ++ (if taskId == 0 then
                                " disabled"

                            else
                                ""
                           )
                    )
                , href "#"
                , onClick (DeleteTaskEntity taskId)
                ]
                [ text "Delete Task" ]
            , div [ class "dropdown-divider" ] []
            , a
                [ class "dropdown-item"
                , href "#"
                , onClick (GetTaskId "")
                ]
                [ text "Get Task by Id" ]
            , div [ class "dropdown-divider" ] []
            , a
                [ class "dropdown-item"
                , href "#"
                , onClick GetTaskEntityList
                ]
                [ text "Get All Tasks" ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ p [] []
        , viewMenu model
        , p [] []
        , viewTaskEntity
            model
        ]
