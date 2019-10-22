module View.View exposing (view)

import Array exposing (..)
import Debug
import FontAwesome exposing (icon, search)
import Html exposing (Html, a, br, button, div, h1, h2, h3, h4, h5, i, img, input, label, li, p, span, table, tbody, td, text, textarea, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, href, id, placeholder, property, rows, scope, src, style, type_, value)
import Html.Attributes.Aria exposing (ariaExpanded, ariaHasPopup, ariaHidden, ariaLabel, ariaLabelledby, role)
import Html.Events exposing (onClick, onInput, onSubmit)
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

        NotImplementedYet ->
            div [ class "card", style "width" "18rem" ]
                [ div [ class "card-body" ]
                    [ h5 [ class "card-title text-primary" ] [ text "This function has not been implemented yet." ]
                    ]
                ]

        HomePage ->
            div [ class "card", style "width" "18rem" ]
                [ div [ class "card-body" ]
                    [ h5 [ class "card-title text-primary" ] [ text "Welcome to Task Manager." ]
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

        CreatingEntity taskEntity ->
            div [ class "card", style "width" "18rem" ]
                [ div [ class "card-body" ]
                    [ h5 [ class "card-title text-primary" ] [ text "Creating a Task" ]
                    ]
                ]

        EditingEntity taskEntity ->
            div [ class "card", style "width" "18rem" ]
                [ div [ class "card-body" ]
                    [ h5 [ class "card-title text-primary" ]
                        [ text
                            (if taskEntity.id == 0 then
                                "Create a Task"

                             else
                                "Editing a Task"
                            )
                        ]
                    , viewForm taskEntity
                    ]
                ]

        DisplayingEntity taskEntity ->
            div [ class "card", style "width" "18rem" ]
                [ div [ class "card-body" ]
                    [ viewTaskEntityField taskEntity Title True
                    , viewTaskEntityField taskEntity Description True
                    , viewTaskEntityField taskEntity Status True
                    , viewTaskEntityField taskEntity Urgency (taskEntity.status == 0)
                    , viewTaskEntityField taskEntity Duration True
                    , viewTaskEntityField taskEntity AttentionDate (taskEntity.status /= 1 && taskEntity.status /= 2)
                    , viewTaskEntityField taskEntity Deadline (taskEntity.status /= 1 && taskEntity.status /= 2)
                    , viewTaskEntityField taskEntity PlannedDate (taskEntity.status /= 0)
                    , viewTaskEntityField taskEntity PlannedStartingTime (taskEntity.status /= 0)
                    ]
                ]

        DisplayingEntityList taskEntityList ->
            let
                sortedList =
                    List.sortBy .status taskEntityList
            in
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
                        , tbody [] (List.map (\taskEntity -> viewTaskEntityLine taskEntity) sortedList)
                        ]
                    ]
                ]


viewTaskEntityField : TaskEntity -> TaskEntityField -> Bool -> Html Msg
viewTaskEntityField taskEntity taskField displayField =
    if displayField then
        case taskField of
            Title ->
                h5 [ class "card-title text-success" ] [ text (String.concat [ taskEntity.title, " (", String.fromInt taskEntity.id, ")" ]) ]

            Description ->
                div [ class "card-title text-success" ] [ text (String.concat [ taskEntity.description ]) ]

            Status ->
                div [ class "card-text text-secondary" ] [ text (String.concat [ "Status: ", viewStatus taskEntity.status ]) ]

            Urgency ->
                div [ class "card-text text-secondary" ] [ text (String.concat [ "Urgency: ", viewUrgency taskEntity.urgency ]) ]

            Duration ->
                div [ class "card-text text-secondary" ] [ text (String.concat [ "Duration: ", String.fromInt taskEntity.durationMinutes, " minutes" ]) ]

            AttentionDate ->
                div [ class "card-text text-secondary" ] [ text (String.concat [ "Attention Date: ", taskEntity.attentionDate ]) ]

            Deadline ->
                div [ class "card-text text-secondary" ] [ text (String.concat [ "Deadline: ", taskEntity.deadline ]) ]

            PlannedDate ->
                div [ class "card-text text-secondary" ] [ text (String.concat [ "Planned Date: ", taskEntity.plannedDate ]) ]

            PlannedStartingTime ->
                div [ class "card-text text-secondary" ] [ text (String.concat [ "Planned Starting Time: ", iso8601ToHoursMinutes taskEntity.plannedStartingTime ]) ]

    else
        span [] []


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
            "In the bin"

        _ ->
            "unknown status"


viewStatusButton : Int -> String
viewStatusButton status =
    case status of
        0 ->
            "Unplan Task"

        1 ->
            "Plan Task"

        2 ->
            " Mark Task as Done"

        3 ->
            "Put Task in the bin"

        _ ->
            "unknown status for button"



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
                , span [ class "text-secondary" ] [ text " to get the task." ]
                ]
            , div [ class "input-group mb-3" ]
                [ input
                    [ value (String.fromInt taskId)
                    , onInput GetTaskId
                    , type_ "number"
                    , class "form-control"
                    , placeholder "0"
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


viewForm : TaskEntity -> Html Msg
viewForm taskEntity =
    Html.form
        [ onSubmit
            (if taskEntity.id == 0 then
                CreateTaskEntity taskEntity

             else
                SaveTaskEntity taskEntity
            )
        ]
        [ div [ class "form-group" ]
            [ label []
                [ text "Title" ]
            , input
                [ type_ "text"
                , placeholder "Title"
                , onInput (SetTaskEntity taskEntity Title)
                , value taskEntity.title
                , class "form-control"
                ]
                []
            ]
        , div [ class "form-group" ]
            [ label []
                [ text "Description" ]
            , textarea
                [ placeholder "Description"
                , onInput (SetTaskEntity taskEntity Description)
                , value taskEntity.description
                , class "form-control"
                , rows 4
                ]
                []
            ]
        , viewTaskEntityField taskEntity Status True
        , div [ class "form-group" ]
            [ button
                [ type_ "submit"
                , class "btn btn-outline-primary"
                ]
                [ text
                    (if taskEntity.id == 0 then
                        "Create"

                     else
                        "Save"
                    )
                ]
            , span [] [ text " " ]
            , button
                [ class "btn btn-outline-warning"
                , type_ "button"
                , onClick (GetTaskEntity taskEntity.id)
                ]
                [ text "Cancel" ]
            ]
        ]


viewTaskEntityMenuButton : Model -> Html Msg
viewTaskEntityMenuButton model =
    let
        taskEntity =
            case model of
                DisplayingEntity taskEntity2 ->
                    taskEntity2

                _ ->
                    initTaskEntity

        taskId =
            taskEntity.id
    in
    div [ class "btn-group", role "group" ]
        [ button
            [ class "btn btn-outline-primary dropdown-toggle"
            , type_ "button"
            , id "dropdownTaskMenuButton"
            , attribute "data-toggle" "dropdown"
            , ariaHasPopup "menu"
            , ariaExpanded "false"
            ]
            [ text "Task" ]
        , div
            [ class "dropdown-menu"
            , ariaLabelledby "dropdownMenuButton"
            ]
            [ a
                [ class "dropdown-item"
                , href "#"
                , onClick (SetTaskEntity initTaskEntity Title "")
                ]
                [ text "New Task" ]
            , a
                [ class "dropdown-item"
                , href "#"
                , onClick (GetTaskId "")
                ]
                [ text "Search for Task" ]
            , div [ class "dropdown-divider" ] []
            , a
                [ class "dropdown-item"
                , href "#"
                , onClick GetTaskEntityList
                ]
                [ text "Get All Tasks" ]
            ]
        ]


includeMenuItem : TaskEntity -> Int -> Bool
includeMenuItem taskEntity status =
    not (taskEntity.status == status)


viewMenuItem : TaskEntity -> Int -> Html Msg
viewMenuItem taskEntity status =
    let
        statusText =
            viewStatus status

        buttonText =
            viewStatusButton status

        showMenuItem =
            taskEntity.status /= status
    in
    a
        [ class
            ("dropdown-item"
                ++ (if taskEntity.status == status then
                        " disabled"

                    else
                        ""
                   )
            )
        , href "#"
        , onClick (SetTaskEntity taskEntity Status statusText)
        ]
        [ text buttonText ]


appendPart1andPart2 : TaskEntity -> List (Html Msg)
appendPart1andPart2 taskEntity =
    List.append (viewTaskFunctionsMenuPart1 taskEntity) (viewTaskFunctionsMenuPart2 taskEntity)


viewTaskFunctionsMenuPart1 : TaskEntity -> List (Html Msg)
viewTaskFunctionsMenuPart1 taskEntity =
    List.map (\i -> viewMenuItem taskEntity i) [ 0, 1, 2, 3 ]


viewTaskFunctionsMenuPart2 : TaskEntity -> List (Html Msg)
viewTaskFunctionsMenuPart2 taskEntity =
    [ div [ class "dropdown-divider" ] []
    , a
        [ class "dropdown-item"
        , href "#"
        , onClick (SetTaskEntity taskEntity Title taskEntity.title)
        ]
        [ text "Edit Task" ]
    , a
        [ class
            ("dropdown-item"
                ++ (if taskEntity.status /= 3 then
                        " disabled"

                    else
                        ""
                   )
            )
        , href "#"
        , onClick (DeleteTaskEntity taskEntity.id)
        ]
        [ text
            ("Delete Task"
                ++ (if taskEntity.id > 0 then
                        " No. " ++ String.fromInt taskEntity.id

                    else
                        ""
                   )
            )
        ]
    ]


viewTaskFunctionsMenuButton : Model -> Html Msg
viewTaskFunctionsMenuButton model =
    let
        taskEntity =
            case model of
                DisplayingEntity taskEntity2 ->
                    taskEntity2

                _ ->
                    initTaskEntity
    in
    div [ class "btn-group", role "group" ]
        [ button
            [ class
                ("btn btn-outline-primary dropdown-toggle"
                    ++ (if taskEntity.id == 0 then
                            " disabled"

                        else
                            ""
                       )
                )
            , type_ "button"
            , id "dropdownFunctionMenuButton"
            , attribute "data-toggle" "dropdown"
            , ariaHasPopup "menu"
            , ariaExpanded "false"
            ]
            [ text "Functions" ]
        , div
            [ class "dropdown-menu"
            , ariaLabelledby "dropdownMenuButton"
            ]
            (appendPart1andPart2 taskEntity)
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    let
        taskEntity =
            case model of
                DisplayingEntity taskEntity2 ->
                    taskEntity2

                _ ->
                    initTaskEntity

        taskId =
            taskEntity.id
    in
    div [ class "p-2" ]
        [ div [ class "btn-toolbar", role "toolbar", ariaLabel "Toolbar" ]
            [ div [ class "btn-group mr-2", role "group", ariaLabel "TaskEntity Menu Buttons" ]
                [ viewTaskEntityMenuButton model
                , viewTaskFunctionsMenuButton model
                ]
            , div [ class "btn-group mr-2", role "group", ariaLabel "TaskEntity Menu Buttons" ]
                [ button
                    [ type_ "button"
                    , class "btn btn-outline-primary"
                    , href "#"
                    , onClick GoToHomePage
                    ]
                    [ text "Home" ]
                ]
            ]
        ]


solidBlackBorder : Html.Attribute Msg
solidBlackBorder =
    style "border" "1px solid black"


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ div [ class "row" ]
            [ div [ class "col" ] [ viewMenu model ] ]
        , div [ class "row" ]
            [ div [ class "col" ] [ viewTaskEntity model ] ]
        ]
