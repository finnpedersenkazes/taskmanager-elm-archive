module TestsUpdate exposing (testTaskEntityDecoder)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode exposing (Decoder, andThen, decodeString, fail, field, float, int, list, map, map2, map5, string, succeed)
import Model.Model exposing (Id, Model(..), Msg(..), TaskEntity, init)
import Test exposing (Test, describe, fuzz, only, skip, test, todo)
import Time exposing (Posix, millisToPosix, toHour, toMinute, utc)
import Update.Update exposing (api, getTaskEntity, taskEntityDecoder, update)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


jsonResponse =
    "{\"id\":26,\"title\":\"Scabbers\",\"description\":\"You can't transmit the transmitter without quantifying the open-source COM card!\",\"urgency\":\"dont_do_it\",\"duration_minutes\":90,\"attention_date\":\"2018-01-17\",\"deadline\":\"2018-01-22\",\"planned_date\":\"2018-01-19\",\"planned_starting_time\":\"2000-01-01T19:30:00.000Z\",\"status\":\"planned\",\"created_at\":\"2018-01-17T08:54:53.625Z\",\"updated_at\":\"2018-01-17T08:54:53.625Z\"}"


expectedTaskEntity : TaskEntity
expectedTaskEntity =
    TaskEntity
        26
        "Scabbers"
        "You can't transmit the transmitter without quantifying the open-source COM card!"
        "dont_do_it"
        90
        "2018-01-17"
        "2018-01-22"
        "2018-01-19"
        "2000-01-01T19:30:00.000Z"
        "planned"
        "2018-01-17T08:54:53.625Z"
        "2018-01-17T08:54:53.625Z"


testTaskEntityDecoder : Test
testTaskEntityDecoder =
    describe "Test taskEntityDecoder"
        [ test "Test decoding valid json response" <|
            \_ ->
                case decodeString taskEntityDecoder jsonResponse of
                    Ok taskEntity ->
                        Expect.equal expectedTaskEntity taskEntity

                    Err err ->
                        Expect.fail ("Something went wrong: " ++ Decode.errorToString err)
        ]
