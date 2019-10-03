module TestsView exposing (testConversionDirection, testGetCompassDirection, testTimeToString)

import Array exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (Test, describe, fuzz, only, skip, test, todo)
import Time exposing (Posix, millisToPosix, toHour, toMinute, utc)
import View.View exposing (compassDirections, convertDirection, timeToString)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


testView : Test
testView =
    todo "test functions in View"
