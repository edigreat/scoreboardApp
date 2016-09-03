port module Main exposing (..)

import Test.Runner.Node exposing (run)
import Test exposing (..)
import Json.Encode exposing (Value)
import PlayerModuleTest


main : Program Value
main =
    PlayerModuleTest.tests
        |> concat
        |> run emit


port emit : ( String, Value ) -> Cmd msg
