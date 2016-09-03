module PlayerModuleTest exposing (..)

import Expect
import Test exposing (..)
import PlayerModule
import List


tests =
    [ addPlayerTest ]


findPlayer : List PlayerModule.Player -> String -> Bool
findPlayer listPlayer name =
    List.any
        (\player ->
            player.name == name
        )
        listPlayer


addPlayerTest : Test
addPlayerTest =
    test "A player should be added to list players" <|
        \() ->
            let
                players =
                    (PlayerModule.addPlayer [] player)

                player =
                    "First player"
            in
                Expect.equal True (findPlayer players player)
