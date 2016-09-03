module PlayerModuleTest exposing (..)

import Expect
import Test exposing (..)
import PlayerModule
import List


tests =
    [ suiteTestPlayerModule ]


playerListTest : List PlayerModule.Player
playerListTest =
    (PlayerModule.Player 1 "First Player" 0)
        :: (PlayerModule.Player 2 "Second Player" 0)
        :: (PlayerModule.Player 3 "Third Player" 0)
        :: []


findPlayer : List PlayerModule.Player -> String -> Bool
findPlayer listPlayer name =
    List.any
        (\player ->
            player.name == name
        )
        listPlayer


suiteTestPlayerModule : Test
suiteTestPlayerModule =
    describe "The Player Module"
        [ describe "Add player"
            [ test "A player should be added to list players" <|
                \() ->
                    let
                        player =
                            "Fourth player"

                        playerList =
                            PlayerModule.addPlayer playerListTest player
                    in
                        (findPlayer playerList player)
                            |> Expect.true "A player must be added to the list"
            , test "Unmodified player list with an empty name" <|
                \() ->
                    let
                        player =
                            ""
                    in
                        Expect.equal playerListTest
                            (PlayerModule.addPlayer playerListTest player)
            ]
        , describe "Edit player"
            [ test "A player name should be edited" <|
                \() ->
                    let
                        playerName =
                            "First Player edited"

                        player =
                            case (List.head playerListTest) of
                                Just player ->
                                    player

                                Nothing ->
                                    PlayerModule.Player 0 "Error Player" 0

                        playerList =
                            PlayerModule.editPlayer playerListTest playerName player.id
                    in
                        (findPlayer playerList playerName)
                            |> Expect.true "A player name must be edited"
            , test "A edited player name should not apper in player List" <|
                \() ->
                    let
                        playerName =
                            "First Player edited"

                        player =
                            case (List.head playerListTest) of
                                Just player ->
                                    player

                                Nothing ->
                                    PlayerModule.Player 0 "Error Player" 0

                        playerList =
                            PlayerModule.editPlayer playerListTest playerName player.id
                    in
                        (findPlayer playerList player.name)
                            |> Expect.false "A player name must not appear"
            ]
        ]
