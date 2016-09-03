module PlayerModuleTest exposing (..)

import Expect
import Test exposing (..)
import PlayerModule
import List


tests =
    [ suiteTestPlayerModule ]



-- init data


playerListTest : List PlayerModule.Player
playerListTest =
    (PlayerModule.Player 1 "First Player" 0)
        :: (PlayerModule.Player 2 "Second Player" 0)
        :: (PlayerModule.Player 3 "Third Player" 0)
        :: []


playListTest : List PlayerModule.Play
playListTest =
    (PlayerModule.Play 1 1 "First Player" 2)
        :: (PlayerModule.Play 2 1 "First Player" 2)
        :: (PlayerModule.Play 3 2 "First Player" 3)
        :: (PlayerModule.Play 4 3 "First Player" 2)
        :: (PlayerModule.Play 5 3 "First Player" 3)
        :: []



-- auxilary functions for tests


findPlayer : List PlayerModule.Player -> String -> Bool
findPlayer listPlayer name =
    List.any
        (\player ->
            player.name == name
        )
        listPlayer


findPlayByPlayerAndId : List PlayerModule.Play -> String -> Int -> Bool
findPlayByPlayerAndId listPlay name id =
    List.any
        (\play ->
            (play.name == name && play.playerId == id)
        )
        listPlay



-- test suite


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
                            |> Expect.true "The player name must be edited"
            , test "A edited player name should not appear in player List" <|
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
                            |> Expect.false "The old player's name must not appear"
            ]
        , describe "Edit play"
            [ test "A play should be edited" <|
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

                        editedPlayList =
                            PlayerModule.editPlay playListTest playerName player.id
                    in
                        (findPlayByPlayerAndId editedPlayList playerName player.id)
                            |> Expect.true "The new player name must be edited"
            , test "A edited player's name should not appear in playList " <|
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

                        editedPlayList =
                            PlayerModule.editPlay playListTest playerName player.id
                    in
                        (findPlayByPlayerAndId editedPlayList player.name player.id)
                            |> Expect.false "The old player's name must not appear in playList"
            ]
        ]
