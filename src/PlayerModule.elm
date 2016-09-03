module PlayerModule
    exposing
        ( Player
        , playerForm
        , MsgPlayer(..)
        , Play
        , editPlayer
        , addPlayer
        , editPlay
        , playerSection
        , scorePlayer
        , playSection
        , deletedPlay
        , updateDeletePlayInPlayer
        )

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : Int
    , name : String
    , points : Int
    }


type MsgPlayer
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletedPlay Play



-- bussiness functions


updateDeletePlayInPlayer : Play -> List Player -> List Player
updateDeletePlayInPlayer playModel playerList =
    playerList
        |> List.map
            (\player ->
                if player.id == playModel.playerId then
                    { player | points = player.points - playModel.points }
                else
                    player
            )


deletedPlay : Play -> List Play -> List Play
deletedPlay playModel listPlay =
    List.filter (\p -> playModel.id /= p.id) listPlay


addPlayer : List Player -> String -> List Player
addPlayer players name =
    let
        player =
            Player (List.length players) name 0
    in
        if not (String.isEmpty name) then
            player :: players
        else
            players


editPlayer : List Player -> String -> Int -> List Player
editPlayer listPlayers namePlayer idPlayer =
    List.map
        (\player ->
            if player.id == idPlayer then
                { player | name = namePlayer }
            else
                player
        )
        listPlayers


editPlay : List Play -> String -> Int -> List Play
editPlay listPlay namePlayer idPlayer =
    List.map
        (\play ->
            if play.playerId == idPlayer then
                { play | name = namePlayer }
            else
                play
        )
        listPlay


scorePlayer : Int -> Int -> List Player -> List Player
scorePlayer idPlayer points playerList =
    List.map
        (\player ->
            if player.id == idPlayer then
                { player | points = player.points + points }
            else
                player
        )
        playerList



-- visual components


playSection : List Play -> Html MsgPlayer
playSection playListModel =
    div []
        [ playListHeader
        , playList playListModel
        ]


playListHeader : Html MsgPlayer
playListHeader =
    header []
        [ div [] [ text "Plays" ]
        , div [] [ text "Points" ]
        ]


playList : List Play -> Html MsgPlayer
playList playList =
    playList
        |> List.map play
        |> ul []


play : Play -> Html MsgPlayer
play play =
    li []
        [ i
            [ class "remove"
            , onClick (DeletedPlay play)
            ]
            []
        , div [] [ text play.name ]
        , div [] [ text (toString play.points) ]
        ]


playerSection : List Player -> List Play -> Html MsgPlayer
playerSection playerListModel playListModel =
    div []
        [ playerListHeader
        , playerList playerListModel
        , pointTotal playListModel
        ]


pointTotal : List Play -> Html MsgPlayer
pointTotal playList =
    let
        total =
            List.map .points playList |> List.sum
    in
        footer []
            [ div [] [ text "Total:" ]
            , div [] [ text (toString total) ]
            ]


playerListHeader : Html MsgPlayer
playerListHeader =
    header []
        [ div [] [ text "Name" ]
        , div [] [ text "Points" ]
        ]


playerList : List Player -> Html MsgPlayer
playerList playerList =
    List.map player playerList
        |> ul []


player : Player -> Html MsgPlayer
player player =
    li []
        [ i
            [ class "edit"
            , onClick (Edit player)
            ]
            []
        , div []
            [ text player.name ]
        , button
            [ type' "button"
            , onClick (Score player 2)
            ]
            [ text "2 pt" ]
        , button
            [ type' "button"
            , onClick (Score player 3)
            ]
            [ text "3 pt" ]
        , div []
            [ text (toString player.points) ]
        ]


playerForm : String -> Html MsgPlayer
playerForm name =
    Html.form [ onSubmit Save ]
        [ input
            [ type' "text"
            , placeholder "Add/Edit player ..."
            , onInput Input
            , value name
            ]
            []
        , button [ type' "submit" ] [ text "Save" ]
        , button
            [ type' "button"
            , onClick Cancel
            ]
            [ text "Cancel" ]
        ]
