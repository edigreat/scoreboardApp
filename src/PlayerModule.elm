module PlayerModule
    exposing
        ( Player
        , playerForm
        , MsgPlayer(..)
        , Play
        , editPlayer
        , addPlayer
        , editPlay
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
    | DeletePlay Play



-- bussiness functions


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



-- visual components


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
