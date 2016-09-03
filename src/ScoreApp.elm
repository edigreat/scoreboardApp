module ScoreApp exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String
import Html.App as App
import PlayerModule


-- model


type alias Model =
    { players : List PlayerModule.Player
    , name : String
    , playerId : Maybe Int
    , plays : List PlayerModule.Play
    }


initModel : Model
initModel =
    { players = []
    , name = ""
    , playerId = Nothing
    , plays = []
    }


update : PlayerModule.MsgPlayer -> Model -> Model
update msg model =
    case msg of
        PlayerModule.Input name ->
            { model | name = name }

        PlayerModule.Cancel ->
            { model | name = " ", playerId = Nothing }

        PlayerModule.Save ->
            if (String.isEmpty model.name) then
                model
            else
                save model

        PlayerModule.Score player points ->
            score model player points

        PlayerModule.Edit player ->
            { model | name = player.name, playerId = Just player.id }

        PlayerModule.DeletedPlay play ->
            { model
                | plays = (PlayerModule.deletedPlay play model.plays)
                , players = (PlayerModule.updateDeletePlayInPlayer play model.players)
            }


score : Model -> PlayerModule.Player -> Int -> Model
score model player points =
    let
        newPlayers =
            PlayerModule.scorePlayer player.id points model.players

        play =
            PlayerModule.Play (List.length model.plays) player.id player.name points
    in
        { model
            | players = newPlayers
            , plays = play :: model.plays
        }


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit model id

        Nothing ->
            { model | players = (PlayerModule.addPlayer model.players model.name), name = "" }


edit : Model -> Int -> Model
edit model id =
    let
        newPlayers =
            PlayerModule.editPlayer model.players model.name id

        newPlays =
            PlayerModule.editPlay model.plays model.name id
    in
        { model
            | players = newPlayers
            , plays = newPlays
            , name = ""
            , playerId = Nothing
        }



--view


view : Model -> Html PlayerModule.MsgPlayer
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keepper" ]
        , PlayerModule.playerSection model.players model.plays
        , PlayerModule.playerForm model.name
        , PlayerModule.playSection model.plays
        , hr [] []
        , p [] [ text (toString model) ]
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
