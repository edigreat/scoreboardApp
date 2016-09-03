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

        _ ->
            model



--view


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
            []

        newPlays =
            []

        --newPlays =
        --  PlayerModule.editPlay (model.listPlay model.name id)
        --tot =
        --  PlayerModule.editPlayer (model.listPlayers model.name id)
    in
        { model
            | players = newPlayers
            , plays = newPlays
            , name = ""
            , playerId = Nothing
        }


view : Model -> Html PlayerModule.MsgPlayer
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keepper" ]
        , PlayerModule.playerForm model.name
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
