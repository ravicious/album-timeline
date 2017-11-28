module Main exposing (..)

import Html exposing (..)
import Http
import LastFmApi exposing (AlbumFromWeeklyChart)


type alias Model =
    { albums : List AlbumFromWeeklyChart
    , months : List Month
    , lastFmClient : LastFmApi.Client
    }


type alias Month =
    { month : String
    , start : Int
    , end : Int
    }


type alias Flags =
    { months : List Month
    , currentMonth : Month
    , apiKey : String
    }


type Msg
    = ReceiveWeeklyAlbumChart (Result Http.Error LastFmApi.WeeklyAlbumChart)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


username : String
username =
    "ravicious"


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        lastFmClient =
            LastFmApi.initializeClient flags.apiKey
    in
        { albums = []
        , months = flags.months
        , lastFmClient = lastFmClient
        }
            ! [ Http.send ReceiveWeeklyAlbumChart <|
                    lastFmClient.getWeeklyAlbumChart username
                        flags.currentMonth.start
                        flags.currentMonth.end
              ]


view : Model -> Html Msg
view model =
    ol [] <| List.map viewAlbum <| List.take 15 model.albums


viewAlbum : AlbumFromWeeklyChart -> Html Msg
viewAlbum album =
    li []
        [ text <|
            album.artist
                ++ " - "
                ++ album.name
                ++ " { "
                ++ toString
                    album.playCount
                ++ " scrobbles }"
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveWeeklyAlbumChart (Ok albums) ->
            { model | albums = albums } ! []

        ReceiveWeeklyAlbumChart (Err err) ->
            let
                debugError =
                    Debug.log "Weekly album chart error" err
            in
                model ! []
