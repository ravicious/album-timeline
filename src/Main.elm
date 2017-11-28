module Main exposing (..)

import Html exposing (..)
import Http
import Json.Decode


type alias Model =
    { albums : List Album
    , months : List Month
    }


type alias Album =
    { artist : String, name : String, playCount : Int }


type alias Month =
    { month : String
    , start : Int
    , end : Int
    }


type alias Flags =
    { months : List Month
    }


type Msg
    = ReceiveWeeklyAlbumChart (Result Http.Error (List Album))


main : Program Flags Model Msg
main =
    Html.programWithFlags
        -- { init = { albums = [] } ! [ Http.send ReceiveWeeklyAlbumChart <| getWeeklyAlbumChart ]
        { init = \flags -> { albums = [], months = flags.months } ! []
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


view : Model -> Html Msg
view model =
    ol [] <| List.map viewAlbum model.albums


viewAlbum : Album -> Html Msg
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


weeklyAlbumChartUrl : String
weeklyAlbumChartUrl =
    "http://ws.audioscrobbler.com/2.0/?method=user.getweeklyalbumchart&user=ravicious&api_key=917d76183f70ca5d5b5a24abc13e2531&format=json"


getWeeklyAlbumChart : Http.Request (List Album)
getWeeklyAlbumChart =
    Http.get weeklyAlbumChartUrl <|
        Json.Decode.at [ "weeklyalbumchart", "album" ] <|
            Json.Decode.list <|
                Json.Decode.map3 Album
                    (Json.Decode.at [ "artist", "#text" ] Json.Decode.string)
                    (Json.Decode.field "name" Json.Decode.string)
                    (Json.Decode.field "playcount" Json.Decode.string
                        |> Json.Decode.andThen
                            (\string ->
                                case String.toInt string of
                                    Ok int ->
                                        Json.Decode.succeed int

                                    Err err ->
                                        Json.Decode.fail err
                            )
                    )
