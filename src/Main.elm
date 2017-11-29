module Main exposing (..)

import Html exposing (..)
import Http
import LastFmApi exposing (AlbumFromWeeklyChart)
import Array exposing (Array)


type alias Model =
    { months : Array Month
    , lastFmClient : LastFmApi.Client
    , numberOfMonths : Int
    , monthsWithAlbums : List MonthWithAlbums
    }


type alias MonthWithAlbums =
    { month : Month
    , albums : List AlbumFromWeeklyChart
    }


type alias Month =
    { month : String
    , start : Int
    , end : Int
    }


type alias Flags =
    { months : Array Month
    , currentMonth : Month
    , apiKey : String
    }


type Msg
    = ReceiveWeeklyAlbumChartFromMonth Month (Result Http.Error LastFmApi.WeeklyAlbumChart)


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
        { monthsWithAlbums = []
        , months = flags.months
        , lastFmClient = lastFmClient
        , numberOfMonths = 0
        }
            ! [ Http.send (ReceiveWeeklyAlbumChartFromMonth flags.currentMonth) <|
                    lastFmClient.getWeeklyAlbumChart username
                        flags.currentMonth.start
                        flags.currentMonth.end
              ]


view : Model -> Html Msg
view model =
    div [] (List.map viewMonthWithAlbums model.monthsWithAlbums)


viewMonthWithAlbums : MonthWithAlbums -> Html Msg
viewMonthWithAlbums { month, albums } =
    div []
        [ strong [] [ text month.month ]
        , ol [] <| List.map viewAlbum <| List.take 15 albums
        ]


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
        ReceiveWeeklyAlbumChartFromMonth month (Ok albums) ->
            let
                maybeNextMonth =
                    Array.get (model.numberOfMonths + 1) model.months
            in
                { model
                    | monthsWithAlbums = { month = month, albums = albums } :: model.monthsWithAlbums
                    , numberOfMonths = model.numberOfMonths + 1
                }
                    ! [ if model.numberOfMonths < 6 then
                            Maybe.map
                                (\nextMonth ->
                                    Http.send (ReceiveWeeklyAlbumChartFromMonth nextMonth) <|
                                        model.lastFmClient.getWeeklyAlbumChart username
                                            nextMonth.start
                                            nextMonth.end
                                )
                                maybeNextMonth
                                |> Maybe.withDefault Cmd.none
                        else
                            Cmd.none
                      ]

        ReceiveWeeklyAlbumChartFromMonth _ (Err err) ->
            let
                debugError =
                    Debug.log "Weekly album chart error" err
            in
                model ! []
