port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import LastFmApi exposing (AlbumFromWeeklyChart)
import RemoteData exposing (WebData)
import AlbumImageCache exposing (AlbumImageCache)
import Array exposing (Array)
import Maybe.Extra


type alias Model =
    { months : Array Month
    , lastFmClient : LastFmApi.Client
    , monthsWithAlbums : List MonthWithAlbums
    , albumImageCache : AlbumImageCache
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
    , albumImageCacheFromLocalStorage : AlbumImageCache.AlbumImageCacheInLocalStorageFormat
    }


type Msg
    = ReceiveWeeklyAlbumChartFromMonth Month (Result Http.Error LastFmApi.WeeklyAlbumChart)
    | ReceiveAlbumInfo AlbumFromWeeklyChart (WebData LastFmApi.AlbumInfo)


maxNumberOfMonthsToFetch : Int
maxNumberOfMonthsToFetch =
    6


defaultCoverPath : String
defaultCoverPath =
    "images/default-cover.jpg?v=1"


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
        , albumImageCache =
            AlbumImageCache.convertFromLocalStorageFormat
                flags.albumImageCacheFromLocalStorage
        }
            ! [ Http.send (ReceiveWeeklyAlbumChartFromMonth flags.currentMonth) <|
                    lastFmClient.getWeeklyAlbumChart username
                        flags.currentMonth.start
                        flags.currentMonth.end
              ]


view : Model -> Html Msg
view model =
    div [ class "months" ]
        (List.map (viewMonthWithAlbums model.albumImageCache)
            (List.reverse model.monthsWithAlbums)
        )


viewMonthWithAlbums : AlbumImageCache -> MonthWithAlbums -> Html Msg
viewMonthWithAlbums cache { month, albums } =
    div [ class "month" ]
        [ h2 [ class "month__id" ] [ text month.month ]

        -- TODO: Consider passing just the image url for each album and benchmark results.
        -- Elm may re-render all albums each time anything in the cache changes.
        , div [ class "month__albums" ] <| List.map (viewAlbum cache) <| List.take 15 albums
        ]


viewAlbum : AlbumImageCache -> AlbumFromWeeklyChart -> Html Msg
viewAlbum cache album =
    let
        artistAndName =
            album.artist ++ " - " ++ album.name

        imageUrlFromCache =
            AlbumImageCache.getImageUrlForAlbum album cache

        hasImage =
            imageUrlFromCache |> String.isEmpty |> not

        imageUrl =
            if hasImage then
                imageUrlFromCache
            else
                defaultCoverPath
    in
        div [ class "album", title artistAndName ]
            [ img
                [ class "album__image"
                , src imageUrl
                , alt artistAndName
                ]
                []
            , if not hasImage then
                div [ class "album__name-and-artist-wrapper" ]
                    [ div [ class "album__artist" ] [ text album.artist ]
                    , div [ class "album__name" ] [ text album.name ]
                    ]
              else
                text ""
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveWeeklyAlbumChartFromMonth month (Ok albums) ->
            let
                maybeNextMonth =
                    Array.get (List.length model.monthsWithAlbums + 1) model.months

                ( albumImageCommandsBatch, updatedCache ) =
                    getImagesForAlbums
                        model.lastFmClient
                        model.albumImageCache
                        albums
            in
                { model
                    | monthsWithAlbums = { month = month, albums = albums } :: model.monthsWithAlbums
                    , albumImageCache = updatedCache
                }
                    ! [ getWeeklyAlbumChartForNextMonth
                            model.monthsWithAlbums
                            model.lastFmClient
                            maybeNextMonth
                      , albumImageCommandsBatch
                      ]

        ReceiveWeeklyAlbumChartFromMonth _ (Err err) ->
            -- TODO: Add proper error handling.
            let
                debugError =
                    Debug.log "Weekly album chart error" err
            in
                model ! []

        ReceiveAlbumInfo album albumInfo ->
            let
                updatedCache =
                    AlbumImageCache.addImageUrlForAlbum album
                        (RemoteData.map .imageUrl albumInfo)
                        model.albumImageCache
            in
                { model | albumImageCache = updatedCache }
                    ! [ if AlbumImageCache.isAnyAlbumLoading updatedCache then
                            Cmd.none
                        else
                            saveAlbumImageCacheToLocalStorage
                                (AlbumImageCache.convertToLocalStorageFormat updatedCache)
                      ]


getWeeklyAlbumChartForNextMonth : List a -> LastFmApi.Client -> Maybe Month -> Cmd Msg
getWeeklyAlbumChartForNextMonth monthsWithAlbums lastFmClient maybeNextMonth =
    if List.length monthsWithAlbums < maxNumberOfMonthsToFetch then
        Maybe.Extra.unwrap Cmd.none
            (\nextMonth ->
                Http.send (ReceiveWeeklyAlbumChartFromMonth nextMonth) <|
                    lastFmClient.getWeeklyAlbumChart username
                        nextMonth.start
                        nextMonth.end
            )
            maybeNextMonth
    else
        Cmd.none


getImagesForAlbums :
    LastFmApi.Client
    -> AlbumImageCache
    -> List AlbumFromWeeklyChart
    -> ( Cmd Msg, AlbumImageCache )
getImagesForAlbums lastFmClient cache albums =
    let
        albumToCmdFold =
            \album ( accCmd, accCache ) ->
                if AlbumImageCache.isAlbumInCache album accCache then
                    ( accCmd, accCache )
                else
                    ( Cmd.batch
                        [ accCmd
                        , RemoteData.sendRequest (lastFmClient.getAlbumInfo album)
                            |> Cmd.map (ReceiveAlbumInfo album)
                        ]
                    , AlbumImageCache.markAlbumAsLoading album accCache
                    )
    in
        albums
            |> List.take 15
            |> List.foldl albumToCmdFold ( Cmd.none, cache )


port saveAlbumImageCacheToLocalStorage : AlbumImageCache.AlbumImageCacheInLocalStorageFormat -> Cmd msg
