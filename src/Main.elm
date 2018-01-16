port module Main exposing (..)

import AlbumStats exposing (AlbumStats)
import AlbumStatsStore exposing (AlbumStatsStore)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import LastFmApi exposing (AlbumFromWeeklyChart)
import Month exposing (Month)
import RemoteData exposing (WebData)
import AlbumImageCache exposing (AlbumImageCache)
import Array exposing (Array)
import Maybe.Extra


type alias Model =
    { months : Array Month
    , lastFmClient : LastFmApi.Client
    , monthsWithAlbums : List MonthWithAlbums
    , albumImageCache : AlbumImageCache
    , albumStatsStore : AlbumStatsStore
    }


type alias MonthWithAlbums =
    { month : Month
    , albums : List AlbumFromWeeklyChart
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
    96


itemsPerRow : Int
itemsPerRow =
    3


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
        , albumStatsStore = AlbumStatsStore.empty
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
    div [ class "c-month-list" ]
        (List.map (viewMonthWithAlbums model.albumImageCache model.albumStatsStore)
            (List.reverse model.monthsWithAlbums)
        )


viewMonthWithAlbums : AlbumImageCache -> AlbumStatsStore -> MonthWithAlbums -> Html Msg
viewMonthWithAlbums cache albumStatsStore { month, albums } =
    let
        renderedAlbums =
            List.map (Just >> viewAlbum cache albumStatsStore) <| List.take 15 albums

        emptySpacesForPadding =
            if (List.length renderedAlbums) % itemsPerRow /= 0 then
                List.repeat (3 - (List.length renderedAlbums) % itemsPerRow)
                    (viewAlbum cache
                        albumStatsStore
                        Nothing
                    )
            else
                []
    in
        div [ class "c-month" ]
            [ h2 [ class "c-month__id" ] [ text month.month ]

            -- TODO: Consider passing just the image url for each album and benchmark results.
            -- Elm may re-render all albums each time anything in the cache changes.
            , div [ class "c-month__albums" ] (List.append renderedAlbums emptySpacesForPadding)
            ]


viewAlbum : AlbumImageCache -> AlbumStatsStore -> Maybe AlbumFromWeeklyChart -> Html Msg
viewAlbum cache albumStatsStore maybeAlbum =
    case maybeAlbum of
        Just album ->
            let
                artistAndName =
                    album.artist ++ " - " ++ album.name

                imageUrlFromCache =
                    AlbumImageCache.getImageUrlForAlbum album cache

                maybeStats =
                    AlbumStatsStore.getAlbumStats album albumStatsStore

                hasImage =
                    imageUrlFromCache |> String.isEmpty |> not

                imageUrl =
                    if hasImage then
                        imageUrlFromCache
                    else
                        defaultCoverPath
            in
                if hasImage then
                    div [ class "c-album", title artistAndName ]
                        [ img
                            [ class "c-album__image"
                            , src imageUrl
                            , alt artistAndName
                            ]
                            []
                        , viewStats maybeStats
                        ]
                else
                    div [ class "c-album without-image", title artistAndName ]
                        [ div [ class "c-album__name-and-artist-wrapper" ]
                            [ div [ class "c-album__artist" ] [ text album.artist ]
                            , div [ class "c-album__name" ] [ text album.name ]
                            ]
                        , viewStats maybeStats
                        ]

        Nothing ->
            div [ class "c-album" ] []


viewStats : Maybe AlbumStats -> Html Msg
viewStats maybeStats =
    case maybeStats of
        Nothing ->
            text ""

        Just stats ->
            ul []
                [ li [] [ text <| "Most listened to: " ++ (AlbumStats.getMostListenedToMonth stats) ]
                , li [] [ text <| "First listened to: " ++ (AlbumStats.getFirstListenedToMonth stats) ]
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveWeeklyAlbumChartFromMonth month (Ok albums) ->
            let
                maybeNextMonth =
                    Array.get (List.length model.monthsWithAlbums + 1) model.months

                updatedAlbumStatsStore =
                    List.foldl
                        (\album albumStatsStore ->
                            AlbumStatsStore.addPlayCountForMonthForAlbum
                                album.playCount
                                month.month
                                album
                                albumStatsStore
                        )
                        model.albumStatsStore
                        albums

                ( albumImageCommandsBatch, updatedCache ) =
                    getImagesForAlbums
                        model.lastFmClient
                        model.albumImageCache
                        albums
            in
                { model
                    | monthsWithAlbums = { month = month, albums = albums } :: model.monthsWithAlbums
                    , albumImageCache = updatedCache
                    , albumStatsStore = updatedAlbumStatsStore
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
