port module Main exposing (..)

import AlbumStore exposing (AlbumStore)
import Album exposing (Album)
import AlbumStats exposing (AlbumStats)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import LastFmApi exposing (AlbumFromWeeklyChart)
import Month exposing (Month)
import RemoteData exposing (WebData)
import Array exposing (Array)
import Maybe.Extra


type alias Model =
    { months : Array Month
    , lastFmClient : LastFmApi.Client
    , monthsWithAlbums : List MonthWithAlbums
    , albumStore : AlbumStore
    }


type alias MonthWithAlbums =
    { month : Month
    , albums : List AlbumFromWeeklyChart
    }


type alias Flags =
    { months : Array Month
    , currentMonth : Month
    , apiKey : String
    , albumImageCacheFromLocalStorage : AlbumStore.AlbumImagesInLocalStorageFormat
    }


type Msg
    = ReceiveWeeklyAlbumChartFromMonth Month (Result Http.Error LastFmApi.WeeklyAlbumChart)
    | ReceiveAlbumInfo AlbumFromWeeklyChart (WebData LastFmApi.AlbumInfo)


maxNumberOfMonthsToFetch : Int
maxNumberOfMonthsToFetch =
    6


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
        , albumStore =
            AlbumStore.initializeFromLocalStorageFormat
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
        (List.map (viewMonthWithAlbums model.albumStore)
            (List.reverse model.monthsWithAlbums)
        )


viewMonthWithAlbums : AlbumStore -> MonthWithAlbums -> Html Msg
viewMonthWithAlbums albumStore { month, albums } =
    let
        renderedAlbums =
            albums
                |> List.take 15
                |> List.foldr
                    (\albumFromWeeklyChart accRenderedAlbums ->
                        LastFmApi.albumToAlbumId albumFromWeeklyChart
                            |> (flip AlbumStore.get) albumStore
                            |> Maybe.map
                                (\album ->
                                    (viewAlbum <| Just ( albumFromWeeklyChart, album ))
                                        :: accRenderedAlbums
                                )
                            |> Maybe.withDefault accRenderedAlbums
                    )
                    []

        emptySpacesForPadding =
            if (List.length renderedAlbums) % itemsPerRow /= 0 then
                List.repeat (3 - (List.length renderedAlbums) % itemsPerRow)
                    (viewAlbum Nothing)
            else
                []
    in
        div [ class "c-month" ]
            [ h2 [ class "c-month__id" ] [ text month.month ]

            -- TODO: Consider passing just the image url for each album and benchmark results.
            -- Elm may re-render all albums each time anything in the cache changes.
            , div [ class "c-month__albums" ] (List.append renderedAlbums emptySpacesForPadding)
            ]


viewAlbum : Maybe ( AlbumFromWeeklyChart, Album ) -> Html Msg
viewAlbum maybeAlbumFromWeeklyChartAndAlbum =
    case maybeAlbumFromWeeklyChartAndAlbum of
        Nothing ->
            div [ class "c-album" ] []

        Just ( albumFromWeeklyChart, album ) ->
            let
                artistAndName =
                    albumFromWeeklyChart.artist ++ " - " ++ albumFromWeeklyChart.name

                hasImage =
                    Album.hasImageUrl album

                imageUrl =
                    if hasImage then
                        Album.getImageUrl album
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
                        , viewStats album.stats
                        ]
                else
                    div [ class "c-album without-image", title artistAndName ]
                        [ div [ class "c-album__name-and-artist-wrapper" ]
                            [ div [ class "c-album__artist" ] [ text albumFromWeeklyChart.artist ]
                            , div [ class "c-album__name" ] [ text albumFromWeeklyChart.name ]
                            ]
                        , viewStats album.stats
                        ]


viewStats : Maybe AlbumStats -> Html Msg
viewStats maybeStats =
    case maybeStats of
        Nothing ->
            text ""

        Just stats ->
            ul []
                [ li []
                    [ text <| "Most listened to: " ++ (AlbumStats.getMostListenedToMonth stats)
                    ]
                , li []
                    [ text <| "First listened to: " ++ (AlbumStats.getFirstListenedToMonth stats)
                    ]
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveWeeklyAlbumChartFromMonth month (Ok albums) ->
            let
                maybeNextMonth =
                    Array.get (List.length model.monthsWithAlbums + 1) model.months

                albumStoreWithUpdatedPlayCounts =
                    List.foldl
                        (\album albumStore ->
                            AlbumStore.addPlayCountForMonthForAlbum
                                album.playCount
                                month.month
                                (LastFmApi.albumToAlbumId album)
                                albumStore
                        )
                        model.albumStore
                        albums

                ( albumImageCommandsBatch, updatedAlbumStore ) =
                    getImagesForAlbums
                        model.lastFmClient
                        albumStoreWithUpdatedPlayCounts
                        albums
            in
                { model
                    | monthsWithAlbums = { month = month, albums = albums } :: model.monthsWithAlbums
                    , albumStore = updatedAlbumStore
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
                updatedAlbumStore =
                    AlbumStore.updateRemoteImageUrlForAlbum (LastFmApi.albumToAlbumId album)
                        (RemoteData.map .imageUrl albumInfo)
                        model.albumStore
            in
                { model | albumStore = updatedAlbumStore }
                    ! [ if AlbumStore.isAnyAlbumLoadingImageUrl updatedAlbumStore then
                            Cmd.none
                        else
                            saveAlbumImageCacheToLocalStorage
                                (AlbumStore.convertToLocalStorageFormat updatedAlbumStore)
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
    -> AlbumStore
    -> List AlbumFromWeeklyChart
    -> ( Cmd Msg, AlbumStore )
getImagesForAlbums lastFmClient store albums =
    let
        albumToCmdFold =
            \album ( accCmd, accStore ) ->
                let
                    albumId =
                        LastFmApi.albumToAlbumId album
                in
                    if AlbumStore.doesAlbumHaveImageUrl albumId accStore then
                        ( accCmd, accStore )
                    else
                        ( Cmd.batch
                            [ accCmd
                            , RemoteData.sendRequest (lastFmClient.getAlbumInfo album)
                                |> Cmd.map (ReceiveAlbumInfo album)
                            ]
                        , AlbumStore.markAlbumImageUrlAsLoading albumId accStore
                        )
    in
        albums
            |> List.take 15
            |> List.foldl albumToCmdFold ( Cmd.none, store )


port saveAlbumImageCacheToLocalStorage : AlbumStore.AlbumImagesInLocalStorageFormat -> Cmd msg
