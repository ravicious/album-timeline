module AlbumStatsStore
    exposing
        ( AlbumStatsStore
        , empty
        , addPlayCountForMonthForAlbum
        , getAlbumStats
        )

import AlbumStats exposing (AlbumStats)
import Dict exposing (Dict)
import Month exposing (MonthId)
import LastFmApi exposing (AlbumFromWeeklyChart, PlayCount)


type alias AlbumStatsStore =
    Dict String AlbumStats


empty : AlbumStatsStore
empty =
    Dict.empty


addPlayCountForMonthForAlbum : PlayCount -> MonthId -> AlbumFromWeeklyChart -> AlbumStatsStore -> AlbumStatsStore
addPlayCountForMonthForAlbum playCount month album store =
    let
        albumId =
            LastFmApi.albumToAlbumId album

        updateAlbum =
            \maybeStats ->
                case maybeStats of
                    Just stats ->
                        Just <| AlbumStats.addPlayCountForMonth playCount month stats

                    Nothing ->
                        Just <| AlbumStats.initializeWithPlayCountForMonth playCount month
    in
        Dict.update albumId updateAlbum store


getAlbumStats : AlbumFromWeeklyChart -> AlbumStatsStore -> Maybe AlbumStats
getAlbumStats album =
    let
        albumId =
            LastFmApi.albumToAlbumId album
    in
        Dict.get albumId
