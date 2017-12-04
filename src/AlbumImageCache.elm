module AlbumImageCache
    exposing
        ( AlbumImageCache
        , AlbumImageCacheInLocalStorageFormat
        , empty
        , addImageUrlForAlbum
        , convertToLocalStorageFormat
        , convertFromLocalStorageFormat
        , getImageUrlForAlbum
        , isAlbumInCache
        , isAnyAlbumLoading
        , markAlbumAsLoading
        )

import Dict exposing (Dict)
import LastFmApi
import List.Extra
import Maybe.Extra
import RemoteData


type alias AlbumImageCache =
    Dict String (RemoteData.WebData String)


type alias AlbumImageCacheInLocalStorageFormat =
    List ( String, String )


empty : AlbumImageCache
empty =
    Dict.empty


addImageUrlForAlbum :
    LastFmApi.AlbumFromWeeklyChart
    -> RemoteData.WebData String
    -> AlbumImageCache
    -> AlbumImageCache
addImageUrlForAlbum album =
    let
        albumId =
            LastFmApi.albumToAlbumId album
    in
        Dict.insert albumId


getImageUrlForAlbum : LastFmApi.AlbumFromWeeklyChart -> AlbumImageCache -> String
getImageUrlForAlbum album =
    let
        albumId =
            LastFmApi.albumToAlbumId album
    in
        Dict.get albumId >> Maybe.Extra.unwrap "" (RemoteData.withDefault "")


isAlbumInCache : LastFmApi.AlbumFromWeeklyChart -> AlbumImageCache -> Bool
isAlbumInCache album =
    let
        albumId =
            LastFmApi.albumToAlbumId album
    in
        Dict.get albumId
            >> Maybe.withDefault RemoteData.NotAsked
            >> RemoteData.withDefault ""
            >> (String.isEmpty >> not)


markAlbumAsLoading : LastFmApi.AlbumFromWeeklyChart -> AlbumImageCache -> AlbumImageCache
markAlbumAsLoading album =
    let
        albumId =
            LastFmApi.albumToAlbumId album
    in
        Dict.insert albumId RemoteData.Loading


isAnyAlbumLoading : AlbumImageCache -> Bool
isAnyAlbumLoading =
    Dict.values >> List.Extra.find RemoteData.isLoading >> Maybe.Extra.isJust


convertToLocalStorageFormat : AlbumImageCache -> AlbumImageCacheInLocalStorageFormat
convertToLocalStorageFormat =
    Dict.filter (\key value -> RemoteData.isSuccess value)
        >> Dict.toList
        >> List.map (\( key, value ) -> ( key, RemoteData.withDefault "" value ))


convertFromLocalStorageFormat : AlbumImageCacheInLocalStorageFormat -> AlbumImageCache
convertFromLocalStorageFormat =
    Dict.fromList >> Dict.map (\key value -> RemoteData.succeed value)
