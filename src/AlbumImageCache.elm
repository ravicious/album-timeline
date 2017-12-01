module AlbumImageCache
    exposing
        ( AlbumImageCache
        , empty
        , addImageUrlForAlbum
        , getImageUrlForAlbum
        , isAlbumInCache
        )

import Dict exposing (Dict)
import LastFmApi


type alias AlbumImageCache =
    Dict String String


empty : AlbumImageCache
empty =
    Dict.empty


addImageUrlForAlbum : LastFmApi.AlbumFromWeeklyChart -> String -> AlbumImageCache -> AlbumImageCache
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
        Dict.get albumId >> Maybe.withDefault ""


isAlbumInCache : LastFmApi.AlbumFromWeeklyChart -> AlbumImageCache -> Bool
isAlbumInCache album =
    let
        albumId =
            LastFmApi.albumToAlbumId album
    in
        Dict.member albumId
