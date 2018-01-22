module AlbumStore
    exposing
        ( AlbumStore
        , AlbumImagesInLocalStorageFormat
        , addPlayCountForMonthForAlbum
        , convertToLocalStorageFormat
        , doesAlbumHaveImageUrl
        , initializeFromLocalStorageFormat
        , isAnyAlbumLoadingImageUrl
        , get
        , markAlbumImageUrlAsLoading
        , updateRemoteImageUrlForAlbum
        )

import Album exposing (Album)
import Dict exposing (Dict)
import LastFmApi
import List.Extra
import Maybe.Extra
import Month
import RemoteData


type alias AlbumStore =
    Dict LastFmApi.AlbumId Album


type alias AlbumImagesInLocalStorageFormat =
    List ( String, String )


initializeFromLocalStorageFormat : AlbumImagesInLocalStorageFormat -> AlbumStore
initializeFromLocalStorageFormat =
    Dict.fromList
        >> Dict.map
            (\albumId imageUrl ->
                Album.initialize albumId |> Album.addImageUrl imageUrl
            )


convertToLocalStorageFormat : AlbumStore -> AlbumImagesInLocalStorageFormat
convertToLocalStorageFormat =
    Dict.values
        >> List.foldl
            (\album accAlbumImages ->
                if Album.hasLoadedImageUrl album then
                    ( album.id, Album.getImageUrl album ) :: accAlbumImages
                else
                    accAlbumImages
            )
            []


doesAlbumHaveImageUrl : LastFmApi.AlbumId -> AlbumStore -> Bool
doesAlbumHaveImageUrl albumId store =
    Dict.get albumId store |> Maybe.map Album.hasImageUrl |> Maybe.withDefault False


markAlbumImageUrlAsLoading : LastFmApi.AlbumId -> AlbumStore -> AlbumStore
markAlbumImageUrlAsLoading albumId store =
    Dict.update albumId (Maybe.map Album.markImageUrlAsLoading) store


get : LastFmApi.AlbumId -> AlbumStore -> Maybe Album
get =
    Dict.get


addPlayCountForMonthForAlbum :
    LastFmApi.PlayCount
    -> Month.MonthId
    -> LastFmApi.AlbumId
    -> AlbumStore
    -> AlbumStore
addPlayCountForMonthForAlbum playCount month albumId store =
    updateOrInitialize albumId (Album.addPlayCountForMonth playCount month) store


updateRemoteImageUrlForAlbum : LastFmApi.AlbumId -> RemoteData.WebData String -> AlbumStore -> AlbumStore
updateRemoteImageUrlForAlbum albumId imageUrl store =
    updateOrInitialize albumId (Album.updateRemoteImageUrl imageUrl) store


isAnyAlbumLoadingImageUrl : AlbumStore -> Bool
isAnyAlbumLoadingImageUrl =
    Dict.values >> List.Extra.find Album.isLoadingImageUrl >> Maybe.Extra.isJust


updateOrInitialize : LastFmApi.AlbumId -> (Album -> Album) -> AlbumStore -> AlbumStore
updateOrInitialize albumId f store =
    Dict.update albumId
        (\maybeAlbum ->
            case maybeAlbum of
                Just album ->
                    Just <| f album

                Nothing ->
                    Just <| f <| Album.initialize albumId
        )
        store
