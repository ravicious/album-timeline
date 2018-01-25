module Album
    exposing
        ( Album
        , addImageUrl
        , addPlayCountForMonth
        , getImageUrl
        , hasImageUrl
        , hasLoadedImageUrl
        , hasSuccessfullyLoadedImageUrl
        , initialize
        , isLoadingImageUrl
        , markImageUrlAsLoading
        , updateRemoteImageUrl
        )

import AlbumStats exposing (AlbumStats)
import LastFmApi
import Month
import RemoteData


type alias Album =
    { id : LastFmApi.AlbumId
    , imageUrl : RemoteData.WebData String
    , stats : Maybe AlbumStats
    }


initialize : LastFmApi.AlbumId -> Album
initialize id =
    { id = id
    , imageUrl = RemoteData.NotAsked
    , stats = Nothing
    }


updateRemoteImageUrl : RemoteData.WebData String -> Album -> Album
updateRemoteImageUrl remoteImageUrl album =
    { album | imageUrl = remoteImageUrl }


addImageUrl : String -> Album -> Album
addImageUrl imageUrl album =
    updateRemoteImageUrl (RemoteData.Success imageUrl) album


hasImageUrl : Album -> Bool
hasImageUrl =
    getImageUrl >> (String.isEmpty >> not)


getImageUrl : Album -> String
getImageUrl =
    .imageUrl >> RemoteData.withDefault ""


markImageUrlAsLoading : Album -> Album
markImageUrlAsLoading album =
    { album | imageUrl = RemoteData.Loading }


addPlayCountForMonth : LastFmApi.PlayCount -> Month.MonthId -> Album -> Album
addPlayCountForMonth playCount monthId album =
    let
        updatedStats =
            case album.stats of
                Just stats ->
                    Just <| AlbumStats.addPlayCountForMonth playCount monthId stats

                Nothing ->
                    Just <| AlbumStats.initializeWithPlayCountForMonth playCount monthId
    in
        { album | stats = updatedStats }


isLoadingImageUrl : Album -> Bool
isLoadingImageUrl =
    .imageUrl >> RemoteData.isLoading


hasLoadedImageUrl : Album -> Bool
hasLoadedImageUrl album =
    RemoteData.isSuccess album.imageUrl || RemoteData.isFailure album.imageUrl


hasSuccessfullyLoadedImageUrl : Album -> Bool
hasSuccessfullyLoadedImageUrl =
    .imageUrl >> RemoteData.isSuccess
