module LastFmApi
    exposing
        ( AlbumFromWeeklyChart
        , AlbumInfo
        , Client
        , WeeklyAlbumChart
        , albumToAlbumId
        , initializeClient
        )

import Http
import Json.Decode as Decode
import Dict
import List.Extra
import Maybe.Extra


type alias Client =
    { getWeeklyAlbumChart : String -> Int -> Int -> Http.Request WeeklyAlbumChart
    , getAlbumInfo : AlbumFromWeeklyChart -> Http.Request AlbumInfo
    }


type alias WeeklyAlbumChart =
    List AlbumFromWeeklyChart


type alias WeeklyAlbumChartResult =
    Result Http.Error WeeklyAlbumChart


type alias AlbumFromWeeklyChart =
    { artist : String
    , name : String
    , musicBrainzId : String
    , playCount : Int
    }


type alias AlbumInfo =
    { imageUrl : String
    }


initializeClient : String -> Client
initializeClient apiKey =
    { getWeeklyAlbumChart =
        \username from to ->
            Http.get (constructWeeklyAlbumChartUrl apiKey username from to) weeklyAlbumChartDecoder
    , getAlbumInfo =
        \album ->
            Http.get (constructAlbumInfoUrl apiKey album) albumInfoDecoder
    }


albumToAlbumId : AlbumFromWeeklyChart -> String
albumToAlbumId album =
    if String.isEmpty album.musicBrainzId then
        album.artist ++ album.name
    else
        album.musicBrainzId



-- Weekly album chart


constructWeeklyAlbumChartUrl : String -> String -> Int -> Int -> String
constructWeeklyAlbumChartUrl apiKey username from to =
    "http://ws.audioscrobbler.com/2.0/?method=user.getweeklyalbumchart&format=json"
        ++ "&api_key="
        ++ apiKey
        ++ "&user="
        ++ Http.encodeUri username
        ++ "&from="
        ++ toString from
        ++ "&to="
        ++ toString to


weeklyAlbumChartDecoder : Decode.Decoder WeeklyAlbumChart
weeklyAlbumChartDecoder =
    Decode.at [ "weeklyalbumchart", "album" ] <|
        Decode.list <|
            Decode.map4 AlbumFromWeeklyChart
                (Decode.at [ "artist", "#text" ] Decode.string)
                (Decode.field "name" Decode.string)
                (Decode.field "mbid" Decode.string)
                (Decode.field "playcount" Decode.string
                    |> Decode.andThen
                        (\string ->
                            case String.toInt string of
                                Ok int ->
                                    Decode.succeed int

                                Err err ->
                                    Decode.fail err
                        )
                )



-- Album info


constructAlbumInfoUrl : String -> AlbumFromWeeklyChart -> String
constructAlbumInfoUrl apiKey album =
    let
        idParams =
            if String.isEmpty album.musicBrainzId then
                "&artist="
                    ++ Http.encodeUri album.artist
                    ++ "&album="
                    ++ Http.encodeUri album.name
            else
                "&mbid=" ++ album.musicBrainzId
    in
        "http://ws.audioscrobbler.com/2.0/?method=album.getInfo&format=json"
            ++ "&api_key="
            ++ apiKey
            ++ idParams


albumInfoDecoder : Decode.Decoder AlbumInfo
albumInfoDecoder =
    Decode.at [ "album", "image" ] (Decode.list (Decode.dict Decode.string))
        |> Decode.andThen
            (\imageSizes ->
                imageSizes
                    |> List.Extra.find
                        (\imageSize ->
                            imageSize
                                |> Dict.get "size"
                                |> Maybe.Extra.unwrap False (\size -> size == "mega")
                        )
                    |> Maybe.andThen (Dict.get "#text")
                    |> Maybe.Extra.unwrap (Decode.fail "Couldn't find image with size mega for album")
                        Decode.succeed
            )
        |> Decode.map AlbumInfo
