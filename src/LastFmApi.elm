module LastFmApi exposing (AlbumFromWeeklyChart, Client, WeeklyAlbumChart, initializeClient)

import Http
import Json.Decode as Decode


type alias Client =
    { getWeeklyAlbumChart : String -> Int -> Int -> Http.Request WeeklyAlbumChart
    }


type alias WeeklyAlbumChart =
    List AlbumFromWeeklyChart


type alias WeeklyAlbumChartResult =
    Result Http.Error WeeklyAlbumChart


type alias AlbumFromWeeklyChart =
    { artist : String
    , name : String
    , playCount : Int
    }


initializeClient : String -> Client
initializeClient apiKey =
    { getWeeklyAlbumChart =
        \username from to ->
            Http.get (constructWeeklyAlbumChartUrl apiKey username from to) weeklyAlbumChartDecoder
    }


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
            Decode.map3 AlbumFromWeeklyChart
                (Decode.at [ "artist", "#text" ] Decode.string)
                (Decode.field "name" Decode.string)
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
