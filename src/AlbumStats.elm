module AlbumStats
    exposing
        ( AlbumStats
        , initializeWithPlayCountForMonth
        , addPlayCountForMonth
        , getMostListenedToMonth
        , getFirstListenedToMonth
        )

import Dict exposing (Dict)
import Month exposing (MonthId)
import LastFmApi exposing (PlayCount)


type alias AlbumStats =
    { playCountsForMonths : Dict MonthId PlayCount
    , mostListenedTo : ( MonthId, PlayCount )
    , firstListenedTo : MonthId
    }


initializeWithPlayCountForMonth : Int -> String -> AlbumStats
initializeWithPlayCountForMonth playCount month =
    { playCountsForMonths = Dict.singleton month playCount
    , mostListenedTo = ( month, playCount )
    , firstListenedTo = month
    }


addPlayCountForMonth : Int -> String -> AlbumStats -> AlbumStats
addPlayCountForMonth newPlayCount newMonth albumStats =
    { albumStats
        | playCountsForMonths = Dict.insert newMonth newPlayCount albumStats.playCountsForMonths
        , mostListenedTo = calculateMostListenedTo ( newMonth, newPlayCount ) albumStats.mostListenedTo
        , firstListenedTo = calculateFirstListenedTo newMonth albumStats.firstListenedTo
    }


calculateMostListenedTo : ( MonthId, PlayCount ) -> ( MonthId, PlayCount ) -> ( MonthId, PlayCount )
calculateMostListenedTo ( newMonth, newPlayCount ) ( existingMonth, existingPlayCount ) =
    if newPlayCount > existingPlayCount then
        ( newMonth, newPlayCount )
    else
        ( existingMonth, existingPlayCount )


calculateFirstListenedTo : MonthId -> MonthId -> MonthId
calculateFirstListenedTo =
    min


getMostListenedToMonth : AlbumStats -> MonthId
getMostListenedToMonth =
    .mostListenedTo >> Tuple.first


getFirstListenedToMonth : AlbumStats -> MonthId
getFirstListenedToMonth =
    .firstListenedTo
