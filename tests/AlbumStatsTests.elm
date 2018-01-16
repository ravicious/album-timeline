module AlbumStatsTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import AlbumStats


uniqueTwoMonthsFuzzer : Fuzzer ( String, String )
uniqueTwoMonthsFuzzer =
    Fuzz.map2 (,) string string
        |> Fuzz.conditional
            { retries = 1
            , fallback = \( a, b ) -> ( a, "1" ++ b )
            , condition = \( a, b ) -> a /= b
            }


{-| We're not going to see 0 as a play count value, so let's generate only positive ints.
-}
playCountFuzzer : Fuzzer Int
playCountFuzzer =
    Fuzz.intRange 1 10000


suite : Test
suite =
    describe "The AlbumStats module"
        [ -- It might seem like we're repeating implementation details in tests, but that's what we
          -- want. The implementation is different depending on whether we're initializing the stats
          -- or adding another month to the stats. So if we were to write standard unit tests, we'd
          -- have to write tests for all those different cases.
          fuzz3 uniqueTwoMonthsFuzzer
            playCountFuzzer
            playCountFuzzer
            "correctly calculates the most listened to month"
          <|
            \( month1, month2 ) playCount1 playCount2 ->
                let
                    biggerPlayCount =
                        max playCount1 playCount2

                    albumStats =
                        AlbumStats.initializeWithPlayCountForMonth playCount1 month1
                            |> AlbumStats.addPlayCountForMonth playCount2 month2
                in
                    Expect.equal biggerPlayCount (Tuple.second albumStats.mostListenedTo)
        , fuzz3 uniqueTwoMonthsFuzzer
            playCountFuzzer
            playCountFuzzer
            "correctly calculates the first month the album was listened to"
          <|
            \( month1, month2 ) playCount1 playCount2 ->
                let
                    earlierMonth =
                        min month1 month2

                    albumStats =
                        AlbumStats.initializeWithPlayCountForMonth playCount1 month1
                            |> AlbumStats.addPlayCountForMonth playCount2 month2
                in
                    Expect.equal earlierMonth albumStats.firstListenedTo
        ]
