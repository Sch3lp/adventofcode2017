module ZoneTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Zone exposing (..)


suite : Test
suite =
    describe "Zone"
        [ describe "nextZone"
            [ test "for Zone 1 returns Zone 9" <|
                \_ ->
                    nextZone (Zone 1)
                        |> Expect.equal
                            (Zone 9)
            , test "for Zone 9 returns Zone 25" <|
                \_ ->
                    nextZone (Zone 9)
                        |> Expect.equal
                            (Zone 25)
            , test "for Zone 25 returns Zone 49" <|
                \_ ->
                    nextZone (Zone 25)
                        |> Expect.equal
                            (Zone 49)
            , test "for Zone 363609 returns Zone 49" <|
                \_ ->
                    nextZone (Zone 363609)
                        |> Expect.equal
                            (Zone 366025)
            ]
        , describe "previousZone"
            [ test "for Zone 1 returns Zone 1" <|
                \_ ->
                    previousZone (Zone 1)
                        |> Expect.equal
                            (Zone 1)
            , test "for Zone 25 returns Zone 9" <|
                \_ ->
                    previousZone (Zone 25)
                        |> Expect.equal
                            (Zone 9)
            , test "for Zone 49 returns Zone 25" <|
                \_ ->
                    previousZone (Zone 49)
                        |> Expect.equal
                            (Zone 25)
            , test "for Zone 363609 returns Zone 361201" <|
                \_ ->
                    previousZone (Zone 363609)
                        |> Expect.equal
                            (Zone 361201)
            ]
        , describe "sideLength of Zone"
            [ test "for Zone 9 is 3" <|
                \_ ->
                    sideLength (Zone 9)
                        |> Expect.equal
                            3
            , test "for Zone 25 is 5" <|
                \_ ->
                    sideLength (Zone 25)
                        |> Expect.equal
                            5
            , test "for Zone 363609 is 603" <|
                \_ ->
                    sideLength (Zone 363609)
                        |> Expect.equal
                            603
            ]
        , describe "corners"
            [ test "9, 7, 5, 3 for Zone 9" <|
                \_ ->
                    corners (Zone 9)
                        |> Expect.equalLists
                            [ 9, 7, 5, 3 ]
            , test "25, 21, 17, 13 for Zone 25" <|
                \_ ->
                    corners (Zone 25)
                        |> Expect.equalLists
                            [ 25, 21, 17, 13 ]
            ]
        , describe "closestCorner of a position in a Zone"
            [ test "closest to bottom left from bottom" <|
                \_ ->
                    closestCorner (Zone 25) 22
                        |> Expect.equal
                            21
            , test "closest to bottom left from left" <|
                \_ ->
                    closestCorner (Zone 25) 20
                        |> Expect.equal
                            21
            , test "closest to top left from left" <|
                \_ ->
                    closestCorner (Zone 25) 18
                        |> Expect.equal
                            17
            , test "closest to top left from top" <|
                \_ ->
                    closestCorner (Zone 25) 16
                        |> Expect.equal
                            17
            , test "closest to top right from top" <|
                \_ ->
                    closestCorner (Zone 25) 14
                        |> Expect.equal
                            13
            , test "closest to top right from right" <|
                \_ ->
                    closestCorner (Zone 25) 12
                        |> Expect.equal
                            13
            , test "closest to bottom right from right" <|
                \_ ->
                    closestCorner (Zone 25) 10
                        |> Expect.equal
                            25
            , test "closest to bottom right from bottom" <|
                \_ ->
                    closestCorner (Zone 25) 24
                        |> Expect.equal
                            25
            ]
        , describe "correctionForCloserToPreviousZone"
            [ test "10 in Zone 25 is closest to 25, but 9 is just as much distance away as 25" <|
                \_ ->
                    correctionForCloserToPreviousZone (Zone 25) (Maybe.Just ( 9, 1 ))
                        |> Expect.equal
                            (Maybe.Just ( 25, 1 ))
            ]
        ]
