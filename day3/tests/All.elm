module All exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Manhattan exposing (..)
import Zone exposing (..)
import Util exposing (..)


suite : Test
suite =
    describe "Manhattan"
        [ describe "findZone"
            [ test "for position 1 returns Zone 1" <|
                \_ ->
                    findZone 1
                        |> Expect.equal
                            (Zone 1)
            , test "for position 2 returns Zone 9" <|
                \_ ->
                    findZone 2
                        |> Expect.equal
                            (Zone 9)
            , test "for position 5 returns Zone 9" <|
                \_ ->
                    findZone 5
                        |> Expect.equal
                            (Zone 9)
            , test "position is a corner, position 9 returns Zone 9" <|
                \_ ->
                    findZone 9
                        |> Expect.equal
                            (Zone 9)
            , test "for position 10 returns Zone 25" <|
                \_ ->
                    findZone 10
                        |> Expect.equal
                            (Zone 25)
            , test "with 361527" <|
                \_ ->
                    findZone 361527
                        |> Expect.equal
                            (Zone 363609)
            ]
        , describe
            "stepsAwayFromMiddle"
            [ test "on bottom side, on middle" <|
                \_ ->
                    stepsAwayFromMiddle 23 (Zone 25)
                        |> Expect.equal
                            0
            , test "on right side, on middle" <|
                \_ ->
                    stepsAwayFromMiddle 11 (Zone 25)
                        |> Expect.equal
                            0
            , test "on top side, on middle" <|
                \_ ->
                    stepsAwayFromMiddle 15 (Zone 25)
                        |> Expect.equal
                            0
            , test "on left side, on middle" <|
                \_ ->
                    stepsAwayFromMiddle 19 (Zone 25)
                        |> Expect.equal
                            0
            , test "on bottom side, overshooting middle" <|
                \_ ->
                    stepsAwayFromMiddle 24 (Zone 25)
                        |> Expect.equal
                            1
            , test "on right side, overshooting middle" <|
                \_ ->
                    stepsAwayFromMiddle 12 (Zone 25)
                        |> Expect.equal
                            1
            , test "on top side, overshooting middle" <|
                \_ ->
                    stepsAwayFromMiddle 16 (Zone 25)
                        |> Expect.equal
                            1
            , test "on left side, overshooting middle" <|
                \_ ->
                    stepsAwayFromMiddle 20 (Zone 25)
                        |> Expect.equal
                            1
            , test "on bottom side, undershooting middle" <|
                \_ ->
                    stepsAwayFromMiddle 22 (Zone 25)
                        |> Expect.equal
                            1
            , test "on right side, undershooting middle" <|
                \_ ->
                    stepsAwayFromMiddle 10 (Zone 25)
                        |> Expect.equal
                            1
            , test "on top side, undershooting middle" <|
                \_ ->
                    stepsAwayFromMiddle 14 (Zone 25)
                        |> Expect.equal
                            1
            , test "on left side, undershooting middle" <|
                \_ ->
                    stepsAwayFromMiddle 18 (Zone 25)
                        |> Expect.equal
                            1
            , test "in top right corner" <|
                \_ ->
                    stepsAwayFromMiddle 13 (Zone 25)
                        |> Expect.equal
                            2
            , test "in top left corner" <|
                \_ ->
                    stepsAwayFromMiddle 17 (Zone 25)
                        |> Expect.equal
                            2
            , test "in bottom right corner" <|
                \_ ->
                    stepsAwayFromMiddle 25 (Zone 25)
                        |> Expect.equal
                            2
            , test "in bottom left corner" <|
                \_ ->
                    stepsAwayFromMiddle 21 (Zone 25)
                        |> Expect.equal
                            2
            ]
        , describe "manhattan"
            [ test "for 1 is 0 steps" <|
                \_ ->
                    manhattan 1
                        |> Expect.equal 0
            , test "for 12 is 3 steps" <|
                \_ ->
                    manhattan 12
                        |> Expect.equal 3
            , test "for 23 is 2 steps" <|
                \_ ->
                    manhattan 23
                        |> Expect.equal 2
            , test "for 1024 is 31 steps" <|
                \_ ->
                    manhattan 1024
                        |> Expect.equal 31
            , test "for 361527 is 31 steps" <|
                \_ ->
                    manhattan 361527
                        |> Expect.equal 1
            ]
        ]
