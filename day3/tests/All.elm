module All exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Manhattan exposing (..)


suite : Test
suite =
    describe "Manhattan"
        [ describe "spiral"
            [ test "with 1" <|
                \_ ->
                    spiral 1
                        |> Expect.equal
                            [ Right ]
            , test "with 2" <|
                \_ ->
                    spiral 2
                        |> Expect.equal
                            [ Right, Up ]
            , test "with 5" <|
                \_ ->
                    spiral 5
                        |> Expect.equal
                            [ Right, Up, Left, Left ]
            , test "with 7" <|
                \_ ->
                    spiral 7
                        |> Expect.equal
                            [ Right, Up, Left, Left, Down, Down ]
            , test "with 10" <|
                \_ ->
                    spiral 9
                        |> Expect.equal
                            [ Right, Up, Left, Left, Down, Down, Right, Right, Right ]
            , test "with 361527" <|
                \_ ->
                    spiral 361527
                        |> List.length
                        |> Expect.equal
                            361527
            ]
        , describe "manhattanSteps"
            [ test "for 1 is empty" <|
                \_ ->
                    manhattanSteps 1
                        |> Expect.equal []
            , test "for 12 is 3 steps" <|
                \_ ->
                    manhattanSteps 12
                        |> Expect.equal [ Down, Left, Left ]
            , test "for 23 is 2 steps" <|
                \_ ->
                    manhattanSteps 23
                        |> Expect.equal [ Up, Up ]
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
            ]
        ]
