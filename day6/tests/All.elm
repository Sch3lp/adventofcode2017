module All exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import MemoryBanks exposing (..)


solveTest : Test
solveTest =
    describe "solve"
        [ test "spec" <|
            \_ ->
                banksFrom [ 0, 2, 7, 0 ]
                    |> solve
                    |> Expect.equal (Redistributions 5)
        , test "given puzzle input" <|
            \_ ->
                banksFrom [ 5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6 ]
                    |> solve
                    |> Expect.equal (Redistributions 1)
        ]


solve2Test : Test
solve2Test =
    describe "solve2"
        [ test "spec" <|
            \_ ->
                banksFrom [ 2, 4, 1, 2 ]
                    |> solve
                    |> Expect.equal (Redistributions 4)
        , test "given puzzle input" <|
            \_ ->
                banksFrom [ 1, 1, 14, 13, 12, 11, 10, 9, 8, 7, 7, 5, 5, 3, 3, 0 ]
                    |> solve
                    |> Expect.equal (Redistributions 1)
        ]


bankWithMostBlocksTest : Test
bankWithMostBlocksTest =
    describe "bankWithMostBlocks"
        [ test "[0,5,2,9,1] => Bank at index 3 with 9" <|
            \_ ->
                banksFrom [ 0, 5, 2, 9, 1 ]
                    |> bankWithMostBlocks
                    |> Expect.equal ( 3, 9 )
        , test "[0,9,2,9,1,9] => Bank at index 1 with 9, tie is won by bank with lowest index" <|
            \_ ->
                banksFrom [ 0, 9, 2, 9, 1, 9 ]
                    |> bankWithMostBlocks
                    |> Expect.equal ( 1, 9 )
        , test "[0,0] => Bank at index 0 with 0" <|
            \_ ->
                banksFrom [ 0, 0 ]
                    |> bankWithMostBlocks
                    |> Expect.equal ( 0, 0 )
        ]


cycleTest : Test
cycleTest =
    describe "cycle"
        [ describe "spec"
            [ test "[0,2,7,0] -> [2,4,1,2]" <|
                \_ ->
                    banksFrom [ 0, 2, 7, 0 ]
                        |> cycle
                        |> Expect.equal
                            (banksFrom [ 2, 4, 1, 2 ])
            , test "[2,4,1,2] -> [3,1,2,3]" <|
                \_ ->
                    banksFrom [ 2, 4, 1, 2 ]
                        |> cycle
                        |> Expect.equal
                            (banksFrom [ 3, 1, 2, 3 ])
            , test "[3,1,2,3] -> [0,2,3,4]" <|
                \_ ->
                    banksFrom [ 3, 1, 2, 3 ]
                        |> cycle
                        |> Expect.equal
                            (banksFrom [ 0, 2, 3, 4 ])
            , test "[0,2,3,4] -> [1,3,4,1]" <|
                \_ ->
                    banksFrom [ 0, 2, 3, 4 ]
                        |> cycle
                        |> Expect.equal
                            (banksFrom [ 1, 3, 4, 1 ])
            , test "[1,3,4,1] -> [2,4,1,2]" <|
                \_ ->
                    banksFrom [ 1, 3, 4, 1 ]
                        |> cycle
                        |> Expect.equal
                            (banksFrom [ 2, 4, 1, 2 ])
            ]
        , test "from the start [3,0,0] -> [1,1,1]" <|
            \_ ->
                banksFrom [ 3, 0, 0 ]
                    |> cycle
                    |> Expect.equal
                        (banksFrom [ 1, 1, 1 ])
        , test "from the end [0,0,3] -> [1,1,1]" <|
            \_ ->
                banksFrom [ 0, 0, 3 ]
                    |> cycle
                    |> Expect.equal
                        (banksFrom [ 1, 1, 1 ])
        , test "from the middle [0,3,0] -> [1,1,1]" <|
            \_ ->
                banksFrom [ 0, 3, 0 ]
                    |> cycle
                    |> Expect.equal
                        (banksFrom [ 1, 1, 1 ])
        ]
