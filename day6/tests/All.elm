module All exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import MemoryBanks exposing (..)


solveTest : Test
solveTest =
    describe "solve"
        [ test "given puzzle input" <|
            \_ ->
                banksFrom [ 5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6 ]
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
                    |> Expect.equal (Bank 3 9)
        , test "[0,9,2,9,1] => Bank at index 1 with 9" <|
            \_ ->
                banksFrom [ 0, 5, 2, 9, 1 ]
                    |> bankWithMostBlocks
                    |> Expect.equal (Bank 1 9)
        , test "[0,0] => Bank at index 0 with 0" <|
            \_ ->
                banksFrom [ 0, 0 ]
                    |> bankWithMostBlocks
                    |> Expect.equal (Bank 0 0)
        ]


redistributeTest : Test
redistributeTest =
    skip <|
        describe "redistribute"
            [ test "from the start [3,0,0] -> [1,1,1]" <|
                \_ ->
                    banksFrom [ 3, 0, 0 ]
                        |> redistribute 3
                        |> Expect.equal
                            (banksFrom [ 1, 1, 1 ])
            , test "from the end [0,0,3] -> [1,1,1]" <|
                \_ ->
                    banksFrom [ 0, 0, 3 ]
                        |> redistribute 3
                        |> Expect.equal
                            (banksFrom [ 1, 1, 1 ])
            , test "from the middle [0,3,0] -> [1,1,1]" <|
                \_ ->
                    banksFrom [ 0, 3, 0 ]
                        |> redistribute 3
                        |> Expect.equal
                            (banksFrom [ 1, 1, 1 ])
            ]
