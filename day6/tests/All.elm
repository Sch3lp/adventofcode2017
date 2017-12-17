module All exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import MemoryBanks exposing (..)


solveTest : Test
solveTest =
    skip <|
        describe "solve"
            [ test "given puzzle input" <|
                \_ ->
                    solve [ 5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6 ]
                        |> Expect.equal (Redistributions 1)
            ]
