module ManhattanSumsTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Dict exposing (..)
import ManhattanSums exposing (..)


suite : Test
suite =
    describe "ManhattanSums"
        [ describe "spiral"
            [ test "2 times" <|
                \_ ->
                    createSpiral ( 0, 0 ) (Some 2)
                        |> Expect.equal
                            (Dict.fromList
                                [ ( ( 0, 0 ), 1 ), ( ( 1, 0 ), 1 ) ]
                            )
            ]
        ]
