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
                    createSpiral ( 1, 1 ) (Some 2)
                        |> Expect.equal
                            (Dict.fromList
                                [ ( ( 1, 1 ), 1 ), ( ( 2, 1 ), 1 ) ]
                            )
            ]
        ]
