module All exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Trampolines exposing (..)


suite : Test
suite =
    describe "Trampolines"
        [ describe "does stuff"
            [ test "like jumping" <|
                \_ ->
                    Trampoline True
                        |> Expect.equal
                            { jump = True }
            ]
        ]
