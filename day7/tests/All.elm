module All exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import RecursiveCircus exposing (..)


snarf =
    describe "snarf"
        [ test "snarf snarf" <|
            \_ ->
                True |> Expect.equal True
        ]
