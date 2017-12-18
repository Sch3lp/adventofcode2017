module All exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import RecursiveCircus exposing (..)


snarf =
    describe "Tower"
        [ test "programOf Top returns Program" <|
            \_ ->
                let
                    top =
                        Program "gyxo" 61
                            |> Top
                in
                    programOf top |> Expect.equal (Program "gyxo" 61)
        , test "programOf Disc returns Disc" <|
            \_ ->
                let
                    top1 =
                        Program "gyxo" 61
                            |> Top

                    top2 =
                        Program "ebii" 61
                            |> Top

                    top3 =
                        Program "jptl" 61
                            |> Top

                    disc =
                        Disc (Program "ugml" 68) [ top1, top2, top3 ]
                in
                    programOf disc |> Expect.equal (Program "ugml" 68)
        ]
