module All exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import RecursiveCircus exposing (..)


towerTests : Test
towerTests =
    describe "Tower"
        [ describe "programOf"
            [ test "Top returns Program" <|
                \_ ->
                    let
                        top =
                            Top (Program "gyxo" 61)
                    in
                        programOf top |> Expect.equal (Program "gyxo" 61)
            , test "Disc returns Disc" <|
                \_ ->
                    let
                        top1 =
                            Top (Program "gyxo" 61)

                        top2 =
                            Top (Program "ebii" 61)

                        top3 =
                            Top (Program "jptl" 61)

                        disc =
                            Disc (Program "ugml" 68) [ top1, top2, top3 ]
                    in
                        programOf disc |> Expect.equal (Program "ugml" 68)
            ]
        , describe "appendTowerTo"
            [ test "appending to Top returns Disc" <|
                \_ ->
                    let
                        top1 =
                            Top (Program "gyxo" 61)

                        topProgram =
                            Program "ugml" 68

                        topToAddTo =
                            Top topProgram
                    in
                        appendTowerTo top1 topToAddTo |> Expect.equal (Disc topProgram [ top1 ])
            , test "appending to Disc returns Disc with new Tower appended to existing towers" <|
                \_ ->
                    let
                        top1 =
                            Top (Program "gyxo" 61)

                        topProgram =
                            Program "ugml" 68

                        topToAddTo =
                            Disc topProgram [ top1 ]

                        top2 =
                            Top (Program "ebii" 61)
                    in
                        appendTowerTo top2 topToAddTo |> Expect.equal (Disc topProgram [ top1, top2 ])
            ]
        ]


parseTests : Test
parseTests =
    describe "parseLine"
        [ test "no arrows returns a Program" <|
            \_ ->
                "pbga (66)"
                    |> parseLine
                    |> Expect.equal (Program "pbga" 66)
        ]
