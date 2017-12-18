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
    describe "parseTests"
        [ describe "parsePrograms"
            [ test "Parsing a Program" <|
                \_ ->
                    """
                    ebii (61)
                    jptl (61)
                    ugml (68) -> gyxo, ebii, jptl
                    gyxo (61)
                    """
                        |> parsePrograms
                        |> Expect.equal ([ Program "ebii" 61, Program "jptl" 61, Program "ugml" 68, Program "gyxo" 61 ])
            ]
        , describe "parseProgram"
            [ test "Parsing a Program" <|
                \_ ->
                    "pbga (66)"
                        |> parseProgram
                        |> Expect.equal (Just <| Program "pbga" 66)
            , test "Parsing a Program with trailing space" <|
                \_ ->
                    "pbga (66) "
                        |> parseProgram
                        |> Expect.equal (Just <| Program "pbga" 66)
            , test "Parsing a Program with an arrow" <|
                \_ ->
                    "ugml (68) -> gyxo, ebii, jptl"
                        |> parseProgram
                        |> Expect.equal (Just <| Program "ugml" 68)
            ]
        ]
