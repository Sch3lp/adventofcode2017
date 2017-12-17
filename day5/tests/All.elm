module All exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Trampolines exposing (..)


suite : Test
suite =
    describe "Trampolines"
        [ instructionTests
        , parseTests
        , solveTest
        ]


solveTest : Test
solveTest =
    describe "solve"
        [ test "given puzzle input" <|
            \_ ->
                solve """
                0
                """
                    |> Expect.equal 1
        ]


parseTests : Test
parseTests =
    describe "parsing"
        [ describe "toInstruction"
            [ test "negative number becomes Backwards with absolute" <|
                \_ ->
                    toInstruction -7687
                        |> Expect.equal (Backwards 7687)
            , test "0 becomes Remain" <|
                \_ ->
                    toInstruction 0
                        |> Expect.equal (Remain)
            , test "positive number becomes Forwards with absolute" <|
                \_ ->
                    toInstruction 999
                        |> Expect.equal (Forwards 999)
            ]
        , describe "toInstructions"
            [ test "given numbers seperated with newLines, produces Instructions" <|
                \_ ->
                    toInstructions """
                                    -1
                                    0
                                    3
                                    1
                                    -999
                                    """
                        |> Expect.equal [ Backwards 1, Remain, Forwards 3, Forwards 1, Backwards 999 ]
            ]
        ]


instructionTests : Test
instructionTests =
    describe "Instruction"
        [ stepsToExitTests
        , applyInstructionTests
        ]


applyInstructionTests : Test
applyInstructionTests =
    describe "applyInstruction"
        [ test "Forwards 1: increases steps with 1, increases Offset with 1, jumps forwards 1 position" <|
            \_ ->
                Path [ Forwards 1, Remain, Remain ] (Index 0) (Steps 0)
                    |> applyInstruction (Forwards 1)
                    |> Expect.equal
                        (Path [ Forwards 2, Remain, Remain ] (Index 1) (Steps 1))
        , test "Forwards 2: increases steps with 1, increases Offset with 1, jumps forwards 2 positions" <|
            \_ ->
                Path [ Forwards 2, Remain, Remain ] (Index 0) (Steps 0)
                    |> applyInstruction (Forwards 2)
                    |> Expect.equal
                        (Path [ Forwards 3, Remain, Remain ] (Index 2) (Steps 1))
        , test "Remain: increases steps with 1, becomes Forwards 1, remains on its position" <|
            \_ ->
                Path [ Remain, Forwards 2, Remain ] (Index 0) (Steps 0)
                    |> applyInstruction (Remain)
                    |> Expect.equal
                        (Path [ Forwards 1, Forwards 2, Remain ] (Index 0) (Steps 1))
        , test "Backwards 1: increases steps with 1, becomes Remain, jumps backwards 1 position" <|
            \_ ->
                Path [ Remain, Forwards 2, Backwards 1 ] (Index 2) (Steps 0)
                    |> applyInstruction (Backwards 1)
                    |> Expect.equal
                        (Path [ Remain, Forwards 2, Remain ] (Index 1) (Steps 1))
        , test "Backwards 2: increases steps with 1, decreases Offset with 1, jumps backwards 2 positions" <|
            \_ ->
                Path [ Remain, Forwards 2, Backwards 2 ] (Index 2) (Steps 0)
                    |> applyInstruction (Backwards 2)
                    |> Expect.equal
                        (Path [ Remain, Forwards 2, Backwards 1 ] (Index 0) (Steps 1))
        , test "Backwards with a resulting index below 0: increases steps with 1, decreases Offset with 1, jumps to starting position" <|
            \_ ->
                Path [ Remain, Forwards 2, Backwards 4 ] (Index 2) (Steps 0)
                    |> applyInstruction (Backwards 4)
                    |> Expect.equal
                        (Path [ Remain, Forwards 2, Backwards 3 ] (Index 0) (Steps 1))
        , test "Forwards with a resulting index higher than the total of Instructions: increases steps with 1, increases Offset with 1, jumps to Exit" <|
            \_ ->
                Path [ Remain, Forwards 2, Backwards 2 ] (Index 1) (Steps 0)
                    |> applyInstruction (Forwards 2)
                    |> Expect.equal
                        (Path [ Remain, Forwards 3, Backwards 2 ] (Exit) (Steps 1))
        ]


stepsToExitTests : Test
stepsToExitTests =
    describe "stepsToExit"
        [ test "with only a Forwards 1 takes 1 steps to exit" <|
            \_ ->
                stepsToExit [ Forwards 1 ]
                    |> Expect.equal
                        1
        , test "with only a Remain takes 2 steps to exit" <|
            \_ ->
                stepsToExit [ Remain ]
                    |> Expect.equal
                        2
        , test "with only a Backwards 1 takes 3 steps to exit" <|
            \_ ->
                stepsToExit [ Backwards 1 ]
                    |> Expect.equal
                        3
        ]
