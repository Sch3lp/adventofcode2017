module All exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Trampolines exposing (..)


suite : Test
suite =
    describe "Trampolines"
        [ instructionTests
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
                        (Path [ Forwards 1, Forwards 2, Remain ] (Index 1) (Steps 1))
        , test "Backwards 2: increases steps with 1, decreases Offset with 1, jumps backwards 2 positions" <|
            \_ ->
                Path [ Remain, Forwards 2, Backwards 2 ] (Index 2) (Steps 0)
                    |> applyInstruction (Backwards 2)
                    |> Expect.equal
                        (Path [ Forwards 1, Forwards 2, Backwards 1 ] (Index 0) (Steps 1))
        , test "Forwards with an Offset higher than the total of Instructions: increases steps with 1, increases Offset with 1, jumps to Exit" <|
            \_ ->
                Path [ Remain, Forwards 2, Backwards 2 ] (Index 1) (Steps 0)
                    |> applyInstruction (Forwards 2)
                    |> Expect.equal
                        (Path [ Forwards 1, Forwards 3, Backwards 1 ] (Exit) (Steps 1))
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
