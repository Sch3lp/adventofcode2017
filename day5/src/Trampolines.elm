module Trampolines exposing (..)

import List.Extra exposing (..)


type alias Offset =
    Int


type Instruction
    = Forwards Offset
    | Backwards Offset
    | Remain


type alias Instructions =
    List Instruction


type alias Steps =
    { steps : Int }


increaseStepsTaken : Steps -> Steps
increaseStepsTaken { steps } =
    Steps (steps + 1)


type Position
    = Index Int
    | Exit


type alias Path =
    { instructions : Instructions
    , position : Position
    , stepsTaken : Steps
    }


solve : String -> Int
solve input =
    input
        |> toInstructions
        |> stepsToExit


stepsToExit : Instructions -> Int
stepsToExit instructions =
    let
        initialPath =
            Path instructions (Index 0) (Steps 0)

        applyInstructions : Path -> Path
        applyInstructions =
            \path ->
                case path.position of
                    Exit ->
                        path

                    Index idx ->
                        applyInstructions <| applyInstructionHelper (instructionAt path idx) path
    in
        initialPath
            |> applyInstructions
            |> .stepsTaken
            |> .steps


instructionAt : Path -> Int -> Maybe Instruction
instructionAt path idx =
    List.Extra.getAt idx path.instructions


applyInstructionHelper : Maybe Instruction -> Path -> Path
applyInstructionHelper instruction path =
    case instruction of
        Just instr ->
            applyInstruction instr path

        Nothing ->
            path


applyInstruction : Instruction -> Path -> Path
applyInstruction instruction path =
    let
        increasedSteps =
            increaseStepsTaken path.stepsTaken

        newInstructions =
            advanceInstructionAtPosition path.position instruction path.instructions

        exitIdx =
            (List.length path.instructions)

        newPosition =
            advancePosition path.position instruction exitIdx
    in
        { path
            | instructions = newInstructions
            , position = newPosition
            , stepsTaken = increasedSteps
        }


advanceInstructionAtPosition : Position -> Instruction -> Instructions -> Instructions
advanceInstructionAtPosition pos instruction instructions =
    case pos of
        Exit ->
            instructions

        Index idx ->
            case instruction of
                Forwards offset ->
                    List.Extra.setAt idx (Forwards (forwardOffset offset)) instructions

                Remain ->
                    List.Extra.setAt idx (Forwards 1) instructions

                Backwards offset ->
                    if offset == 1 then
                        List.Extra.setAt idx (Remain) instructions
                    else
                        List.Extra.setAt idx (Backwards (offset - 1)) instructions


forwardOffset : Offset -> Offset
forwardOffset offset =
    if (offset >= 3) then
        offset - 1
    else
        offset + 1


advancePosition : Position -> Instruction -> Int -> Position
advancePosition position instruction exitIdx =
    case position of
        Exit ->
            Exit

        Index idx ->
            case instruction of
                Forwards offset ->
                    forwards idx offset exitIdx

                Remain ->
                    position

                Backwards offset ->
                    backwards idx offset


forwards : Int -> Offset -> Int -> Position
forwards idx offset exitIdx =
    let
        newIdx =
            idx + offset
    in
        if newIdx >= exitIdx then
            Exit
        else
            Index newIdx


backwards : Int -> Offset -> Position
backwards idx offset =
    let
        newIdx =
            idx - offset
    in
        if newIdx < 0 then
            Index 0
        else
            Index newIdx


toInstruction : Int -> Instruction
toInstruction number =
    if number == 0 then
        Remain
    else if number < 0 then
        Backwards (abs number)
    else
        Forwards number


toInstructions : String -> Instructions
toInstructions input =
    input
        |> String.lines
        |> List.map String.trim
        |> List.map String.toInt
        |> List.filterMap Result.toMaybe
        |> List.map toInstruction
