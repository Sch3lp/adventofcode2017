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


stepsToExit : Instructions -> Int
stepsToExit instructions =
    0


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
                    List.Extra.setAt idx (Forwards (offset + 1)) instructions

                Remain ->
                    List.Extra.setAt idx (Forwards 1) instructions

                Backwards offset ->
                    if offset == 1 then
                        List.Extra.setAt idx (Remain) instructions
                    else
                        List.Extra.setAt idx (Backwards (offset - 1)) instructions


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
