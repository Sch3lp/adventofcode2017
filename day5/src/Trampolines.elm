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
    case instruction of
        Forwards offset ->
            let
                increasedSteps =
                    increaseStepsTaken path.stepsTaken

                updatedInstructions =
                    updateInstructions path.position instruction path.instructions

                exitIdx =
                    (List.length path.instructions) + 1

                newPosition =
                    updatePosition path.position instruction exitIdx
            in
                { path
                    | instructions = updatedInstructions
                    , position = newPosition
                    , stepsTaken = increasedSteps
                }

        _ ->
            path


updateInstructions : Position -> Instruction -> Instructions -> Instructions
updateInstructions pos instruction instructions =
    case pos of
        Exit ->
            instructions

        Index idx ->
            case instruction of
                Forwards offset ->
                    List.Extra.setAt idx (Forwards (offset + 1)) instructions

                _ ->
                    instructions


updatePosition : Position -> Instruction -> Int -> Position
updatePosition position instruction exitIdx =
    case position of
        Exit ->
            Exit

        Index idx ->
            case instruction of
                Forwards offset ->
                    forwards idx offset exitIdx

                _ ->
                    position


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
