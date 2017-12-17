module Trampolines exposing (..)


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
applyInstruction instr path =
    path
