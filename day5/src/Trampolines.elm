module Trampolines exposing (..)


type alias Offset =
    Int


type Instruction
    = Forwards Offset
    | Backwards Offset
    | Remain


type alias Instructions =
    List Instruction


type alias Movement =
    { instructions : Instructions
    , stepsTaken : Int
    }
