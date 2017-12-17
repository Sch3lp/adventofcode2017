module MemoryBanks exposing (..)


type alias Banks =
    List Int


type Redistributions
    = Redistributions Int


solve : Banks -> Redistributions
solve banks =
    Redistributions 0
