module RecursiveCircus exposing (..)

import List.Extra exposing (..)


type Disc
    = List


type Tower a
    = Disc a
    | Top a


type alias Name =
    String


type alias Weight =
    Int


type Program
    = Program Name Weight


type alias ProgramTower =
    Tower Program



-- Util


desc : (a -> comparable) -> (a -> a -> Order)
desc extractor left right =
    case compare (extractor left) (extractor right) of
        EQ ->
            EQ

        LT ->
            GT

        GT ->
            LT


sortWithDesc : (a -> comparable) -> List a -> List a
sortWithDesc extractor =
    List.Extra.stableSortWith <| desc <| extractor
