module RecursiveCircus exposing (..)

import Array exposing (..)
import List.Extra exposing (..)


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
