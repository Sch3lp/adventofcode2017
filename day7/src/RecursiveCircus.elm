module RecursiveCircus exposing (..)

import List.Extra exposing (..)


type Tower a
    = Disc a (List (Tower a))
    | Top a


type alias Name =
    String


type alias Weight =
    Int


type Program
    = Program Name Weight


type alias ProgramTower =
    Tower Program


programOf : ProgramTower -> Program
programOf tower =
    case tower of
        Top a ->
            a

        Disc a subtower ->
            a


appendTowerTo : ProgramTower -> ProgramTower -> ProgramTower
appendTowerTo towerToAdd tower =
    case tower of
        Top a ->
            Disc a [ towerToAdd ]

        Disc a towers ->
            Disc a <| towers ++ [ towerToAdd ]


parseLine : String -> Program
parseLine input =
    Program "" 0



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
