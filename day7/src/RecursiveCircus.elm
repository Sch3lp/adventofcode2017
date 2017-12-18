module RecursiveCircus exposing (..)

import List.Extra exposing (..)
import Parser exposing (Parser, (|.), (|=), succeed, symbol, float, ignore, zeroOrMore)
import Parser.LanguageKit exposing (variable)
import Char


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
    input
        |> Parser.run program
        |> Result.toMaybe
        |> Maybe.withDefault (Program "" 0)


program : Parser Program
program =
    succeed Program
        |. spaces
        |= programName
        |. spaces
        |. symbol "("
        |= programWeight
        |. symbol ")"
        |. Parser.end


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


programName : Parser String
programName =
    Parser.keep Parser.oneOrMore isChar


programWeight : Parser Weight
programWeight =
    Parser.int


isChar : Char -> Bool
isChar char =
    Char.isLower char
        || Char.isUpper char
        || Char.isDigit char



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
