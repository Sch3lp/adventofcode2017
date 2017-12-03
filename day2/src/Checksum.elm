module Checksum exposing (..)


sumWith : (List Int -> Int) -> List (List Int) -> Int
sumWith fn spreadSheet =
    (List.map fn >> List.sum) spreadSheet


checksum : List (List Int) -> Int
checksum spreadsheet =
    sumWith rowDiff spreadsheet


rowDiff : List Int -> Int
rowDiff row =
    let
        max =
            List.maximum row |> Maybe.withDefault 0

        min =
            List.minimum row |> Maybe.withDefault 0
    in
        max - min



-- exercise 2


totalEvenDivision : List (List Int) -> Int
totalEvenDivision spreadsheet =
    sumWith rowDivision spreadsheet



-- The assignment doesn't state what should happen
-- when there are multiple evenly divisible pairs
-- so I assumed there's always one and only one evenly divisible pair


rowDivision : List Int -> Int
rowDivision row =
    row
        |> List.sort
        |> List.reverse
        |> permute []
        |> List.filterMap (\( fst, snd ) -> fst /? snd)
        |> List.head
        |> Maybe.withDefault 0


permute : List ( Int, Int ) -> List Int -> List ( Int, Int )
permute acc row =
    case row of
        [] ->
            acc

        h :: t ->
            acc
                |> List.append (List.map (\item -> ( h, item )) t)
                |> List.append (permute acc t)


(/?) : Int -> Int -> Maybe Int
(/?) fst snd =
    let
        evenlyDivisible =
            (rem fst snd) == 0

        result =
            fst // snd
    in
        case evenlyDivisible of
            True ->
                Just result

            False ->
                Nothing
infix 9 /?
