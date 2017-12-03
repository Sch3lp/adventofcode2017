module Checksum exposing (..)


checksum : List (List Int) -> Int
checksum spreadsheet =
    spreadsheet
        |> List.map rowDiff
        |> List.sum


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
    spreadsheet
        |> List.map rowDivision
        |> List.sum


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
        even =
            (rem fst snd) == 0

        result =
            fst // snd
    in
        case even of
            True ->
                Just result

            False ->
                Nothing
infix 9 /?
