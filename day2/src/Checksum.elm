module Checksum exposing (..)


checksum : List (List Int) -> Int
checksum spreadsheet =
    spreadsheet
        |> List.map rowDiff
        |> List.foldl (+) 0


rowDiff : List Int -> Int
rowDiff row =
    let
        max =
            List.maximum row |> Maybe.withDefault 0

        min =
            List.minimum row |> Maybe.withDefault 0
    in
        max - min
