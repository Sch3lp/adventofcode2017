module RCaptcha exposing (..)

import Array exposing (..)
import Regex exposing (..)


rcaptcha : String -> Int
rcaptcha input =
    let
        inputAsArray =
            input
                |> Regex.split All (regex "")
                |> List.filterMap (String.toInt >> Result.toMaybe)
                |> Array.fromList
    in
        rcaptchaHelper inputAsArray


rcaptchaHelper : Array Int -> Int
rcaptchaHelper sequence =
    sequence
        |> Array.indexedMap (retainIfMatchesNext sequence)
        |> Debug.log "indexedMap:"
        |> Array.foldl (+) 0


halfway : Int -> Int
halfway listSize =
    round <| toFloat listSize / toFloat 2


findNextIndex : Int -> Int -> Int
findNextIndex listSize idx =
    let
        halfwayIdx =
            halfway listSize

        nextIdx =
            idx + halfwayIdx
    in
        if nextIdx <= (listSize - 1) then
            nextIdx
        else
            negate (listSize - nextIdx)


retainIfMatchesNext : Array Int -> (Int -> Int -> Int)
retainIfMatchesNext seq =
    \idx a ->
        let
            nextIdx =
                findNextIndex (Array.length seq) idx

            elementAtNextIndex =
                Array.get nextIdx seq
        in
            case elementAtNextIndex of
                Just b ->
                    if a == b then
                        a
                    else
                        0

                Nothing ->
                    0
