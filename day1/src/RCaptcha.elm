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
        -- |> Debug.log "indexedMap:"
        |> Array.foldl (+) 0


retainIfMatchesNext : Array Int -> (Int -> Int -> Int)
retainIfMatchesNext seq =
    \idx a ->
        let
            elementAtNextIndex =
                Array.get (idx + 1) seq
        in
            case elementAtNextIndex of
                Just b ->
                    if a == b then
                        a
                    else
                        0

                -- Compare last number to first number
                Nothing ->
                    let
                        first =
                            Array.get 0 seq
                    in
                        case first of
                            Just b ->
                                if a == b then
                                    a
                                else
                                    0

                            _ ->
                                0
