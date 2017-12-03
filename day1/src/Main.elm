module Main exposing (..)

import Array exposing (..)
import Regex exposing (..)


type alias Model =
    { code : String }


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
        |> Array.indexedMap (matchesNext sequence)
        -- |> Debug.log "indexedMap:"
        |> Array.foldl (+) 0


matchesNext : Array Int -> (Int -> Int -> Int)
matchesNext seq =
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
