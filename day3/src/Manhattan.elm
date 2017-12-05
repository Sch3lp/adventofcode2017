module Manhattan exposing (..)

import Zone exposing (..)
import Util exposing (..)


type Direction
    = Up
    | Right
    | Down
    | Left


manhattan : Int -> Int
manhattan position =
    manhattanSteps position
        |> List.length


manhattanSteps : Int -> List Direction
manhattanSteps position =
    []


stepsAwayFromMiddle : Int -> Zone -> Int
stepsAwayFromMiddle position zone =
    if isOnACorner zone position then
        middleOfASide zone
    else
        closestCorner zone position


stepsToAccessPort : Zone -> Int
stepsToAccessPort zone =
    0


findZone : Int -> Zone
findZone position =
    if position == 1 then
        { corner = 1 }
    else
        let
            nearestZone =
                toFloat position
                    |> sqrt
                    |> ceiling
                    |> nextUneven
                    |> powerTwo
        in
            { corner = nearestZone }
