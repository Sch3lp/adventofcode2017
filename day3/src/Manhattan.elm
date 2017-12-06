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
    let
        distanceToMiddle =
            distance (middleOfASide zone) (sideLength zone)

        --            3                    5 = 2
        distanceFromCorner =
            distanceFromClosestCorner zone position

        -- 1 --> must be 2
        stepsToMiddle =
            distance distanceToMiddle distanceFromCorner

        --           2                 1
    in
        if isOnACorner zone position then
            distanceToMiddle
        else
            stepsToMiddle


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
