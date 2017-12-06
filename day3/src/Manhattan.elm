module Manhattan exposing (..)

import Zone exposing (..)
import Util exposing (..)


type Direction
    = Up
    | Right
    | Down
    | Left


manhattanSteps : Int -> List Direction
manhattanSteps position =
    []


manhattan : Int -> Int
manhattan position =
    let
        zone =
            findZone position
    in
        (stepsAwayFromMiddle position zone) + (stepsToAccessPort zone)


stepsAwayFromMiddle : Int -> Zone -> Int
stepsAwayFromMiddle position zone =
    let
        distanceToMiddle =
            distanceToMiddleFor zone

        distanceFromCorner =
            distanceFromClosestCorner zone position

        stepsToMiddle =
            distance distanceToMiddle distanceFromCorner
    in
        if isOnACorner zone position then
            distanceToMiddle
        else
            stepsToMiddle


stepsToAccessPort : Zone -> Int
stepsToAccessPort zone =
    distanceToMiddleFor zone


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
