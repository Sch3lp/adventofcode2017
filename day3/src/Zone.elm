module Zone exposing (..)

import List.Extra exposing (..)
import Util exposing (..)


type alias Zone =
    { corner : Int }


sideLength : Zone -> Int
sideLength zone =
    zoneNumber zone


zoneNumber : Zone -> Int
zoneNumber { corner } =
    toFloat corner
        |> sqrt
        |> ceiling


positionIsOnBottomRightCorner : Int -> Zone -> Bool
positionIsOnBottomRightCorner position { corner } =
    corner == position


isOnACorner : Zone -> Int -> Bool
isOnACorner zone position =
    corners zone |> List.any (\corner -> corner == position)


corners : Zone -> List Int
corners zone =
    let
        side =
            sideLength zone

        stepsInBetweenCorners =
            side - 1

        bottomRight =
            zone.corner

        bottomLeft =
            bottomRight - stepsInBetweenCorners

        topLeft =
            bottomLeft - stepsInBetweenCorners

        topRight =
            topLeft - stepsInBetweenCorners
    in
        [ bottomRight, bottomLeft, topLeft, topRight ]


closestCorner : Zone -> Int -> Int
closestCorner zone position =
    corners zone
        |> List.append [ (previousZone zone).corner ]
        |> List.map (\corner -> ( corner, abs (position - corner) ))
        |> Debug.log
            ("corners and their diffs from "
                ++ (toString position)
            )
        |> List.Extra.minimumBy (\( fst, snd ) -> snd)
        |> Maybe.map (\( fst, snd ) -> fst)
        |> Maybe.map
            (\theClosestCorner ->
                if (theClosestCorner == (previousZone zone).corner) then
                    zone.corner
                else
                    theClosestCorner
            )
        |> Maybe.withDefault 0


middleOfASide : Zone -> Int
middleOfASide zone =
    toFloat (sideLength zone) / 2 |> ceiling


nextZone : Zone -> Zone
nextZone zone =
    advanceZone Outwards zone


previousZone : Zone -> Zone
previousZone zone =
    advanceZone Inwards zone


type Sense
    = Outwards
    | Inwards


advanceZone : Sense -> Zone -> Zone
advanceZone sense zone =
    case sense of
        Outwards ->
            advZone (+) zone

        Inwards ->
            advZone (-) zone


advZone : (Int -> Int -> Int) -> Zone -> Zone
advZone fn zone =
    let
        nextCorner =
            powerTwo (fn (zoneNumber zone) 2)
    in
        Zone nextCorner
