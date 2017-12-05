module Zone exposing (..)

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
    (corner - position) == 0


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
