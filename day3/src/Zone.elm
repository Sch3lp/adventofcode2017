module Zone exposing (..)

import Util exposing (..)


type alias Zone =
    { corner : Int }


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
advZone fn { corner } =
    let
        zoneNumber =
            toFloat corner
                |> sqrt
                |> ceiling

        nextCorner =
            powerTwo (fn zoneNumber 2)
    in
        Zone nextCorner
