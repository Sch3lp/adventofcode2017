module Manhattan exposing (..)


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


type alias Zone =
    { corner : Int }


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


nextUneven : Int -> Int
nextUneven a =
    if rem a 2 == 0 then
        a + 1
    else
        a


powerTwo : Int -> Int
powerTwo a =
    a ^ 2
