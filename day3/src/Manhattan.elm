module Manhattan exposing (..)

import List.Extra exposing (..)


type Direction
    = Up
    | Right
    | Down
    | Left


next : Direction -> Direction
next from =
    case from of
        Right ->
            Up

        Up ->
            Left

        Left ->
            Down

        Down ->
            Right


previous : Direction -> Direction
previous from =
    case from of
        Right ->
            Down

        Down ->
            Left

        Left ->
            Up

        Up ->
            Right



-- both Left and Right add 1 everytime they start anew
-- Down is always the same as Left
-- Up is always the same as Right
-- [ Right, Up, Left, Left, Down, Down, Right, Right, Right, Up, Up, Up, Left, Left, Left, Left, Down, Down, Down, Down ]
-- [ 1    , 2 , 3   , 4   , 5   , 6   , 7    , 8    , 9    , 10, 11, 12, 13  , 14  , 15  , 16  , 17  , 18  , 19  , 20   ]


spiral : Int -> List Direction
spiral position =
    spiralHelper position []


spiralHelper : Int -> List Direction -> List Direction
spiralHelper acc steps =
    if acc == 0 then
        steps
    else
        let
            newSteps =
                steps ++ [ nextDirection steps ]
        in
            spiralHelper (acc - 1) newSteps


nextDirection : List Direction -> Direction
nextDirection steps =
    case List.reverse steps of
        [] ->
            Right

        h :: t ->
            let
                stepsInSameDirection =
                    List.Extra.takeWhile (\step -> step == h) steps |> List.length

                stepsInPreviousDirection =
                    steps
                        |> List.Extra.dropWhile (\step -> step == h)
                        |> List.Extra.takeWhile
                            (\step -> step == previous h)
                        |> List.length
            in
                case stepsInSameDirection <= stepsInPreviousDirection of
                    True ->
                        next h

                    False ->
                        h


manhattan : Int -> Int
manhattan position =
    manhattanSteps position
        |> List.length


manhattanSteps : Int -> List Direction
manhattanSteps position =
    []
