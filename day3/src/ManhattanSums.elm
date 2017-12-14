-- 47419   47272   46983   46561   46164   45850   22864
-- 95142     147     142     133     122      59   22805
-- 95923     304       5       4       2      57   13681
-- 96908     330      10       1       1      54    9008
-- 97951     351      11      23      25      26    4477
-- 98664     362     747     806     880    1760    2691 436638
-- 99026  198052  199967  202400  205846  211177  215628 218319
-- points
-- (-1,1)   (0,1)   (1,1)
-- (-1,0)   (0,0)   (1,0)
-- (-1,-1)  (-1,0)  (1,-1)


module ManhattanSums exposing (..)

import Dict exposing (..)


type alias Point =
    ( Int, Int )


type alias Spiral =
    Dict Point Int


type Times
    = None
    | Some Int


accessPort : Point
accessPort =
    ( 0, 0 )


createSpiral : Point -> Times -> Spiral
createSpiral start times =
    case times of
        None ->
            Dict.empty

        Some amount ->
            Dict.singleton start 1
                |> expandSpiral amount


expandSpiral : Int -> Spiral -> Spiral
expandSpiral acc spiral =
    if acc == 0 then
        spiral
    else
        let
            amount =
                acc - 1

            point =
                determinePoint acc

            newSpiral =
                Dict.insert point 1 spiral
        in
            expandSpiral amount newSpiral



-- Manhattan/Zone would be useful if it'd been able to determine the quadrant the position is in
-- relative to the access port (0,0), or 1 in the previous exercise


determinePoint : Int -> Point
determinePoint position =
    ( 0, 0 )
