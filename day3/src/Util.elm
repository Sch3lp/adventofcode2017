module Util exposing (..)


powerTwo : Int -> Int
powerTwo a =
    a ^ 2


nextUneven : Int -> Int
nextUneven a =
    if rem a 2 == 0 then
        a + 1
    else
        a
