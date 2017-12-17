module MemoryBanks exposing (..)

import Array exposing (..)


type alias Block =
    Int


type alias Index =
    Int


type alias Bank =
    ( Index, Block )


type alias Banks =
    Array Bank


banksFrom : List Block -> Banks
banksFrom blocks =
    blocks
        |> List.indexedMap (,)
        |> Array.fromList


type Redistributions
    = Redistributions Int


solve : Banks -> Redistributions
solve banks =
    Redistributions 0


bankWithMostBlocks : Banks -> Bank
bankWithMostBlocks banks =
    banks
        |> Array.toList
        |> (sortWithDesc Tuple.second << List.sortBy Tuple.first)
        |> List.head
        |> Maybe.withDefault ( 0, 0 )



-- In each cycle,
-- it finds the memory bank with the most blocks (ties won by the lowest-numbered memory bank) and
-- redistributes those blocks among the banks.
-- To do this, it removes all of the blocks from the selected bank,
-- then moves to the next (by index) memory bank and inserts one of the blocks.
-- It continues doing this until it runs out of blocks;
-- if it reaches the last memory bank, it wraps around to the first one.


cycle : Banks -> Banks
cycle banks =
    let
        toDistributeBank =
            bankWithMostBlocks banks
    in
        banks



-- Util


desc : (a -> comparable) -> (a -> a -> Order)
desc extractor left right =
    case compare (extractor left) (extractor right) of
        EQ ->
            EQ

        LT ->
            GT

        GT ->
            LT


sortWithDesc : (a -> comparable) -> List a -> List a
sortWithDesc extractor =
    List.sortWith <| desc <| extractor
