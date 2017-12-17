module MemoryBanks exposing (..)


type alias Banks =
    List Bank


banksFrom : List Block -> Banks
banksFrom =
    List.indexedMap (,)


type Redistributions
    = Redistributions Int


type alias Block =
    Int


type alias Index =
    Int


type alias Bank =
    ( Index, Block )


solve : Banks -> Redistributions
solve banks =
    Redistributions 0


bankWithMostBlocks : Banks -> Bank
bankWithMostBlocks banks =
    banks
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


redistribute : Block -> Banks -> Banks
redistribute toDistribute banks =
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
