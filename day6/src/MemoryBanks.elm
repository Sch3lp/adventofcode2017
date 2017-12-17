module MemoryBanks exposing (..)

import Array exposing (..)


type alias Blocks =
    Int


type alias Index =
    Int


type alias Bank =
    ( Index, Blocks )


type alias Banks =
    Array Bank


banksFrom : List Blocks -> Banks
banksFrom blocks =
    blocks
        |> List.indexedMap (,)
        |> Array.fromList


type Redistributions
    = Redistributions Int


solve : Banks -> Redistributions
solve banks =
    let
        loop : List Banks -> Banks -> List Banks
        loop configs banksuh =
            if List.member banksuh configs then
                configs
            else
                loop (configs ++ [ banksuh ]) <| Debug.log "cycle" <| cycle banksuh

        redists =
            List.length <| loop [] banks
    in
        Redistributions redists


bankWithMostBlocks : Banks -> Bank
bankWithMostBlocks banks =
    banks
        |> Array.toList
        |> sortByMostBlocksThenLowestIndex
        |> List.head
        |> Maybe.withDefault ( 0, 0 )


sortByMostBlocksThenLowestIndex =
    (sortWithDesc Tuple.second << List.sortBy Tuple.first)



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
        ( idxToDist, blocksToDist ) =
            bankWithMostBlocks banks

        toDistributeBanks =
            Array.set idxToDist ( idxToDist, 0 ) banks

        idxToStartRedist =
            idxToDist + 1
    in
        redistribute blocksToDist idxToStartRedist toDistributeBanks


redistribute : Blocks -> Index -> Banks -> Banks
redistribute blocks idx banks =
    if blocks == 0 then
        banks
    else
        let
            { nextIdx, toDistributeBanks, blocksToDist } =
                case Array.get idx banks of
                    Nothing ->
                        { nextIdx = 0, toDistributeBanks = banks, blocksToDist = blocks }

                    Just ( _, blocksAtIdx ) ->
                        let
                            toDistBanks =
                                Array.set idx ( idx, blocksAtIdx + 1 ) banks
                        in
                            { nextIdx = idx + 1, toDistributeBanks = toDistBanks, blocksToDist = blocks - 1 }
        in
            redistribute blocksToDist nextIdx toDistributeBanks



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
