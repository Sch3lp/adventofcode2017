module MemoryBanks exposing (..)


type alias Banks =
    List Bank


banksFrom : List Block -> Banks
banksFrom =
    List.indexedMap Bank


type Redistributions
    = Redistributions Int


type alias Block =
    Int


type alias Index =
    Int


type Bank
    = Bank Index Block


solve : Banks -> Redistributions
solve banks =
    Redistributions 0


bankWithMostBlocks : Banks -> Bank
bankWithMostBlocks banks =
    Bank 1 1



-- In each cycle, it finds the memory bank with the most blocks (ties won by the lowest-numbered memory bank) and redistributes those blocks among the banks. To do this, it removes all of the blocks from the selected bank, then moves to the next (by index) memory bank and inserts one of the blocks. It continues doing this until it runs out of blocks; if it reaches the last memory bank, it wraps around to the first one.


redistribute : Block -> Banks -> Banks
redistribute toDistribute banks =
    banks
