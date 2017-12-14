module PassphraseValidation exposing (..)

import Set exposing (..)


type alias Passphrase =
    List String


numberOfValidPassphrases : String -> Int
numberOfValidPassphrases input =
    splitIntoPassphrases input
        |> List.filter hasUniqueWords
        |> List.length


splitIntoPassphrases : String -> List Passphrase
splitIntoPassphrases input =
    String.lines input
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> List.map String.words


hasNoAnagrams : Passphrase -> Bool
hasNoAnagrams passphrase =
    let
        reversed =
            List.map String.reverse passphrase

        -- this is not a good solution, because when the letters of a word can be rearranged (in any order), then it's considered an anagram
    in
        not <|
            List.any (\word -> List.member word reversed) passphrase


hasUniqueWords : Passphrase -> Bool
hasUniqueWords passphrase =
    let
        uniqueWords =
            Set.fromList passphrase
    in
        List.length passphrase == Set.size uniqueWords
