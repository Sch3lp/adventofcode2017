module PassphraseValidation exposing (..)

import Set exposing (..)
import Regex exposing (..)


type alias Passphrase =
    List Word


type alias Word =
    { word : String }


numberOfValidPassphrases : String -> Int
numberOfValidPassphrases input =
    splitIntoPassphrases input
        |> List.filter hasUniqueWords
        |> List.length


splitIntoPassphrases : String -> List Passphrase
splitIntoPassphrases input =
    input
        |> String.lines
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> List.map String.words
        |> List.map (\words -> List.map Word words)


hasNoAnagrams : Passphrase -> Bool
hasNoAnagrams passphrase =
    False


toLetters : Word -> List String
toLetters { word } =
    Regex.split All (regex "") word


containsAll : List a -> List a -> Bool
containsAll list1 list2 =
    List.all (\li -> List.member li list2) list1


hasUniqueWords : Passphrase -> Bool
hasUniqueWords passphrase =
    let
        uniqueWords =
            passphrase
                |> List.map .word
                |> Set.fromList
    in
        List.length passphrase == Set.size uniqueWords
