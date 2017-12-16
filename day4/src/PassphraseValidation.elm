module PassphraseValidation exposing (..)

import Set exposing (..)
import Regex exposing (..)
import List.Extra exposing (..)


type alias Passphrase =
    List Word


type alias Word =
    { word : String }


numberOfValidPassphrases : (Passphrase -> Bool) -> String -> Int
numberOfValidPassphrases validatorFn input =
    splitIntoPassphrases input
        |> List.filter validatorFn
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
    not <| hasAnagrams passphrase


hasAnagrams : Passphrase -> Bool
hasAnagrams passphrase =
    case passphrase of
        [] ->
            False

        one :: [] ->
            False

        h :: t ->
            hasAnagramIn h t || hasAnagrams t


hasAnagramIn : Word -> Passphrase -> Bool
hasAnagramIn word passphrase =
    List.any (isAnagramOf word) passphrase


isAnagramOf : Word -> Word -> Bool
isAnagramOf w1 w2 =
    let
        word1 =
            toLetters w1

        word2 =
            toLetters w2
    in
        containsAll word1 word2


toLetters : Word -> List String
toLetters { word } =
    Regex.split All (regex "") word


containsAll : List comparable -> List comparable -> Bool
containsAll list1 list2 =
    if List.length list1 /= List.length list2 then
        False
    else
        let
            sorted1 =
                List.sort list1

            sorted2 =
                List.sort list2

            zipped =
                List.Extra.zip sorted1 sorted2
        in
            List.all (\( fst, snd ) -> fst == snd) zipped


hasUniqueWords : Passphrase -> Bool
hasUniqueWords passphrase =
    let
        uniqueWords =
            passphrase
                |> List.map .word
                |> Set.fromList
    in
        List.length passphrase == Set.size uniqueWords
