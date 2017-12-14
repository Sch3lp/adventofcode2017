module PassphraseValidation exposing (..)

import Set exposing (..)


type alias Passphrase =
    List String


numberOfValidPassphrases : String -> Int
numberOfValidPassphrases input =
    splitIntoPassphrases input
        |> List.filter isValid
        |> List.length


splitIntoPassphrases : String -> List Passphrase
splitIntoPassphrases input =
    String.lines input
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)
        |> List.map String.words


isValid : Passphrase -> Bool
isValid passphrase =
    let
        uniqueWords =
            Set.fromList passphrase
    in
        List.length passphrase == Set.size uniqueWords
