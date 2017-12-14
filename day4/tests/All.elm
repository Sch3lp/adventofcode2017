module All exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import PassphraseValidation exposing (..)


suite : Test
suite =
    describe "PassphraseValidation"
        [ describe "numberOfInvalidPassphrases"
            [ test "aa bb cc dd => 0" <|
                \_ ->
                    """
                    aa bb cc dd
                    """
                        |> numberOfInvalidPassphrases
                        |> Expect.equal
                            0
            , test "aa bb cc aa => 1" <|
                \_ ->
                    """
                    aa bb cc aa
                    """
                        |> numberOfInvalidPassphrases
                        |> Expect.equal
                            1
            , test "2 invalid in multiple valid passphrase lines => 2" <|
                \_ ->
                    """
                    aa bb cc dd
                    aa bb cc aa
                    aa bb cc dd
                    aa bb cc bb
                    aa bb cc dd
                    aa bb cc dd
                    """
                        |> numberOfInvalidPassphrases
                        |> Expect.equal
                            2
            ]
        ]
