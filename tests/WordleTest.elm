module WordleTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Wordle.Wordle exposing (LetterState(..), verifyRow)


check : String -> String -> List LetterState
check answer guess =
    verifyRow (String.toList answer) (String.toList guess)
        |> List.map Tuple.second


suite : Test
suite =
    describe "verifyRow"
        [ test "marks every letter correct for the right word" <|
            \_ ->
                check "crane" "crane"
                    |> Expect.equal [ Correct, Correct, Correct, Correct, Correct ]
        , test "an exact match uses up the letter, so extra copies are incorrect" <|
            \_ ->
                check "crane" "eerie"
                    |> Expect.equal [ Incorrect, Incorrect, WrongSpot, Incorrect, Correct ]
        , test "only as many copies are marked wrong-spot as the answer has" <|
            \_ ->
                check "abide" "speed"
                    |> Expect.equal [ Incorrect, Incorrect, WrongSpot, Incorrect, WrongSpot ]
        ]
