module Minesweeper.RandomHelpers exposing (chooseRandom, sequence)

{-| Sample without replacement: produce a randomly selected element of the
list, and the list with that element omitted. If the list is empty, the
selected element will be `Nothing`.
-}

import Random exposing (Generator, constant)


{-| Get nth element of the list. If the list is empty, the selected element
will be `Nothing`.
-}
get : Int -> List a -> Maybe a
get index list =
    list
        |> List.drop index
        |> List.head


chooseRandom : List a -> Generator ( Maybe a, List a )
chooseRandom list =
    if List.isEmpty list then
        constant ( Nothing, list )

    else
        let
            lastIndex =
                List.length list - 1

            front i =
                List.take i list

            back i =
                List.drop (i + 1) list

            gen =
                Random.int 0 lastIndex
        in
        Random.map
            (\index ->
                ( get index list, List.append (front index) (back index) )
            )
            gen


sequence : List (Random.Generator a) -> Random.Generator (List a)
sequence =
    List.foldr (Random.map2 (::)) (Random.constant [])
