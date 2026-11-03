module RouteTest exposing (suite)

import Expect
import Route exposing (Route(..))
import Test exposing (Test, describe, test)
import Url


parse : String -> Maybe Route
parse path =
    Url.fromString ("https://janschoepp.com" ++ path)
        |> Maybe.andThen Route.fromUrl


suite : Test
suite =
    describe "Route.fromUrl"
        [ test "parses path URLs" <|
            \_ -> parse "/contact" |> Expect.equal (Just Contact)
        , test "parses the root" <|
            \_ -> parse "/" |> Expect.equal (Just Main)
        , test "still accepts old hash URLs" <|
            \_ -> parse "/#/wordle" |> Expect.equal (Just Wordle)
        , test "round-trips through toUrl" <|
            \_ -> parse (Route.toUrl Education) |> Expect.equal (Just Education)
        , test "unknown paths are not found" <|
            \_ -> parse "/nope" |> Expect.equal Nothing
        ]
