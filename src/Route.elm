module Route exposing (Route(..), fromUrl, toUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s)


type Route
    = Main
    | Contact
    | Education
    | Projects
    | Minesweeper
    | Wordle


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Main Parser.top
        , Parser.map Contact (s "contact")
        , Parser.map Education (s "education")
        , Parser.map Projects (s "projects")
        , Parser.map Minesweeper (s "minesweeper")
        , Parser.map Wordle (s "wordle")
        ]



-- Public


fromUrl : Url -> Maybe Route
fromUrl url =
    case url.fragment of
        -- Old hash links (#/contact) from before path routing keep working.
        Just fragment ->
            if String.startsWith "/" fragment then
                Parser.parse parser { url | path = fragment, fragment = Nothing }

            else
                Parser.parse parser url

        Nothing ->
            Parser.parse parser url


toUrl : Route -> String
toUrl page =
    "/" ++ String.join "/" (routeToPieces page)


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Main ->
            []

        Contact ->
            [ "contact" ]

        Education ->
            [ "education" ]

        Projects ->
            [ "projects" ]

        Minesweeper ->
            [ "minesweeper" ]

        Wordle ->
            [ "wordle" ]
