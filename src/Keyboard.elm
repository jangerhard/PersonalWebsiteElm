module Keyboard exposing (KeyPress(..), sub, toKey)

import Browser.Events
import Json.Decode as Decode


sub : Sub KeyPress
sub =
    Browser.Events.onKeyDown keyDecoder


type KeyPress
    = Character Char
    | Number Int
    | Enter
    | Backspace
    | ControlKey String


toKey : String -> KeyPress
toKey keyValue =
    if keyValue == "Enter" then
        Enter

    else if keyValue == "Backspace" || keyValue == "⌫" then
        Backspace

    else
        case String.uncons keyValue of
            Just ( char, "" ) ->
                if Char.isAlpha char then
                    Character char

                else if Char.isDigit char then
                    String.fromChar char
                        |> String.toInt
                        |> Maybe.withDefault 0
                        |> Number

                else
                    ControlKey <| String.fromChar char

            _ ->
                ControlKey keyValue



-- Internal --


keyDecoder : Decode.Decoder KeyPress
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)
