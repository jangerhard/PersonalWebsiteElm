module Styling exposing (..)

import Element exposing (Color, rgb255)


type alias Palette =
    { dark_grey : Color
    , black : Color
    , white : Color
    , light_grey : Color
    , light_blue : Color
    , blue : Color
    , silver : Color
    , gold : Color
    , royal_blue : Color
    , red : Color
    }


palette : Palette
palette =
    { dark_grey = rgb255 33 33 33
    , black = rgb255 0 0 0
    , white = rgb255 255 255 255
    , light_grey = rgb255 230 230 230
    , light_blue = rgb255 240 248 255
    , blue = rgb255 188 210 238
    , silver = rgb255 192 192 192
    , gold = rgb255 226 219 33
    , royal_blue = rgb255 70 94 132
    , red = rgb255 198 40 40
    }
