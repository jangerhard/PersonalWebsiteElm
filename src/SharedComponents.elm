module SharedComponents exposing (..)

import Element exposing (Attribute, Color, Element, centerX, el, height, px, width)
import Element.Background as Background


hrBreak : Int -> Int -> Color -> Element msg
hrBreak length thickness color =
    el [ height <| px thickness, width <| px length, Background.color color, centerX ]
        Element.none
