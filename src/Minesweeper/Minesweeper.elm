module Minesweeper.Minesweeper exposing (..)

import Array exposing (Array)
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, el, fill, height, padding, px, spacing, text, width)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Grid exposing (Grid)
import Html.Attributes
import Minesweeper.GameGrid as GameGrid exposing (..)
import Random
import Styling exposing (palette)


type Mode
    = Clicking
    | Flagging


type Game
    = Playing (Grid Tile)
    | Initializing
    | GameOver (Grid Tile)
    | Won (Grid Tile)


type alias Model =
    { game : Game, clickMode : Mode }


init : ( Model, Cmd Msg )
init =
    ( { game = Initializing, clickMode = Clicking }, resetGameMsg )


type Msg
    = ToggleMode
    | ClickTile Tile
    | InitGrid ( Grid Tile, Maybe Tile )
    | ResetGame
    | Cheat


resetGameMsg : Cmd Msg
resetGameMsg =
    Random.generate InitGrid (GameGrid.gridGenerator 20)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.game ) of
        ( ResetGame, _ ) ->
            ( model, resetGameMsg )

        ( InitGrid ( newGrid, randomBlank ), _ ) ->
            case randomBlank of
                Just tile ->
                    toggleBlank tile newGrid
                        |> (\updatedGrid ->
                                ( { model | game = Playing updatedGrid }, Cmd.none )
                           )

                -- No blanks present for some reason
                Nothing ->
                    ( model, resetGameMsg )

        ( ToggleMode, _ ) ->
            ( case model.clickMode of
                Clicking ->
                    { model | clickMode = Flagging }

                Flagging ->
                    { model | clickMode = Clicking }
            , Cmd.none
            )

        ( ClickTile clickedTile, Playing grid ) ->
            let
                updatedModel =
                    case model.clickMode of
                        Clicking ->
                            case ( clickedTile.state, clickedTile.content ) of
                                ( Hidden, Mine ) ->
                                    GameGrid.showTile clickedTile grid
                                        |> GameGrid.showAllFlagged
                                        |> (\g -> { model | game = GameOver g })

                                ( Hidden, Blank ) ->
                                    { model | game = Playing (GameGrid.toggleBlank clickedTile grid) }

                                ( Hidden, Number _ ) ->
                                    GameGrid.showTile clickedTile grid
                                        |> (\updatedGrid ->
                                                case GameGrid.isInWinState updatedGrid of
                                                    True ->
                                                        { model | game = Won updatedGrid }

                                                    False ->
                                                        { model | game = Playing updatedGrid }
                                           )

                                _ ->
                                    -- Do nothing on already shown and flagged
                                    model

                        Flagging ->
                            { model | game = Playing (GameGrid.flagTile clickedTile grid) }
            in
            ( updatedModel, Cmd.none )

        ( Cheat, Playing grid ) ->
            ( { model | game = Won (GameGrid.autoSolve grid) }, Cmd.none )

        _ ->
            ( model, Cmd.none )


tileView : Game -> Tile -> Element Msg
tileView game tile =
    let
        mine =
            Char.fromCode 0x0001F4A3
                |> String.fromChar

        flag =
            Char.fromCode 0x2691
                |> String.fromChar

        tileText : Element msg
        tileText =
            case ( game, tile.state, tile.content ) of
                ( Won _, _, Mine ) ->
                    el [ Element.Background.color palette.red, centerX ] (text mine)

                ( _, Shown, content ) ->
                    text <|
                        case content of
                            Mine ->
                                mine

                            Number int ->
                                String.fromInt int

                            Blank ->
                                " "

                ( _, Hidden, _ ) ->
                    el [ width fill, height fill, Element.Background.color palette.white ] Element.none

                ( _, Flagged, _ ) ->
                    el
                        [ width fill
                        , height fill
                        , Element.Background.color palette.white
                        , Element.Font.color palette.dark_grey
                        ]
                        (text flag)
    in
    Element.Input.button [ width <| px 20, height <| px 20 ]
        { onPress = Just (ClickTile tile)
        , label = tileText
        }


displayGrid : Game -> Grid Tile -> Element Msg
displayGrid game grid =
    let
        attributes =
            [ spacing 2 ]

        displayRow : Array Tile -> Element Msg
        displayRow tiles =
            let
                displayTile : Tile -> Element Msg
                displayTile tile =
                    tileView game tile
            in
            Element.row attributes
                (Array.map displayTile tiles
                    |> Array.toList
                )
    in
    Element.column attributes
        (Grid.rows grid
            |> Array.map displayRow
            |> Array.toList
        )


view : Model -> Element Msg
view model =
    let
        topRow : Element Msg
        topRow =
            Element.row [ centerX, width <| px 350 ]
                [ Element.Input.button [ alignLeft ] { onPress = Just ResetGame, label = text "New Game" }

                --, Element.Input.button [ centerX ] { onPress = Just Cheat, label = text "Cheat" }
                , Element.Input.button [ alignRight ]
                    { onPress = Just ToggleMode
                    , label =
                        case model.clickMode of
                            Clicking ->
                                text "Click"

                            Flagging ->
                                text "Flag"
                    }
                ]

        attributes =
            [ Element.Border.width 2
            , Element.Border.color palette.white
            , Element.padding 20
            , spacing 10
            , width fill
            , height fill
            ]

        blankOverlay : String -> Element.Attribute msg
        blankOverlay t =
            Element.inFront
                (el
                    [ width fill
                    , height fill
                    , Element.Font.size 64
                    , Element.Background.color palette.transparent_grey
                    ]
                    (el [ centerX, centerY, Element.Font.color palette.dark_grey ] (text t))
                )
    in
    Element.column
        attributes
        [ topRow
        , el [ centerY, centerX ] <|
            case model.game of
                Playing grid ->
                    displayGrid model.game grid

                Initializing ->
                    text "Initializing.."

                GameOver grid ->
                    el [ blankOverlay "Game over.." ] (displayGrid model.game grid)

                Won grid ->
                    el [ blankOverlay "You won!" ] (displayGrid model.game grid)
        ]
