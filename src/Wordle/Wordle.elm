module Wordle.Wordle exposing (Model, Msg, init, subscriptions, update, view)

import Element exposing (Element, spacing)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Keyboard exposing (KeyPress)
import Styling exposing (palette)
import Wordle.Words as Words


wordLength =
    5


maxRows =
    6


type alias Word =
    List Char


type GameState
    = InProgress
    | Winner
    | Loser


type alias Model =
    { wordToGuess : Word
    , currentGuess : Word
    , previousGuesses : List Word
    , currentState : GameState
    }


type Msg
    = KeyPressed KeyPress
    | GotWord String


init : ( Model, Cmd Msg )
init =
    ( { wordToGuess = []
      , currentGuess = []
      , previousGuesses = []
      , currentState = InProgress
      }
    , Words.getRandomWord GotWord "house"
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.currentState, msg ) of
        ( Winner, _ ) ->
            ( model, Cmd.none )

        ( Loser, _ ) ->
            ( model, Cmd.none )

        ( InProgress, GotWord w ) ->
            ( { model | wordToGuess = String.toList w }, Cmd.none )

        ( InProgress, KeyPressed keypress ) ->
            updateKeypress keypress model


updateKeypress : KeyPress -> Model -> ( Model, Cmd Msg )
updateKeypress keyPress model =
    case keyPress of
        Keyboard.Character c ->
            if List.length model.currentGuess < wordLength then
                ( { model
                    | currentGuess = model.currentGuess ++ List.singleton (Char.toLower c)
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Keyboard.Enter ->
            if List.length model.currentGuess == wordLength then
                let
                    newGuesses =
                        model.previousGuesses ++ List.singleton model.currentGuess
                in
                ( { model
                    | currentGuess = []
                    , previousGuesses = newGuesses
                    , currentState =
                        case ( List.length newGuesses > wordLength, isWinner model.wordToGuess model.currentGuess ) of
                            ( _, True ) ->
                                Winner

                            ( True, _ ) ->
                                Loser

                            _ ->
                                InProgress
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        Keyboard.Backspace ->
            ( { model | currentGuess = List.take (List.length model.currentGuess - 1) model.currentGuess }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Element Msg
view m =
    Element.column
        [ spacing 25
        , Element.above <|
            case m.currentState of
                InProgress ->
                    Element.none

                Winner ->
                    Element.el [ Element.moveUp 5, Element.centerX ] (Element.text "Congratulations!!")

                Loser ->
                    "You lost.. The correct word was: "
                        ++ String.fromList m.wordToGuess
                        |> Element.text
                        |> Element.el [ Element.moveUp 5, Element.centerX ]
        ]
        [ buildRows m, keyboardView m, footerView ]


footerView : Element msg
footerView =
    Element.column [ Element.spacing 10, Element.centerX ]
        [ Element.newTabLink [ Element.centerX ]
            { url = "https://www.powerlanguage.co.uk/wordle/", label = Element.text "Shameless clone of Wordle" }
        , Element.el [ Element.centerX ] (Element.text "Reload the page to guess a new word.")
        ]


buildRows : Model -> Element msg
buildRows { wordToGuess, currentGuess, previousGuesses } =
    let
        wordRow : Bool -> Word -> Element msg
        wordRow shouldVerify word =
            (if shouldVerify then
                verifyRow wordToGuess word

             else
                List.map (\c -> ( c, NotVerified )) word
            )
                |> List.map letterElement
                |> Element.row [ Element.spacing 5, Element.centerX ]

        emptyRows : List (Element msg)
        emptyRows =
            List.repeat wordLength ' '
                |> wordRow False
                |> List.repeat maxRows

        currentRow : Element msg
        currentRow =
            String.fromList currentGuess
                |> String.padRight wordLength ' '
                |> String.toList
                |> wordRow False

        previousGuessesRows : List (Element msg)
        previousGuessesRows =
            List.map (wordRow True) previousGuesses
    in
    previousGuessesRows
        ++ currentRow
        :: emptyRows
        |> List.take maxRows
        |> Element.column [ Element.spacing 5, Element.centerX ]


verifyRow : Word -> Word -> List ( Char, LetterState )
verifyRow wordToGuess currentWord =
    let
        correctSpot : Int -> Char -> Bool
        correctSpot index char =
            List.head (List.drop index wordToGuess) == Just char

        incorrectSpot : Char -> Bool
        incorrectSpot char =
            List.member char wordToGuess

        verifyChar : Int -> Char -> ( Char, LetterState )
        verifyChar index char =
            if correctSpot index char then
                ( char, Correct )

            else if incorrectSpot char then
                ( char, WrongSpot )

            else
                ( char, Incorrect )
    in
    List.indexedMap verifyChar currentWord


isWinner : Word -> Word -> Bool
isWinner wordToGuess w =
    verifyRow wordToGuess w
        |> List.all (\( _, letterState ) -> letterState == Correct)


type LetterState
    = Correct
    | WrongSpot
    | Incorrect
    | NotVerified


letterElement : ( Char, LetterState ) -> Element msg
letterElement ( s, state ) =
    Element.el
        [ Element.Border.width 2
        , Element.Border.color palette.transparent_grey
        , Element.Background.color <| stateToColor state
        , Element.width <| Element.px 72
        , Element.height <| Element.px 72
        , Element.Font.size 42
        , Element.Font.bold
        ]
        (Char.toUpper s
            |> String.fromChar
            |> Element.text
            |> Element.el [ Element.centerX, Element.centerY ]
        )


stateToColor : LetterState -> Element.Color
stateToColor state =
    case state of
        Correct ->
            palette.green

        WrongSpot ->
            palette.gold

        Incorrect ->
            palette.grey

        NotVerified ->
            palette.transparent_grey


subscriptions : Sub Msg
subscriptions =
    Sub.map KeyPressed Keyboard.sub


keyboardView : Model -> Element Msg
keyboardView m =
    let
        usedLetters : List Char
        usedLetters =
            m.previousGuesses
                ++ List.singleton m.currentGuess
                |> List.concat

        button : String -> Element Msg
        button s =
            Element.Input.button
                [ Element.padding 10
                , Element.Border.rounded 10
                , case String.uncons s of
                    Just ( char, "" ) ->
                        case List.member (Char.toLower char) usedLetters of
                            True ->
                                Element.Background.color palette.grey

                            False ->
                                Element.Background.color palette.transparent_grey

                    _ ->
                        Element.Background.color palette.transparent_grey
                ]
                { onPress = Just <| KeyPressed (Keyboard.toKey s), label = Element.text s }

        keyboardRow : String -> Element Msg
        keyboardRow letters =
            String.toList letters
                |> List.map (\s -> button (String.fromChar s))
                |> Element.row [ Element.spacing 5, Element.centerX ]
    in
    Element.column [ Element.width Element.fill, Element.spacing 5 ]
        [ keyboardRow "QWERTYUIOP"
        , keyboardRow "ASDFGHJKL"
        , Element.row [ Element.spacing 5 ] [ button "Enter", keyboardRow "ZXCVBNM", button "⌫" ]
        ]
