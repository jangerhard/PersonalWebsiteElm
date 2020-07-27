module Main exposing (..)

import Browser
import Browser.Events exposing (onResize)
import Element exposing (Attribute, Color, Device, DeviceClass(..), Element, Orientation(..), alignLeft, alignRight, centerX, classifyDevice, column, el, fill, height, maximum, padding, paddingEach, paragraph, px, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes as Html
import Styling exposing (..)



---- CONSTANTS ----


gravatarUrl : Int -> String
gravatarUrl size =
    "https://en.gravatar.com/avatar/f217e346181d45ae9c2e5a834696f9b8?size=" ++ String.fromInt size


isPortraitOrPhone : Device -> Bool
isPortraitOrPhone { class, orientation } =
    class == Phone || orientation == Portrait



---- TYPES ----


type alias Flags =
    { width : Int
    , height : Int
    }


type alias SchoolShowcase =
    { name : String
    , title : String
    , location : String
    , gpa : Maybe Float
    , img : String
    , year : Int
    , month : Maybe String
    }



---- MODEL ----


type alias Model =
    { currentPage : Page
    , currentDevice : Device
    }


type Page
    = MainPage
    | ContactPage
    | EducationPage
    | ProjectsPage


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { currentPage = MainPage
      , currentDevice =
            classifyDevice { width = flags.width, height = flags.height }
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ChangePage Page
    | OnResize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangePage page ->
            ( { model | currentPage = page }, Cmd.none )

        OnResize w h ->
            let
                device =
                    classifyDevice { width = w, height = h }
            in
            ( { model | currentDevice = device }, Cmd.none )



---- VIEW ----


roundedImage : String -> Element Msg
roundedImage src =
    Element.html
        (Html.img [ Html.class "roundedImage", Html.src src ] [])


hrBreak : Int -> Int -> Color -> Element Msg
hrBreak length thickness color =
    el [ height <| px thickness, width <| px length, Background.color color, centerX ]
        Element.none


buttonStyling =
    [ Font.color light_grey
    , Font.bold
    , Font.size 15
    , padding 10
    ]


button : List (Attribute Msg) -> Maybe Msg -> Element Msg -> Element Msg
button additionalAttributes action label =
    Input.button
        (buttonStyling
            ++ additionalAttributes
        )
        { onPress = action, label = label }


menuButtons : Model -> Element Msg
menuButtons model =
    let
        alignment =
            if isPortraitOrPhone model.currentDevice then
                [ centerX ]

            else
                [ alignRight ]

        buttonText : Page -> String
        buttonText page =
            case page of
                MainPage ->
                    "Main"

                ContactPage ->
                    "Contact"

                EducationPage ->
                    "Education"

                ProjectsPage ->
                    "Projects"

        singleButton : Page -> Element Msg
        singleButton page =
            let
                highlightIfSelected =
                    if model.currentPage == page then
                        [ Font.underline ]

                    else
                        []
            in
            button highlightIfSelected (Just <| ChangePage page) (text <| (String.toUpper <| buttonText page))
    in
    row (alignment ++ [ Region.navigation ])
        [ singleButton ContactPage
        , singleButton EducationPage
        , singleButton ProjectsPage
        ]


headerElement : Model -> Element Msg
headerElement model =
    let
        columnOrRow =
            if isPortraitOrPhone model.currentDevice then
                column [ width fill, spacing 20 ]

            else
                row [ width fill ]

        homeButton : Element Msg
        homeButton =
            let
                align =
                    if isPortraitOrPhone model.currentDevice then
                        centerX

                    else
                        alignLeft

                logoAttributes =
                    [ width <| px 100
                    , height <| px 100
                    , align
                    ]

                logoImage =
                    el
                        [ width fill
                        , height fill
                        , Region.description
                            "Return to main page"
                        ]
                        (roundedImage (gravatarUrl 80))
            in
            case model.currentPage of
                MainPage ->
                    el logoAttributes Element.none

                _ ->
                    Input.button logoAttributes { onPress = Just <| ChangePage MainPage, label = logoImage }
    in
    columnOrRow
        [ homeButton
        , menuButtons model
        ]


boxAttributes : List (Attribute Msg)
boxAttributes =
    [ Background.color light_blue
    , Font.color black
    , Border.rounded 20
    , padding 20
    , Border.shadow
        { offset = ( 1, 1 )
        , size = 0.2
        , blur = 6
        , color = dark_grey
        }
    ]


educationElement : Device -> Element Msg
educationElement currentDevice =
    let
        isTabletAndPortrait =
            currentDevice.class == Tablet && currentDevice.orientation == Portrait

        attributes =
            [ spacing 30, centerX, Region.navigation ]

        columnOrRow =
            if isTabletAndPortrait || currentDevice.class == Phone then
                column attributes

            else
                wrappedRow attributes

        topTag schoolShowcase =
            if currentDevice.class /= Phone then
                row [ alignRight ]
                    [ column
                        [ Background.color blue
                        , padding 5
                        , Border.roundEach { topLeft = 0, bottomLeft = 5, bottomRight = 5, topRight = 0 }
                        ]
                        [ text <| String.fromInt schoolShowcase.year
                        , case schoolShowcase.month of
                            Just month ->
                                el [ Font.light ] (text month)

                            Nothing ->
                                Element.none
                        ]
                    , el [ padding 10 ] Element.none -- Extra padding
                    ]

            else
                Element.none

        showcaseElement : SchoolShowcase -> Element Msg
        showcaseElement schoolShowcase =
            let
                orientation =
                    if isTabletAndPortrait then
                        row [ alignLeft, spacing 40 ]

                    else
                        column [ centerX, spacing 15 ]

                schoolLogo : Element Msg
                schoolLogo =
                    let
                        logoSize =
                            if currentDevice.class == Phone then
                                px 50

                            else
                                px 125
                    in
                    el
                        [ if currentDevice.orientation == Landscape then
                            centerX

                          else
                            alignLeft
                        , width logoSize
                        , height logoSize
                        , Region.description (schoolShowcase.location ++ " logo")
                        ]
                        (roundedImage schoolShowcase.img)

                educationInfo : Element Msg
                educationInfo =
                    let
                        schoolInfo =
                            column [ centerX, width fill, spacing 10 ]
                                [ Element.paragraph [ centerX, Font.light, Font.size 27 ] [ text <| schoolShowcase.name ]
                                , el [ centerX, Font.light, Font.size 18 ] (text <| schoolShowcase.location)
                                ]

                        schoolHeader =
                            if currentDevice.class == Phone then
                                row [ spacing 5 ]
                                    [ schoolLogo
                                    , schoolInfo
                                    ]

                            else
                                schoolInfo
                    in
                    column [ spacing 15 ]
                        [ schoolHeader
                        , hrBreak 150 1 silver
                        , paragraph [ centerX ] [ text schoolShowcase.title ]
                        , case schoolShowcase.gpa of
                            Just gpa ->
                                el [ centerX, Font.light, Font.size 18 ] (text <| "GPA: " ++ String.fromFloat gpa)

                            Nothing ->
                                Element.none
                        ]

                sizeOfDiv =
                    if currentDevice.class == Phone then
                        [ width <| px 280 ]

                    else if currentDevice.class == Tablet && currentDevice.orientation == Portrait then
                        [ width <| px 450 ]

                    else
                        [ width <| px 270
                        , height
                            (fill |> maximum 350)
                        ]
            in
            el
                (sizeOfDiv
                    ++ boxAttributes
                    ++ [ Element.behindContent <| topTag schoolShowcase
                       , Element.mouseOver [ Background.color silver ]
                       ]
                )
                (orientation
                    [ if currentDevice.class /= Phone then
                        schoolLogo

                      else
                        Element.none
                    , educationInfo
                    ]
                )
    in
    columnOrRow
        [ showcaseElement
            { name = "Pace University"
            , title = "Master of Computer Science"
            , location = "New York City, USA"
            , gpa = Just 3.86
            , img = "images/pace_logo.jpg"
            , year = 2017
            , month = Just "May"
            }
        , showcaseElement
            { name = "Hong Kong Polytechnic University"
            , title = "Exchange Year"
            , location = "Hung Hom, Hong Kong"
            , img = "images/polyu_logo.png"
            , gpa = Nothing
            , year = 2013
            , month = Nothing
            }
        , showcaseElement
            { name = "University of Stavanger"
            , title = "Bachelor of Computer Engineering"
            , location = "Stavanger, Norway"
            , gpa = Nothing
            , img = "images/uis_logo.png"
            , year = 2014
            , month = Just "May"
            }
        ]


type alias ProjectShowcase =
    { title : String
    , tools : String
    , location : Maybe String
    , shortDesc : String
    , image : String
    , url : String
    , code : Maybe String
    }


projectsElement : Device -> Element Msg
projectsElement currentDevice =
    let
        attributes =
            [ spacing 30, centerX, Region.navigation ]

        columnOrRow =
            if currentDevice.class == Phone then
                column attributes

            else
                wrappedRow attributes

        projectElementContent : ProjectShowcase -> Element Msg
        projectElementContent projectShowcase =
            column [ padding 10, spacing 15, width (fill |> maximum 230), height <| px 330 ]
                [ el [ Font.light, Font.bold, Font.color royal_blue, Font.wordSpacing 2, Font.size 25, centerX ] (text projectShowcase.title)
                , hrBreak 50 3 gold
                , paragraph [ Font.light, Font.italic, Font.color royal_blue, Font.size 16, centerX ] [ text projectShowcase.tools ]
                , Element.image [] { src = projectShowcase.image, description = "" }
                , paragraph [ Font.size 16, Font.alignLeft ] [ text projectShowcase.shortDesc ]
                ]

        projectElementButton : ProjectShowcase -> Element Msg
        projectElementButton projectShowcase =
            Element.newTabLink
                (boxAttributes
                    ++ [ Region.description <| "Go to webpage for project titled " ++ projectShowcase.title ]
                )
                { url = projectShowcase.url, label = projectElementContent projectShowcase }
    in
    columnOrRow
        [ projectElementButton
            { title = "Personal Website"
            , tools = "Elm"
            , location = Nothing
            , image = "images/personalWebsite.png"
            , shortDesc = "Education and Projects Showcase in Elm"
            , url = "https://github.com/jangerhard/PersonalWebsiteElm"
            , code = Nothing
            }
        , projectElementButton
            { title = "Personal Website"
            , tools = "React, Gatsby"
            , location = Nothing
            , image = "images/personalWebsite.png"
            , shortDesc = "Education and Projects Showcase in React"
            , url = "https://github.com/jangerhard/PersonalGatsbyWebsite"
            , code = Nothing
            }
        , projectElementButton
            { title = "WalletWatcher"
            , tools = "Java (Android)"
            , location = Nothing
            , shortDesc = "App tracking scanned Bitcoin addresses' transactions"
            , image = "images/WalletWatcher.png"
            , url = "https://play.google.com/store/apps/details?id=io.github.jangerhard.BitcoinWalletTracker"
            , code = Just "https://github.com/jangerhard/BitcoinWalletTracker"
            }
        , projectElementButton
            { title = "Github Showcase"
            , tools = "React, GraphQL"
            , location = Nothing
            , shortDesc = "React component showcasing latest Github activity"
            , image = "images/githubshowcase.png"
            , url = "https://github.com/jangerhard/react-github-showcase#readme"
            , code = Just "https://github.com/jangerhard/react-github-showcase"
            }
        , projectElementButton
            { title = "QuizMaster"
            , tools = "Node.js, Firebase, Twilio"
            , location = Just "New York City, USA"
            , shortDesc = "Cellphone-based Trivia Game targeting areas without WIFI"
            , image = "images/quizMasterSmall.png"
            , url = "http://jangerhard-node.herokuapp.com/twiliopart2"
            , code = Just "https://github.com/jangerhard/TwilioEducation"
            }
        , projectElementButton
            { title = "AppliCafe"
            , tools = "Java (Android), Firebase"
            , location = Just "New York City, USA"
            , shortDesc = "Technology outreach project targeting Senegal"
            , image = "images/appdock.png"
            , url = "http://mobilesenegal.org/applicafe/"
            , code = Nothing
            }
        , projectElementButton
            { title = "NOMO3D: The Arc"
            , tools = "Java (Android), Bluetooth Low Energy, Arduino"
            , location = Just "Helsinki, Finland"
            , shortDesc = "Interface Controlling a mobile 3D Scanner"
            , image = "images/arc2.jpg"
            , url = "http://nomo3d.com/"
            , code = Nothing
            }
        , projectElementButton
            { title = "NeedTutor"
            , tools = "Java (Android), Bluetooth Low Energy"
            , location = Nothing
            , shortDesc = "App utilizing beacons to locate nearby available tutors"
            , image = "images/TutorsScreenshot.png"
            , url = "https://play.google.com/store/apps/details?id=com.pacemobilelab.TutorsAtSeidenberg&hl=en"
            , code = Just "https://github.com/paceuniversity/pacemobilelab/tree/master/Tutor"
            }
        ]


mainPageElement : Element Msg
mainPageElement =
    let
        urlButton : ( String, String ) -> Element Msg
        urlButton ( url, labelText ) =
            Element.newTabLink ([ padding 10, Border.width 1 ] ++ buttonStyling)
                { url = url, label = text (String.toUpper labelText) }
    in
    column [ centerX, spacing 20 ]
        [ el [ centerX, Border.rounded 100, Region.description "Profile photo" ] (roundedImage <| gravatarUrl 200)
        , paragraph [ centerX ] [ text "Full Stack Developer based in Oslo, Norway" ]
        , row [ spacing 20, centerX ]
            [ urlButton ( "https://www.linkedin.com/in/jangschoepp", "Career" )
            , urlButton ( "https://github.com/jangerhard", "Code" )
            ]
        ]


contactElement : Element Msg
contactElement =
    el (boxAttributes ++ [ centerX, width (fill |> maximum 650) ])
        (column [ centerX, spacing 30 ]
            [ el [ centerX, Font.size 36 ] (text "Get in touch!")
            , hrBreak 200 1 light_grey
            , wrappedRow [ spacing 60 ]
                [ column [ spacing 20 ]
                    [ el [ centerX ] (text "Gmail")
                    , el [ Font.light ] (text "jgschoepp")
                    ]
                , column [ spacing 20 ]
                    [ el [ centerX ] (text "Call my cell")
                    , el [ Font.light ] (text "+47 480 20 857")
                    ]
                ]
            ]
        )


pageElement : Model -> Element Msg
pageElement model =
    case model.currentPage of
        MainPage ->
            mainPageElement

        ContactPage ->
            contactElement

        EducationPage ->
            educationElement model.currentDevice

        ProjectsPage ->
            projectsElement model.currentDevice


viewHtml : Model -> Html Msg
viewHtml model =
    Element.layout
        [ Background.image "images/background.svg" ]
        (view model)


view : Model -> Element Msg
view model =
    column
        [ width (fill |> maximum 1350)
        , centerX
        , paddingEach { top = 20, bottom = 20, left = 75, right = 75 }
        , spacing 50
        , Font.family
            [ Font.typeface "Josefin Sans"
            , Font.sansSerif
            ]
        , Font.color white
        ]
        [ headerElement model
        , pageElement model
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onResize OnResize ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = viewHtml
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
