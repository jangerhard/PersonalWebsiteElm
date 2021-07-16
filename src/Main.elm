module Main exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav exposing (Key)
import Element exposing (Attribute, Color, Device, DeviceClass(..), Element, Orientation(..), alignLeft, alignRight, centerX, centerY, classifyDevice, column, el, fill, height, maximum, padding, paddingEach, paragraph, px, row, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes as Html
import Minesweeper.Minesweeper as Minesweeper
import Route exposing (Route)
import Styling exposing (..)
import Url exposing (Url)



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
    , navKey : Nav.Key
    }


type Page
    = Root
    | ContactPage
    | EducationPage
    | ProjectsPage
    | MinesweeperPage Minesweeper.Model
    | NotFound


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    changeRouteTo (Route.fromUrl url)
        { currentPage = Root
        , navKey = navKey
        , currentDevice =
            classifyDevice { width = flags.width, height = flags.height }
        }


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        ( currentPage, commands ) =
            case maybeRoute of
                Just route ->
                    case route of
                        Route.Main ->
                            ( Root, Cmd.none )

                        Route.Contact ->
                            ( ContactPage, Cmd.none )

                        Route.Education ->
                            ( EducationPage, Cmd.none )

                        Route.Projects ->
                            ( ProjectsPage, Cmd.none )

                        Route.Minesweeper ->
                            Minesweeper.init
                                |> (\( m, subCommands ) ->
                                        ( MinesweeperPage m, Cmd.map GotMinesweeperMsg subCommands )
                                   )

                Nothing ->
                    ( NotFound, Cmd.none )
    in
    ( { model | currentPage = currentPage }, commands )



---- UPDATE ----


type Msg
    = OnResize Int Int
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotMinesweeperMsg Minesweeper.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.currentPage ) of
        ( OnResize w h, _ ) ->
            let
                device =
                    classifyDevice { width = w, height = h }
            in
            ( { model | currentDevice = device }, Cmd.none )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( GotMinesweeperMsg subMsg, MinesweeperPage subModel ) ->
            let
                ( updatedModel, updateMsg ) =
                    Minesweeper.update subMsg subModel
            in
            ( { model | currentPage = MinesweeperPage updatedModel }
            , Cmd.map GotMinesweeperMsg updateMsg
            )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )



---- VIEW ----


roundedImage : String -> Element Msg
roundedImage src =
    Element.html
        (Html.img [ Html.class "roundedImage", Html.src src ] [])


hrBreak : Int -> Int -> Color -> Element msg
hrBreak length thickness color =
    el [ height <| px thickness, width <| px length, Background.color color, centerX ]
        Element.none


buttonStyling =
    [ Font.color palette.light_grey
    , Font.bold
    , Font.size 15
    , padding 10
    ]


button : List (Attribute Msg) -> String -> Element Msg -> Element Msg
button additionalAttributes url label =
    Element.link
        (buttonStyling
            ++ additionalAttributes
        )
        { url = url, label = label }


menuButtons : Model -> Element Msg
menuButtons model =
    let
        alignment =
            if isPortraitOrPhone model.currentDevice then
                [ centerX ]

            else
                [ alignRight ]

        singleButton : Route -> Page -> String -> Element Msg
        singleButton route page bText =
            let
                buttonText =
                    text <| (String.toUpper <| bText)

                highlightIfSelected =
                    if model.currentPage == page then
                        [ Font.underline ]

                    else
                        []
            in
            button highlightIfSelected (Route.toUrl route) buttonText
    in
    row (alignment ++ [ Region.navigation ])
        [ singleButton Route.Contact ContactPage "Contact"
        , singleButton Route.Education EducationPage "Education"
        , singleButton Route.Projects ProjectsPage "Projects"
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
                Root ->
                    el logoAttributes Element.none

                _ ->
                    Element.link logoAttributes { url = Route.toUrl Route.Main, label = logoImage }
    in
    columnOrRow
        [ homeButton
        , menuButtons model
        ]


boxAttributes : List (Attribute msg)
boxAttributes =
    [ Background.color palette.light_blue
    , Font.color palette.black
    , Border.rounded 20
    , Element.htmlAttribute (Html.style "marginLeft" "auto")
    , Element.htmlAttribute (Html.style "marginRight" "auto")
    , padding 20
    , Border.shadow
        { offset = ( 1, 1 )
        , size = 0.2
        , blur = 6
        , color = palette.dark_grey
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
                        [ Background.color palette.blue
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
                        [ if not (currentDevice.class == Phone) then
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
                        , hrBreak 150 1 palette.silver
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
                [ width fill
                ]
                (el
                    (sizeOfDiv
                        ++ boxAttributes
                        ++ [ Element.behindContent <| topTag schoolShowcase
                           , Element.mouseOver [ Background.color palette.silver ]
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
            [ spacing 30, centerX ]

        columnOrRow =
            if currentDevice.class == Phone then
                column attributes

            else
                wrappedRow attributes

        projectElementContent : ProjectShowcase -> Element Msg
        projectElementContent projectShowcase =
            column [ padding 10, spacing 15, width (fill |> maximum 230), height <| px 330 ]
                [ el [ Font.light, Font.bold, Font.color palette.royal_blue, Font.wordSpacing 2, Font.size 25, centerX ] (text projectShowcase.title)
                , hrBreak 50 3 palette.gold
                , paragraph [ Font.light, Font.italic, Font.color palette.royal_blue, Font.size 16, centerX ] [ text projectShowcase.tools ]
                , Element.image [ centerX ] { src = projectShowcase.image, description = "" }
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
            Element.newTabLink ([ width <| px 85, padding 10, Border.width 1 ] ++ buttonStyling)
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


contactElement : Element msg
contactElement =
    el (boxAttributes ++ [ centerX, width (fill |> maximum 650) ])
        (column [ centerX, spacing 30 ]
            [ el [ centerX, Font.size 36 ] (text "Get in touch!")
            , hrBreak 200 1 palette.light_grey
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
        Root ->
            mainPageElement

        ContactPage ->
            contactElement

        EducationPage ->
            educationElement model.currentDevice

        ProjectsPage ->
            projectsElement model.currentDevice

        NotFound ->
            column [ centerX, spacing 20 ]
                [ el [ centerX ] (text "You seem lost..")
                , Element.image [ centerX, centerY ]
                    { src = "images/confused.gif"
                    , description = "No page found"
                    }
                ]

        MinesweeperPage m ->
            el [ centerX, centerY ] (Minesweeper.view m)
                |> Element.map GotMinesweeperMsg


viewHtml : Model -> Document Msg
viewHtml model =
    { title = "Jan Schøpp's Personal Website"
    , body =
        [ Element.layout
            [ Background.image "images/background.svg" ]
            (view model)
        ]
    }


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
        , Font.color palette.white
        ]
        [ headerElement model
        , pageElement model
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize OnResize



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { view = viewHtml
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }
