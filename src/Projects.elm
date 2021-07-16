module Projects exposing (..)

import Element exposing (..)
import Element.Font as Font
import SharedComponents
import Styling exposing (palette)
import Url.Builder


type alias ProjectShowcase =
    { title : String
    , tools : String
    , location : Maybe String
    , shortDesc : String
    , image : String
    , url : String
    , code : Maybe String
    }


view : ProjectShowcase -> Element msg
view projectShowcase =
    column
        [ padding 10
        , spacing 15
        , width (fill |> maximum 230)
        , height <| px 330
        ]
        [ el
            [ Font.light
            , Font.bold
            , Font.color palette.royal_blue
            , Font.wordSpacing 2
            , Font.size 25
            , centerX
            ]
            (text projectShowcase.title)
        , SharedComponents.hrBreak 50 3 palette.gold
        , paragraph
            [ Font.light
            , Font.italic
            , Font.color palette.royal_blue
            , Font.size 16
            , centerX
            ]
            [ text projectShowcase.tools ]
        , Element.image [ centerX, width <| px 200, height <| px 130 ] { src = projectShowcase.image, description = "" }
        , paragraph [ Font.size 16, Font.alignLeft ] [ text projectShowcase.shortDesc ]
        ]


allProjects : List ProjectShowcase
allProjects =
    [ { title = "Minesweeper"
      , tools = "Elm"
      , location = Nothing
      , image = "images/minesweeper.png"
      , shortDesc = "Minesweeper clone"
      , url = Url.Builder.relative [ "#", "minesweeper" ] []
      , code = Nothing
      }
    , { title = "Personal Website"
      , tools = "Elm"
      , location = Nothing
      , image = "images/personalWebsite.png"
      , shortDesc = "Education and Projects Showcase in Elm"
      , url = "https://github.com/jangerhard/PersonalWebsiteElm"
      , code = Nothing
      }
    , { title = "Personal Website"
      , tools = "React, Gatsby"
      , location = Nothing
      , image = "images/personalWebsite.png"
      , shortDesc = "Education and Projects Showcase in React"
      , url = "https://github.com/jangerhard/PersonalGatsbyWebsite"
      , code = Nothing
      }
    , { title = "WalletWatcher"
      , tools = "Java (Android)"
      , location = Nothing
      , shortDesc = "App tracking scanned Bitcoin addresses' transactions"
      , image = "images/WalletWatcher.png"
      , url = "https://play.google.com/store/apps/details?id=io.github.jangerhard.BitcoinWalletTracker"
      , code = Just "https://github.com/jangerhard/BitcoinWalletTracker"
      }
    , { title = "Github Showcase"
      , tools = "React, GraphQL"
      , location = Nothing
      , shortDesc = "React component showcasing latest Github activity"
      , image = "images/githubshowcase.png"
      , url = "https://github.com/jangerhard/react-github-showcase#readme"
      , code = Just "https://github.com/jangerhard/react-github-showcase"
      }
    , { title = "QuizMaster"
      , tools = "Node.js, Firebase, Twilio"
      , location = Just "New York City, USA"
      , shortDesc = "Cellphone-based Trivia Game targeting areas without WIFI"
      , image = "images/quizMasterSmall.png"
      , url = "http://jangerhard-node.herokuapp.com/twiliopart2"
      , code = Just "https://github.com/jangerhard/TwilioEducation"
      }
    , { title = "AppliCafe"
      , tools = "Java (Android), Firebase"
      , location = Just "New York City, USA"
      , shortDesc = "Technology outreach project targeting Senegal"
      , image = "images/appdock.png"
      , url = "http://mobilesenegal.org/applicafe/"
      , code = Nothing
      }
    , { title = "NOMO3D: The Arc"
      , tools = "Java (Android), Bluetooth Low Energy, Arduino"
      , location = Just "Helsinki, Finland"
      , shortDesc = "Interface Controlling a mobile 3D Scanner"
      , image = "images/arc2.jpg"
      , url = "http://nomo3d.com/"
      , code = Nothing
      }
    , { title = "NeedTutor"
      , tools = "Java (Android), Bluetooth Low Energy"
      , location = Nothing
      , shortDesc = "App utilizing beacons to locate nearby available tutors"
      , image = "images/TutorsScreenshot.png"
      , url = "https://play.google.com/store/apps/details?id=com.pacemobilelab.TutorsAtSeidenberg&hl=en"
      , code = Just "https://github.com/paceuniversity/pacemobilelab/tree/master/Tutor"
      }
    ]
