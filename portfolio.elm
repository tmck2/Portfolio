module Portfolio exposing (..)

import Html exposing (div, text, h1, img, a, select, option)
import Html.Attributes exposing (src, href)
import Html.Events exposing (onInput)
import Html.App as App
import Json.Decode as Decode exposing (..)
import Http
import Task
import String
import List.Extra exposing (unique)


-- MODEL


type alias PortfolioEntry =
    { title : String
    , picture : Maybe String
    , architecture : List String
    , technologies : List String
    , link : Maybe String
    , description : String
    }


type alias Model =
    { portfolioEntries : List PortfolioEntry
    , filter : String
    }


type Msg
    = PortfolioRetrieved (List PortfolioEntry)
    | PortfolioRetrievalError Http.Error
    | FilterChanged String


init =
    ( { portfolioEntries = [], filter = "All" }
    , Task.perform PortfolioRetrievalError PortfolioRetrieved (Http.get portfolioDecoder "portfolio.json")
    )


entryDecoder =
    object6
        (\title picture architecture technologies link description ->
            { title = title
            , picture = picture
            , architecture = architecture
            , technologies = technologies
            , link = link
            , description = description
            }
        )
        ("title" := string)
        (maybe ("picture" := string))
        ("architecture" := list string)
        ("technologies" := list string)
        (maybe ("link" := string))
        ("description" := string)


portfolioDecoder =
    list entryDecoder



-- UPDATE


update msg model =
    case msg of
        PortfolioRetrieved entries ->
            ( { model | portfolioEntries = entries }, Cmd.none )

        PortfolioRetrievalError error ->
            ( model, Cmd.none )

        FilterChanged filter ->
            ( { model | filter = filter }, Cmd.none )



-- VIEW


viewImage image =
    Maybe.withDefault (text "") <|
        Maybe.map (\url -> img [ src url ] []) image


viewLink link =
    Maybe.withDefault (text "") <|
        Maybe.map (\url -> a [ href url ] [ text "Link" ]) link


joinList : String -> List String -> String
joinList sep lst =
    String.dropLeft (String.length sep) <|
        List.foldl (\str acc -> acc ++ sep ++ str) "" lst


viewEntry entry =
    div []
        [ h1 []
            [ text entry.title ]
        , div []
            [ text <| "Architecture: " ++ joinList ", " entry.architecture ]
        , div []
            [ text <| "Technologies: " ++ joinList ", " entry.technologies ]
        , div [] [ viewLink entry.link ]
        , div [] [ viewImage entry.picture ]
        , text entry.description
        ]


filterEntries technology entries =
    if technology == "All" then
        entries
    else
        entries |> List.filter (\entry -> List.any (\str -> str == technology) entry.technologies)


view model =
    div []
        [ select [ onInput FilterChanged ]
            ([ "All" ]
                :: List.map
                    (\entry -> entry.technologies)
                    model.portfolioEntries
                |> List.concat
                |> unique
                |> List.map (\str -> option [] [ text str ])
            )
        , div []
            (model.portfolioEntries
                |> filterEntries model.filter
                |> List.map viewEntry
            )
        ]



-- MAIN


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
