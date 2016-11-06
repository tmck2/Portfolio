module Portfolio exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (src, href, style, value)
import Html.Events exposing (onInput)
import Html.App as App
import Json.Decode as Decode exposing (string, list, (:=), maybe, object6)
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
    , filter : ( String, String )
    }


type FilterId
    = ArchitectureFilter
    | TechnologyFilter


type Msg
    = PortfolioRetrieved (List PortfolioEntry)
    | PortfolioRetrievalError Http.Error
    | FilterChanged FilterId String


init =
    ( { portfolioEntries = [], filter = ( "All", "All" ) }
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

        FilterChanged id str ->
            case id of
                ArchitectureFilter ->
                    ( { model | filter = ( str, "All" ) }, Cmd.none )

                TechnologyFilter ->
                    ( { model | filter = ( "All", str ) }, Cmd.none )



-- VIEW


notVisible =
    text ""


viewImage image =
    Maybe.withDefault notVisible <|
        Maybe.map (\url -> img [ src url, style [ ( "width", "150px" ) ] ] []) image


viewLink link =
    Maybe.withDefault notVisible <|
        Maybe.map (\url -> a [ href url ] [ text "Link" ]) link


joinList : String -> List String -> String
joinList sep lst =
    String.dropLeft (String.length sep) <|
        List.foldl (\str acc -> acc ++ sep ++ str) "" lst


viewEntry entry =
    div []
        [ h2 []
            [ text entry.title ]
        , div []
            [ text <| "Architecture: " ++ joinList ", " entry.architecture ]
        , div []
            [ text <| "Technologies: " ++ joinList ", " entry.technologies ]
        , div [] [ viewLink entry.link ]
        , div []
            [ h3 [] [ text "Description:" ]
            , viewImage entry.picture
            ]
        , text entry.description
        ]


filterEntries (( archFilter, techFilter ) as technology) entries =
    entries
        |> List.filter
            (\entry ->
                List.any (\str -> (str == techFilter || techFilter == "All")) entry.technologies
                    && List.any (\str -> (str == archFilter || archFilter == "All")) entry.architecture
            )


viewSelect labelText onInputMessage selectedValue list =
    div [ Attr.class "form-group" ]
        [ label [] [ text labelText ]
        , select
            [ Attr.class "form-control"
            , onInput onInputMessage
            , Attr.value selectedValue
            ]
            ("All" :: list |> List.map (\str -> option [] [ text str ]))
        ]


uniqueConcatMap projection list =
    list |> List.map projection |> List.concat |> unique


viewFilters ({ filter } as model) =
    form []
        [ h2 [] [ text "Filters" ]
        , viewSelect "Architecture"
            (FilterChanged ArchitectureFilter)
            (fst filter)
            (uniqueConcatMap (\entry -> entry.architecture) model.portfolioEntries)
        , viewSelect "Technology"
            (FilterChanged TechnologyFilter)
            (snd filter)
            (uniqueConcatMap (\entry -> entry.technologies) model.portfolioEntries)
        ]


view model =
    div []
        [ h1 [] [ text "Portfolio" ]
        , div [ Attr.class "row" ]
            [ div [ Attr.class "col-md-2" ] [ viewFilters model ]
            , div [ Attr.class "col-md-10" ]
                (model.portfolioEntries
                    |> filterEntries model.filter
                    |> List.map viewEntry
                )
            ]
        ]



-- MAIN


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
