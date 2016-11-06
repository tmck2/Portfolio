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
import Color
import Random


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


rgb : Random.Generator Color.Color
rgb =
    Random.map3 Color.rgb (Random.int 50 100) (Random.int 0 100) (Random.int 50 100)



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


viewLink link =
    Maybe.withDefault notVisible <|
        Maybe.map
            (\url ->
                a [ href url, Attr.class "btn btn-primary", Attr.target "_blank" ]
                    [ span [ Attr.class "glyphicon glyphicon-link" ] []
                    , text "Link"
                    ]
            )
            link


joinList : String -> List String -> String
joinList sep lst =
    String.dropLeft (String.length sep) <|
        List.foldl (\str acc -> acc ++ sep ++ str) "" lst


filterEntries ( archFilter, techFilter ) entries =
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


viewFollowButton =
    a
        [ Attr.href "https://twitter.com/tony_mckinney"
        , Attr.class "twitter-follow-button"
        , Attr.attribute "data-show-count" "false"
        ]
        [ text "Follow @tony_mckinney" ]


viewAddress =
    List.map (\str -> li [ Attr.class "address" ] [ text str ]) [ "2303 Meadow Village Dr.", "Columbus, OH", "43235", "606-939-2503" ]


viewContactInfo =
    ul [ Attr.id "contact-info" ] <| li [] [ viewFollowButton ] :: viewAddress


viewImage { red, green, blue } title image =
    Maybe.withDefault
        (div
            [ Attr.class "media-object thumbnail"
            , style
                [ ( "color", "#fff" )
                , ( "background-color", "rgb(" ++ joinList "," (List.map toString [ red, green, blue ]) ++ ")" )
                ]
            ]
            [ div [ Attr.style [ ( "font-size", "140px" ) ] ]
                [ text (String.left 1 title) ]
            ]
        )
    <|
        Maybe.map (\url -> img [ src url, Attr.class "media-object thumbnail", style [ ( "width", "140px" ) ] ] []) image


viewEntry color entry =
    div [ Attr.class "media" ]
        [ div [ Attr.class "media-left" ]
            [ viewImage (Color.toRgb color) entry.title entry.picture ]
        , div [ Attr.class "media-body" ]
            [ h4 [ Attr.class "media-heading" ]
                [ text entry.title ]
            , div []
                [ text <| "Architecture: " ++ joinList ", " entry.architecture ]
            , div []
                [ text <| "Technologies: " ++ joinList ", " entry.technologies ]
            , div [] [ viewLink entry.link ]
            , div []
                [ h3 [] [ text "Description:" ] ]
            , text entry.description
            ]
        ]


viewEntries seed entries =
    case entries of
        hd :: tl ->
            let
                ( nextColor, seed ) =
                    Random.step rgb seed
            in
                viewEntry nextColor hd :: viewEntries seed tl

        [] ->
            []


view model =
    div []
        [ div [ style [ ( "float", "right" ) ] ]
            [ div [] [ viewContactInfo ] ]
        , div [ Attr.id "title-container" ]
            [ h1 [] [ text "Anthony McKinney" ]
            , h2 [] [ text "Portfolio" ]
            ]
        , div [ Attr.class "row" ]
            [ div [ Attr.class "col-md-2" ] [ viewFilters model ]
            , div [ Attr.id "portfolio-container", Attr.class "col-md-10" ]
                (viewEntries (Random.initialSeed 123) <| filterEntries model.filter model.portfolioEntries)
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
