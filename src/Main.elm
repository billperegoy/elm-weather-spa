module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode.Pipeline as Pipeline
import Json.Decode as Decode
import Http


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { cityInput : String
    , stateInput : String
    , legalForm : Bool
    , weather : List WeatherEntry
    }


type alias Location =
    { city : String
    , state : String
    }


type alias WeatherUndergroundResponse =
    { currentObservation : Weather }


type alias Weather =
    { temperature : Float
    , conditions : String
    , windSpeed : Float
    , windGust : Float
    }


type alias WeatherEntry =
    ( Location, Maybe Weather )


init : ( Model, Cmd Msg )
init =
    { cityInput = ""
    , stateInput = ""
    , legalForm = False
    , weather =
        [ ( Location "Boston" "MA"
          , Just
                { temperature = 43.0
                , conditions = "rain"
                , windSpeed = 12.1
                , windGust = 17.7
                }
          )
        , ( Location "Takoma" "WA"
          , Just
                { temperature = 57.2
                , conditions = "partly cloudy"
                , windSpeed = 3.2
                , windGust = 4.0
                }
          )
        ]
    }
        ! []


type Msg
    = SetCityInput String
    | SetStateInput String
    | AddNewLocation
    | DeleteLocation Location
    | ProcessResponse (Result Http.Error WeatherUndergroundResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetCityInput text ->
            { model
                | cityInput = text
                , legalForm = legalForm text model.stateInput
            }
                ! []

        SetStateInput text ->
            { model
                | stateInput = text
                , legalForm = legalForm model.cityInput text
            }
                ! []

        AddNewLocation ->
            let
                newLocation =
                    Location model.cityInput model.stateInput

                newWeather =
                    ( newLocation, Nothing ) :: model.weather
            in
                { model
                    | weather = newWeather
                    , cityInput = ""
                    , stateInput = ""
                    , legalForm = False
                }
                    ! [ get model.cityInput model.stateInput ]

        DeleteLocation location ->
            let
                newWeather =
                    List.filter (locationMatch location) model.weather
            in
                { model | weather = newWeather } ! []

        ProcessResponse (Ok response) ->
            model ! []

        ProcessResponse (Err error) ->
            model ! []


locationMatch : Location -> WeatherEntry -> Bool
locationMatch location weatherEntry =
    let
        entryLocation =
            Tuple.first weatherEntry

        entryCity =
            entryLocation.city

        entryState =
            entryLocation.state
    in
        (entryCity /= location.city) || (entryState /= location.state)


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ header
        , contentArea model
        ]


contentArea : Model -> Html Msg
contentArea model =
    div [ class "row" ]
        [ sidebar model
        , resultsPane model
        ]


header : Html Msg
header =
    div
        [ class "jumbotron" ]
        [ h1
            [ class "text-center" ]
            [ text "Elm Weather Report" ]
        ]


sidebar : Model -> Html Msg
sidebar model =
    div [ class "col-3" ]
        [ div [ class "form-group" ]
            [ label [ for "cityInput" ]
                [ text "City " ]
            , input
                [ class "form-control"
                , id "cityInput"
                , placeholder "Enter city"
                , onInput SetCityInput
                , value model.cityInput
                ]
                []
            ]
        , div [ class "form-group" ]
            [ label [ for "stateInput" ]
                [ text "State" ]
            , input
                [ class "form-control"
                , id "stateInput"
                , placeholder "State"
                , onInput SetStateInput
                , value model.stateInput
                ]
                []
            ]
        , button (buttonAttributes model.legalForm)
            [ text "Submit" ]
        ]


buttonAttributes : Bool -> List (Html.Attribute Msg)
buttonAttributes validated =
    if validated then
        [ class "btn btn-primary"
        , onClick AddNewLocation
        ]
    else
        [ class "btn btn-primary"
        , disabled True
        ]


resultsPane : Model -> Html Msg
resultsPane model =
    div [ class "col-9" ]
        [ table
            [ class "table" ]
            (List.map weatherEntry model.weather)
        ]


weatherEntry : WeatherEntry -> Html Msg
weatherEntry weather =
    let
        location =
            Tuple.first weather

        conditions =
            Tuple.second weather
    in
        tr []
            [ td [] [ text <| locationString weather ]
            , td [] [ text (conditionsField .temperature conditions) ]
            , td [] [ text (conditionsField .conditions conditions) ]
            , td [] [ text (conditionsField .windSpeed conditions) ]
            , td [] [ text (conditionsField .windGust conditions) ]
            , td [ onClick (DeleteLocation location) ] [ span [ class "oi oi-circle-x" ] [] ]
            ]


conditionsField extractor conditions =
    case conditions of
        Nothing ->
            ""

        Just c ->
            c |> extractor |> toString


locationString : WeatherEntry -> String
locationString weather =
    let
        location =
            Tuple.first weather
    in
        location.city ++ ", " ++ location.state


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


legalForm : String -> String -> Bool
legalForm city state =
    (String.length city > 2) && (String.length state == 2)


weatherUndergroundDecoder : Decode.Decoder WeatherUndergroundResponse
weatherUndergroundDecoder =
    Pipeline.decode WeatherUndergroundResponse
        |> Pipeline.required "current_observation" weatherDecoder


weatherDecoder : Decode.Decoder Weather
weatherDecoder =
    Pipeline.decode Weather
        |> Pipeline.required "temp_f" Decode.float
        |> Pipeline.required "wether" Decode.string
        |> Pipeline.required "wind_mph" Decode.float
        |> Pipeline.required "wind_gust_mph" Decode.float


get : String -> String -> Cmd Msg
get city state =
    let
        url =
            "http://api.wunderground.com/api/c83a6598d579714d/conditions/q/" ++ state ++ "/San_Francisco.json"
    in
        Http.send ProcessResponse (Http.get url weatherUndergroundDecoder)
