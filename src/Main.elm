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
    , weather : List Weather
    , httpError : String
    }


type alias Location =
    { city : String
    , state : String
    }


type alias Weather =
    { location : Location
    , temperature : Float
    , conditions : String
    , windSpeed : Float
    }


init : ( Model, Cmd Msg )
init =
    { cityInput = ""
    , stateInput = ""
    , legalForm = False
    , weather =
        [ { location = Location "Boston" "MA"
          , temperature = 43.0
          , conditions = "rain"
          , windSpeed = 12.1
          }
        , { location = Location "Takoma" "WA"
          , temperature = 57.2
          , conditions = "partly cloudy"
          , windSpeed = 3.2
          }
        ]
    , httpError = ""
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
                    { location = newLocation
                    , temperature = 0
                    , conditions = ""
                    , windSpeed = 0
                    }
            in
                { model
                    | weather = newWeather :: model.weather
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
            let
                city =
                    response.currentObservation.location.city

                state =
                    response.currentObservation.location.state

                newWeather =
                    updateForLocation city state model.weather
            in
                { model | weather = newWeather } ! []

        ProcessResponse (Err error) ->
            { model | httpError = toString error } ! []


updateForLocation : String -> String -> List Weather -> List Weather
updateForLocation city state entries =
    let
        location =
            Location city state
    in
        List.map
            (\e ->
                (if (locationMatch location e) then
                    e
                 else
                    e
                )
            )
            entries


locationMatch : Location -> Weather -> Bool
locationMatch location weatherEntry =
    let
        entryLocation =
            weatherEntry.location

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
        , div [] [ text model.httpError ]
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


weatherEntry : Weather -> Html Msg
weatherEntry weather =
    tr []
        [ td [] [ text <| locationString weather ]
        , td [] [ text (weather.temperature |> toString) ]
        , td [] [ text weather.conditions ]
        , td [] [ text (weather.windSpeed |> toString) ]
        , td [ onClick (DeleteLocation weather.location) ] [ span [ class "oi oi-circle-x" ] [] ]
        ]


locationString : Weather -> String
locationString weather =
    weather.location.city ++ ", " ++ weather.location.state


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


legalForm : String -> String -> Bool
legalForm city state =
    (String.length city > 2) && (String.length state == 2)


locationDecoder : Decode.Decoder Location
locationDecoder =
    Pipeline.decode Location
        |> Pipeline.required "city" Decode.string
        |> Pipeline.required "state" Decode.string


type alias WeatherUndergroundResponse =
    { currentObservation : Weather }


weatherUndergroundResponseDecoder : Decode.Decoder WeatherUndergroundResponse
weatherUndergroundResponseDecoder =
    Pipeline.decode WeatherUndergroundResponse
        |> Pipeline.required "current_observation" weatherDecoder


weatherDecoder : Decode.Decoder Weather
weatherDecoder =
    Pipeline.decode Weather
        |> Pipeline.required "display_location" locationDecoder
        |> Pipeline.required "temp_f" Decode.float
        |> Pipeline.required "weather" Decode.string
        |> Pipeline.required "wind_mph" Decode.float


get : String -> String -> Cmd Msg
get city state =
    let
        url =
            "http://api.wunderground.com/api/c83a6598d579714d/conditions/q/" ++ state ++ "/" ++ city ++ ".json"
    in
        Http.send ProcessResponse (Http.get url weatherUndergroundResponseDecoder)
