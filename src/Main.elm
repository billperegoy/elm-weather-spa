port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation
import Json.Decode.Pipeline as Pipeline
import Json.Decode as Decode
import Http
import Time
import Regex


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UpdateUrl
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { apiKey : String }


type alias Model =
    { apiKey : String
    , currentRoute : Route
    , cityInput : String
    , stateInput : String
    , legalForm : Bool
    , weather : List Weather
    , currentTime : Time.Time
    , lastUpdated : Time.Time
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


type alias ForecastResponse =
    { forecast : Forecast
    }


forecastResponseDecoder : Decode.Decoder ForecastResponse
forecastResponseDecoder =
    Pipeline.decode ForecastResponse
        |> Pipeline.required "forecast" forecastDecoder


type alias Date =
    { weekday : String }


dateDecoder : Decode.Decoder Date
dateDecoder =
    Pipeline.decode Date
        |> Pipeline.required "weekday" Decode.string


type alias Temperature =
    { fahrenheit : String
    }


temperatureDecoder : Decode.Decoder Temperature
temperatureDecoder =
    Pipeline.decode Temperature
        |> Pipeline.required "fahrenheit" Decode.string


type alias Forecast =
    { simpleForecast : SimpleForecast
    }


forecastDecoder : Decode.Decoder Forecast
forecastDecoder =
    Pipeline.decode Forecast
        |> Pipeline.required "simpleforecast" simpleForecastDecoder


type alias SimpleForecast =
    { forecastDay : List DailyForecast
    }


simpleForecastDecoder : Decode.Decoder SimpleForecast
simpleForecastDecoder =
    Pipeline.decode SimpleForecast
        |> Pipeline.required "simpleforecast" dailyForecastListDecoder


dailyForecastListDecoder : Decode.Decoder (List DailyForecast)
dailyForecastListDecoder =
    Decode.list dailyForecastDecoder


type alias DailyForecast =
    { date : Date
    , highTemperature : Temperature
    , lowTemperature : Temperature
    , conditions : String
    , iconUrl : String
    }


dailyForecastDecoder : Decode.Decoder DailyForecast
dailyForecastDecoder =
    Pipeline.decode DailyForecast
        |> Pipeline.required "date" dateDecoder
        |> Pipeline.required "high" temperatureDecoder
        |> Pipeline.required "low" temperatureDecoder
        |> Pipeline.required "conditions" Decode.string
        |> Pipeline.required "iconUrl" Decode.string


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags location =
    { apiKey = flags.apiKey
    , currentRoute = WeatherIndexRoute
    , cityInput = ""
    , stateInput = ""
    , legalForm = False
    , weather = []
    , currentTime = 0
    , lastUpdated = 0
    , httpError = ""
    }
        ! [ requestLocations "" ]


type Msg
    = SetCityInput String
    | SetStateInput String
    | AddNewLocation
    | DeleteLocation Location
    | ProcessResponse (Result Http.Error WeatherUndergroundResponse)
    | ProcessForecastResponse (Result Http.Error ForecastResponse)
    | Tick Time.Time
    | UpdateWeather Time.Time
    | ReceiveLocalStorage String
    | UpdateUrl Navigation.Location


type Route
    = WeatherIndexRoute
    | WeatherShowRoute String
    | NotFoundRoute


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetCityInput text ->
            let
                lowerText =
                    String.toLower text
            in
                { model
                    | cityInput = lowerText
                    , legalForm = legalForm lowerText model.stateInput
                }
                    ! []

        SetStateInput text ->
            let
                lowerText =
                    String.toLower text
            in
                { model
                    | stateInput = lowerText
                    , legalForm = legalForm model.cityInput lowerText
                }
                    ! []

        AddNewLocation ->
            let
                location =
                    Location model.cityInput model.stateInput

                newWeather =
                    { location = location
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
                    ! [ get model.apiKey model.cityInput model.stateInput
                      , saveLocation (locationString newWeather)
                      ]

        DeleteLocation location ->
            let
                newWeather =
                    List.filter (locationNotMatch location) model.weather

                locationString =
                    location.city ++ "," ++ location.state
            in
                { model | weather = newWeather } ! [ deleteLocation locationString ]

        ProcessResponse (Ok response) ->
            let
                newWeather =
                    updateWeather response.currentObservation model.weather
            in
                { model
                    | weather = newWeather
                    , httpError = ""
                }
                    ! []

        ProcessResponse (Err error) ->
            let
                location =
                    processHttpError error

                newWeather =
                    List.filter (locationNotMatch location) model.weather

                locationString =
                    location.city ++ "," ++ location.state
            in
                { model | weather = newWeather }
                    ! [ deleteLocation locationString ]

        ProcessForecastResponse (Ok response) ->
            { model | httpError = "Success!" } ! []

        ProcessForecastResponse (Err error) ->
            { model | httpError = toString error } ! []

        UpdateWeather time ->
            { model | lastUpdated = time }
                ! List.map
                    (\e -> (get model.apiKey e.location.city e.location.state))
                    model.weather

        Tick time ->
            let
                lastUpdated =
                    if model.lastUpdated == 0 then
                        time
                    else
                        model.lastUpdated
            in
                { model
                    | currentTime = time
                    , lastUpdated = lastUpdated
                }
                    ! []

        ReceiveLocalStorage string ->
            let
                locations =
                    if (String.length string) == 0 then
                        []
                    else
                        String.split ":" string

                newWeather =
                    List.map
                        (\e ->
                            { location = stringToLocation e
                            , temperature = 0
                            , conditions = ""
                            , windSpeed = 0
                            }
                        )
                        locations
            in
                { model | weather = newWeather } ! genCommands model.apiKey locations

        UpdateUrl location ->
            let
                route =
                    locationToRoute location

                cmd =
                    case route of
                        WeatherShowRoute place ->
                            [ get2 model.apiKey "boston" "ma" ]

                        _ ->
                            []
            in
                { model | currentRoute = route } ! cmd


processHttpError : Http.Error -> Location
processHttpError error =
    case error of
        Http.BadPayload string response ->
            let
                strings =
                    String.split "/" response.url
                        |> List.reverse
                        |> List.take 2
            in
                case strings of
                    [ cityString, stateString ] ->
                        let
                            city =
                                Regex.replace Regex.All (Regex.regex ".json") (\_ -> "") cityString

                            state =
                                Regex.replace Regex.All (Regex.regex "%20") (\_ -> "") stateString
                        in
                            Location city state

                    _ ->
                        Location "none" "none"

        _ ->
            Location "none" "none"


stringToLocation : String -> Location
stringToLocation locationString =
    let
        cityState =
            String.split "," locationString
    in
        case cityState of
            [ city, state ] ->
                Location city state

            _ ->
                Location "" ""


stringToTuple locationString =
    let
        cityState =
            String.split "," locationString
    in
        case cityState of
            [ city, state ] ->
                ( city, state )

            _ ->
                ( "", "" )


genCommands : String -> List String -> List (Cmd Msg)
genCommands apiKey list =
    List.map
        (\e ->
            let
                location =
                    stringToTuple e
            in
                get apiKey (Tuple.first location |> String.trim) (Tuple.second location |> String.trim)
        )
        list


updateWeather : Weather -> List Weather -> List Weather
updateWeather newWeather entries =
    List.map (conditionallyReplaceWeather newWeather) entries


conditionallyReplaceWeather : Weather -> Weather -> Weather
conditionallyReplaceWeather newWeather weather =
    if locationsEqual newWeather weather then
        lowercaseWeather newWeather
    else
        lowercaseWeather weather


lowercaseWeather : Weather -> Weather
lowercaseWeather weather =
    let
        location =
            weather.location

        newLocation =
            { location
                | city = String.toLower location.city |> String.trim
                , state = String.toLower location.state |> String.trim
            }
    in
        { weather | location = newLocation }


locationsEqual : Weather -> Weather -> Bool
locationsEqual w1 w2 =
    ((String.toLower (w1.location.city) |> String.trim) == (String.toLower (w2.location.city |> String.trim)))
        && ((String.toLower (w1.location.state) |> String.trim) == (String.toLower (w2.location.state |> String.trim)))


locationNotMatch : Location -> Weather -> Bool
locationNotMatch location weather =
    let
        entryLocation =
            weather.location

        entryCity =
            entryLocation.city

        entryState =
            entryLocation.state
    in
        (String.toLower (entryCity) /= String.toLower (location.city)) || (String.toLower (entryState) /= String.toLower (location.state))


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ header
        , contentArea model
        ]


contentArea : Model -> Html Msg
contentArea model =
    case model.currentRoute of
        WeatherIndexRoute ->
            indexContentArea model

        WeatherShowRoute place ->
            showContentArea place model

        NotFoundRoute ->
            notFoundContentArea model


indexContentArea : Model -> Html Msg
indexContentArea model =
    div [ class "row" ]
        [ sidebar model
        , resultsPane model
        ]


showContentArea : String -> Model -> Html Msg
showContentArea place model =
    div [ class "row" ]
        [ h1
            []
            [ text ("10 Day Forecast for " ++ place) ]
        ]


notFoundContentArea : Model -> Html Msg
notFoundContentArea model =
    div [ class "row" ]
        [ h1
            []
            [ text "Page Not Found" ]
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
        , disabled False
        , onClick AddNewLocation
        ]
    else
        [ class "btn btn-primary"
        , disabled True
        ]


resultsPane : Model -> Html Msg
resultsPane model =
    let
        timeSinceUpdate =
            (model.currentTime - model.lastUpdated)
                |> Time.inSeconds
                |> round
                |> Basics.min 60

        x =
            30 - timeSinceUpdate

        disp =
            if x > 0 then
                Just (toString x)
            else
                Nothing
    in
        div [ class "col-9" ]
            [ displayNextUpdateTime disp
            , weatherTable model
            ]


notFoundPane : Html Msg
notFoundPane =
    div [ class "col-9" ]
        [ h1 [] [ text "Page Not Found" ] ]


weatherTable : Model -> Html Msg
weatherTable model =
    table
        [ class "table table-striped" ]
        [ tableHeader
        , tbody [] (sortedWeatherEntries model.weather)
        ]


displayNextUpdateTime : Maybe String -> Html Msg
displayNextUpdateTime time =
    case time of
        Nothing ->
            div [] []

        Just t ->
            div [ class "alert alert-info" ]
                [ text ("Next update: " ++ t ++ " seconds")
                ]


sortedWeatherEntries : List Weather -> List (Html Msg)
sortedWeatherEntries entries =
    entries
        |> List.filter (\entry -> entry.conditions /= "")
        |> List.sortBy locationString
        |> List.map weatherEntry


tableHeader : Html Msg
tableHeader =
    thead []
        [ tr
            []
            [ th [] [ text "Location" ]
            , th [] [ text "Temperature" ]
            , th [] [ text "Conditions" ]
            , th [] [ text "Wind" ]
            ]
        ]


weatherEntry : Weather -> Html Msg
weatherEntry weather =
    let
        link =
            "#/weather/" ++ weather.location.city ++ "-" ++ weather.location.state
    in
        tr []
            [ td [] [ a [ href link ] [ text <| locationString weather ] ]
            , td [] [ text (weather.temperature |> toString) ]
            , td [] [ text weather.conditions ]
            , td [] [ text (weather.windSpeed |> toString) ]
            , td [ onClick (DeleteLocation weather.location) ] [ span [ class "oi oi-circle-x" ] [] ]
            ]


locationString : Weather -> String
locationString weather =
    weather.location.city ++ ", " ++ weather.location.state


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


get : String -> String -> String -> Cmd Msg
get apiKey city state =
    let
        url =
            "http://api.wunderground.com/api/"
                ++ apiKey
                ++ "/conditions/q/"
                ++ state
                ++ "/"
                ++ city
                ++ ".json"
    in
        Http.send ProcessResponse (Http.get url weatherUndergroundResponseDecoder)


get2 : String -> String -> String -> Cmd Msg
get2 apiKey city state =
    let
        url =
            "http://api.wunderground.com/api/"
                ++ apiKey
                ++ "/forecast10day/q/"
                ++ state
                ++ "/"
                ++ city
                ++ ".json"
    in
        Http.send ProcessForecastResponse (Http.get url forecastResponseDecoder)


port saveLocation : String -> Cmd msg


port deleteLocation : String -> Cmd msg


port requestLocations : String -> Cmd msg


port receiveLocations : (String -> msg) -> Sub msg


locationToRoute : Navigation.Location -> Route
locationToRoute location =
    let
        routePaths =
            location.hash
                |> String.split "/"
                |> List.drop 1

        x =
            Debug.log "x" routePaths
    in
        case routePaths of
            [] ->
                WeatherIndexRoute

            [ "weather" ] ->
                WeatherIndexRoute

            [ "weather", "" ] ->
                WeatherIndexRoute

            [ "weather", place ] ->
                WeatherShowRoute place

            [ "weather", place, "" ] ->
                WeatherShowRoute place

            _ ->
                NotFoundRoute


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second Tick
        , Time.every (Time.second * 30) UpdateWeather
        , receiveLocations ReceiveLocalStorage
        ]
