port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser
import Browser.Navigation
import Json.Decode.Pipeline as Pipeline
import Json.Decode as Decode
import Http
import Http.Progress
import Time
import Regex
import Url
import Char


main : Program Flags Model Msg
main =
    Browser.fullscreen
        { init = init
        , view = view
        , update = update
        , onNavigation = Just UpdateUrl
        , subscriptions = subscriptions
        }


type alias Flags =
    { apiKey : String
    , updatePeriod : Int
    }


type alias Model =
    { apiKey : String
    , updatePeriod : Int
    , currentRoute : Route
    , cityInput : String
    , stateInput : String
    , legalForm : Bool
    , weather : List Weather
    , weatherUrl : Maybe String
    , weatherLoading : Bool
    , forecast10day : List DailyForecast
    , currentTime : Time.Posix
    , lastUpdated : Time.Posix
    , httpError : Maybe Http.Error
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
    Decode.succeed ForecastResponse
        |> Pipeline.required "forecast" forecastDecoder


type alias Date =
    { weekday : String }


dateDecoder : Decode.Decoder Date
dateDecoder =
    Decode.succeed Date
        |> Pipeline.required "weekday" Decode.string


type alias Temperature =
    { fahrenheit : String
    }


temperatureDecoder : Decode.Decoder Temperature
temperatureDecoder =
    Decode.succeed Temperature
        |> Pipeline.required "fahrenheit" Decode.string


type alias Forecast =
    { simpleForecast : SimpleForecast
    }


forecastDecoder : Decode.Decoder Forecast
forecastDecoder =
    Decode.succeed Forecast
        |> Pipeline.required "simpleforecast" simpleForecastDecoder


type alias SimpleForecast =
    { forecastDay : List DailyForecast
    }


simpleForecastDecoder : Decode.Decoder SimpleForecast
simpleForecastDecoder =
    Decode.succeed SimpleForecast
        |> Pipeline.required "forecastday" dailyForecastListDecoder


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
    Decode.succeed DailyForecast
        |> Pipeline.required "date" dateDecoder
        |> Pipeline.required "high" temperatureDecoder
        |> Pipeline.required "low" temperatureDecoder
        |> Pipeline.required "conditions" Decode.string
        |> Pipeline.required "icon_url" Decode.string


init : Browser.Env Flags -> ( Model, Cmd Msg )
init env =
    ( { apiKey = env.flags.apiKey
      , updatePeriod = env.flags.updatePeriod * 1000
      , currentRoute = locationToRoute env.url
      , cityInput = ""
      , stateInput = ""
      , legalForm = False
      , weather = []
      , weatherUrl = Nothing
      , weatherLoading = False
      , forecast10day = []
      , currentTime = Time.millisToPosix 0
      , lastUpdated = Time.millisToPosix 0
      , httpError = Nothing
      }
    , requestLocations ""
    )


type Msg
    = SetCityInput String
    | SetStateInput String
    | AddNewLocation
    | DeleteLocation Location
    | ProcessResponse (Result Http.Error WeatherUndergroundResponse)
    | ProcessForecastResponse (Result Http.Error ForecastResponse)
    | Tick Time.Posix
    | UpdateWeather Time.Posix
    | ReceiveLocalStorage String
    | UpdateUrl Url.Url
    | GetIndexProgress (Http.Progress.Progress WeatherUndergroundResponse)


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
                ( { model
                    | cityInput = lowerText
                    , legalForm = legalForm lowerText model.stateInput
                  }
                , Cmd.none
                )

        SetStateInput text ->
            let
                lowerText =
                    String.toLower text
            in
                ( { model
                    | stateInput = lowerText
                    , legalForm = legalForm model.cityInput lowerText
                  }
                , Cmd.none
                )

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

                url =
                    "http://api.wunderground.com/api/"
                        ++ model.apiKey
                        ++ "/conditions/q/"
                        ++ model.stateInput
                        ++ "/"
                        ++ model.cityInput
                        ++ ".json"
            in
                ( { model
                    | weather = newWeather :: model.weather
                    , cityInput = ""
                    , stateInput = ""
                    , legalForm = False
                    , weatherUrl = Just url
                    , weatherLoading = True
                  }
                , saveLocation (locationString newWeather)
                )

        DeleteLocation location ->
            let
                newWeather =
                    List.filter (locationNotMatch location) model.weather

                locationStr =
                    location.city ++ "," ++ location.state
            in
                ( { model | weather = newWeather }, deleteLocation locationStr )

        ProcessResponse (Ok response) ->
            let
                newWeather =
                    updateWeather response.currentObservation model.weather
            in
                ( { model
                    | weather = newWeather
                    , httpError = Nothing
                  }
                , Cmd.none
                )

        ProcessResponse (Err error) ->
            let
                location =
                    processHttpError error

                newWeather =
                    List.filter (locationNotMatch location) model.weather

                locationStr =
                    location.city ++ "," ++ location.state
            in
                ( { model | weather = newWeather }
                , deleteLocation locationStr
                )

        ProcessForecastResponse (Ok response) ->
            ( { model | forecast10day = response.forecast.simpleForecast.forecastDay }, Cmd.none )

        ProcessForecastResponse (Err error) ->
            ( { model | httpError = Just error }, Cmd.none )

        UpdateWeather time ->
            ( { model | lastUpdated = time }
            , Cmd.batch
                (List.map
                    (\e -> (get model.apiKey e.location.city e.location.state))
                    model.weather
                )
            )

        Tick time ->
            let
                lastUpdated =
                    if model.lastUpdated == Time.millisToPosix 0 then
                        time
                    else
                        model.lastUpdated
            in
                ( { model
                    | currentTime = time
                    , lastUpdated = lastUpdated
                  }
                , Cmd.none
                )

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
                ( { model | weather = newWeather }, Cmd.batch (genCommands model.apiKey locations) )

        UpdateUrl url ->
            let
                route =
                    locationToRoute url

                cmd =
                    case route of
                        WeatherShowRoute place ->
                            let
                                cityState =
                                    String.split "-" place

                                city =
                                    case cityState of
                                        [ first, _ ] ->
                                            first

                                        _ ->
                                            "none"

                                state =
                                    case cityState of
                                        [ _, second ] ->
                                            second

                                        _ ->
                                            "none"
                            in
                                get10day model.apiKey city state

                        _ ->
                            Cmd.none
            in
                ( { model | currentRoute = route }, cmd )

        GetIndexProgress (Http.Progress.Done response) ->
            let
                newWeather =
                    updateWeather response.currentObservation model.weather
            in
                ( { model
                    | weather = newWeather
                    , weatherLoading = False
                  }
                , Cmd.none
                )

        GetIndexProgress (Http.Progress.Fail error) ->
            ( { model | weatherLoading = False }, Cmd.none )

        GetIndexProgress progress ->
            ( { model | weatherLoading = True }, Cmd.none )


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
                            cityRegex =
                                Regex.fromString ".json"

                            stateRegex =
                                Regex.fromString "%20"

                            city =
                                case cityRegex of
                                    Just regex ->
                                        Regex.replace regex (\_ -> "") cityString

                                    Nothing ->
                                        cityString

                            state =
                                case stateRegex of
                                    Just regex ->
                                        Regex.replace regex (\_ -> "") stateString

                                    Nothing ->
                                        stateString
                        in
                            Location city state

                    _ ->
                        Location "none" "none"

        _ ->
            Location "none" "none"


stringToLocation : String -> Location
stringToLocation locationStr =
    let
        cityState =
            String.split "," locationStr
    in
        case cityState of
            [ city, state ] ->
                Location city state

            _ ->
                Location "" ""


stringToTuple : String -> ( String, String )
stringToTuple locationStr =
    let
        cityState =
            String.split "," locationStr
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


view : Model -> Browser.Page Msg
view model =
    { title = "Elm Weather"
    , body =
        [ div [ class "container" ]
            [ header
            , contentArea model
            ]
        ]
    }


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


capitalize : String -> String
capitalize str =
    case String.uncons str of
        Nothing ->
            str

        Just ( firstLetter, rest ) ->
            let
                newFirstLetter =
                    Char.toUpper firstLetter
            in
                String.cons newFirstLetter rest


formatCityState : List String -> List String
formatCityState cityState =
    case cityState of
        [ city, state ] ->
            [ capitalize city, String.toUpper state ]

        _ ->
            [ "None", "None" ]


showContentArea : String -> Model -> Html Msg
showContentArea place model =
    let
        placeName =
            place
                |> String.split "-"
                |> formatCityState
                |> String.join ", "

        errorString =
            case model.httpError of
                Just _ ->
                    "Http Error - FIXME"

                Nothing ->
                    ""
    in
        div [ class "row" ]
            [ div [ class "col-12" ]
                [ h1
                    []
                    [ text ("10 Day Forecast for " ++ placeName) ]
                , forecast10dayTable model.forecast10day
                , p [] [ text errorString ]
                ]
            ]


forecast10dayTable : List DailyForecast -> Html Msg
forecast10dayTable forecast =
    table [ class "table table-striped" ]
        [ thead []
            [ th [] [ text "Day" ]
            , th [] [ text "Icon" ]
            , th [] [ text "Conditions" ]
            , th [] [ text "High" ]
            , th [] [ text "Low" ]
            ]
        , tbody []
            (List.map singleDayForecast forecast)
        ]


singleDayForecast : DailyForecast -> Html Msg
singleDayForecast day =
    tr []
        [ td [] [ text day.date.weekday ]
        , td [] [ conditionsImage day ]
        , td [] [ text day.conditions ]
        , td [] [ text day.highTemperature.fahrenheit ]
        , td [] [ text day.lowTemperature.fahrenheit ]
        ]


conditionsImage : DailyForecast -> Html Msg
conditionsImage day =
    img
        [ src day.iconUrl, style "height" "35px" ]
        []


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
    let
        errorString =
            case model.httpError of
                Just _ ->
                    "Http Error - FIXME"

                Nothing ->
                    ""
    in
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
            , div [] [ text errorString ]
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
            (Time.posixToMillis model.currentTime)
                - (Time.posixToMillis model.lastUpdated)
                |> Basics.min model.updatePeriod

        diff =
            toFloat (model.updatePeriod - timeSinceUpdate) / 1000 |> Basics.round

        disp =
            if diff > 0 then
                Just (String.fromInt diff)
            else
                Nothing
    in
        div [ class "col-9" ]
            [ displayNextUpdateTime disp
            , weatherTable model
            , loadingIndicator model
            ]


loadingIndicator : Model -> Html Msg
loadingIndicator model =
    case model.weatherLoading of
        True ->
            img
                [ src "loading.gif"
                , style "position" "absolute"
                , style "top" "50%"
                , style "left" "50%"
                ]
                []

        _ ->
            div [] []


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
            , td [] [ text (weather.temperature |> String.fromFloat) ]
            , td [] [ text weather.conditions ]
            , td [] [ text (weather.windSpeed |> String.fromFloat) ]
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
    Decode.succeed Location
        |> Pipeline.required "city" Decode.string
        |> Pipeline.required "state" Decode.string


type alias WeatherUndergroundResponse =
    { currentObservation : Weather }


weatherUndergroundResponseDecoder : Decode.Decoder WeatherUndergroundResponse
weatherUndergroundResponseDecoder =
    Decode.succeed WeatherUndergroundResponse
        |> Pipeline.required "current_observation" weatherDecoder


weatherDecoder : Decode.Decoder Weather
weatherDecoder =
    Decode.succeed Weather
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

        request =
            Http.get url weatherUndergroundResponseDecoder
    in
        Http.send ProcessResponse request


get10day : String -> String -> String -> Cmd Msg
get10day apiKey city state =
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


locationToRoute : Url.Url -> Route
locationToRoute url =
    let
        routePaths =
            url.fragment
                |> Maybe.withDefault ""
                |> String.split "/"
                |> List.filter (\elem -> elem /= "")
    in
        case routePaths of
            [] ->
                WeatherIndexRoute

            [ "weather" ] ->
                WeatherIndexRoute

            [ "weather", place ] ->
                WeatherShowRoute place

            _ ->
                NotFoundRoute


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        weatherRequest =
            case model.weatherUrl of
                Just url ->
                    Http.get url weatherUndergroundResponseDecoder
                        |> Http.Progress.track url GetIndexProgress

                Nothing ->
                    Sub.none
    in
        Sub.batch
            [ Time.every 1000 Tick
            , Time.every (toFloat model.updatePeriod) UpdateWeather
            , receiveLocations ReceiveLocalStorage
            , weatherRequest
            ]
