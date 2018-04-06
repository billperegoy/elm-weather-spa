module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { weather : List WeatherEntry
    }


type alias Location =
    { city : String
    , state : String
    }


type alias Weather =
    { temperature : Float
    , conditions : String
    , windSpeed : Float
    , windGust : Float
    }


type alias WeatherEntry =
    ( Location, Weather )


init : ( Model, Cmd Msg )
init =
    { weather =
        [ ( Location "Boston" "MA"
          , { temperature = 43.0
            , conditions = "rain"
            , windSpeed = 12.1
            , windGust = 17.7
            }
          )
        , ( Location "Takoma" "WA"
          , { temperature = 57.2
            , conditions = "partly cloudy"
            , windSpeed = 3.2
            , windGust = 4.0
            }
          )
        ]
    }
        ! []


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []


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
        [ Html.form []
            [ div [ class "form-group" ]
                [ label [ for "cityInput" ]
                    [ text "City " ]
                , input [ class "form-control", id "cityInput", placeholder "Enter city" ]
                    []
                ]
            , div [ class "form-group" ]
                [ label [ for "stateInput" ]
                    [ text "State" ]
                , input [ class "form-control", id "stateInput", placeholder "State" ]
                    []
                ]
            , button [ class "btn btn-primary", type_ "submit" ]
                [ text "Submit" ]
            ]
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
    tr []
        [ td [] [ text <| locationString weather ]
        , td [] [ text ((Tuple.second weather).temperature |> toString) ]
        , td [] [ text ((Tuple.second weather).conditions) ]
        , td [] [ text ((Tuple.second weather).windSpeed |> toString) ]
        , td [] [ text ((Tuple.second weather).windGust |> toString) ]
        ]


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
