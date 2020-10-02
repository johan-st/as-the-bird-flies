module Main exposing (..)

import Browser
import Data exposing (toDisplayList)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Geodesy exposing (Coordinates, Unit(..), distance)
import Html exposing (Html)
import Http



---- MODEL ----


type Status
    = Init
    | Loading
    | Succeded
    | Failed


type alias Model =
    { routes : List AirRoute
    , airports : List Airport
    , state : Status
    , toDisplay : List AirRoute
    }


type alias Airport =
    { id : String
    , name : String
    , city : String
    , location : Coordinates
    }


type alias AirRoute =
    { airlineId : String
    , flightNr : String
    , origin : Airport
    , destinatin : Airport
    }


init : ( Model, Cmd Msg )
init =
    ( { routes = [], airports = [], state = Init, toDisplay = toDisplayList }, getAirports )



---- UPDATE ----


type Msg
    = GotAirports (Result Http.Error String)
    | GotRoutes (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAirports result ->
            case result of
                Ok data ->
                    ( model, Cmd.none )

                Err err ->
                    let
                        debug =
                            Debug.log "getting airports failed" err
                    in
                    ( model, Cmd.none )

        GotRoutes result ->
            case result of
                Ok data ->
                    ( model, Cmd.none )

                Err err ->
                    let
                        debug =
                            Debug.log "getting air routes failed" err
                    in
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout [ BG.color (rgb 0.1 0.1 0.1), Font.color (rgb 0.8 0.2 0.2) ]
        (column [ width fill ]
            [ statusRow model
            , resultTable model
            ]
        )


resultTable : Model -> Element Msg
resultTable model =
    column [ spaceEvenly, centerX ] <| List.map routeView model.toDisplay


statusRow : Model -> Element Msg
statusRow model =
    row [ paddingXY 200 30, spacing 50 ]
        [ el [] <| text <| "routes: " ++ length model.routes
        , el [] <| text <| "airports: " ++ length model.airports
        ]


routeView : AirRoute -> Element Msg
routeView ar =
    row [ spacing 100 ]
        [ el [] <| text <| "id: " ++ ar.airlineId
        , el [] <| text <| "flightNr: " ++ ar.flightNr
        , el [] <| text <| "from: " ++ airportToString ar.origin
        , el [] <| text <| "to: " ++ airportToString ar.destinatin
        , el [] <| text <| "distance: " ++ getDistance ar.origin.location ar.destinatin.location ++ "km"
        ]


length : List a -> String
length list =
    list
        |> List.length
        |> String.fromInt


getDistance : Coordinates -> Coordinates -> String
getDistance loc1 loc2 =
    distance loc1 loc2 Kilometers
        |> round
        |> String.fromInt


calcDist : Coordinates -> Coordinates -> Float
calcDist loc1 loc2 =
    49.49


airportToString : Airport -> String
airportToString ap =
    ap.name


locToString : Coordinates -> String
locToString loc =
    String.fromFloat
        (Tuple.first loc)
        ++ String.fromFloat (Tuple.second loc)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



-- DECODE
-- COMMANDS


getAirports : Cmd Msg
getAirports =
    Http.get
        { url = "https://elm-lang.org/assets/public-opissnion.txt"
        , expect = Http.expectString GotAirports
        }
