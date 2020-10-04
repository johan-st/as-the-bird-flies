module Main exposing (..)

import Array exposing (Array)
import Browser
import Csv exposing (Csv)
import Data exposing (toDisplayList)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Geodesy exposing (Coordinates, Unit(..), distance)
import Html exposing (Html)
import Http
import Task exposing (Task)



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
    , airportData : List (List String)
    }


type alias Airport =
    { id : Int
    , name : String
    , city : String
    , location : Coordinates
    }


portToString : Airport -> List String
portToString ap =
    [ ap.name
    , ap.city
    ]


type alias AirRoute =
    { airlineId : Int
    , airline : String
    , origin : Airport
    , destinatin : Airport
    , distance : Int
    }


type alias AirRouteIntermediate =
    { airlineId : Int
    , airline : String
    , originId : Int
    , destinatinId : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { routes = [], airports = [], state = Init, toDisplay = toDisplayList, airportData = [] }, getAirports )



---- UPDATE ----


type Msg
    = NoOp
    | GotAirports (Result Http.Error String)
    | GotRoutes (Result Http.Error String)
    | OneAirportParsed (List (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAirports result ->
            case result of
                Ok data ->
                    let
                        splitData =
                            data |> Csv.split
                    in
                    ( { model | airportData = splitData }, Task.perform parseOnePort (Task.succeed model.airportData) )

                Err err ->
                    let
                        debug =
                            Debug.log "getting airports failed" err
                    in
                    ( model, Cmd.none )

        OneAirportParsed airportData ->
            case airportData of
                head :: tail ->
                    ( { model | airports = buildAirport head :: model.airports, airportData = tail }, Task.perform parseOnePort (Task.succeed model.airportData) )

                [] ->
                    ( model, getAirRoutes )

        GotRoutes result ->
            case result of
                Ok data ->
                    let
                        routes =
                            dataToAirRoutes model.airports data
                    in
                    ( { model | routes = routes }, Cmd.none )

                Err err ->
                    let
                        debug =
                            Debug.log "getting air routes failed" err
                    in
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


parseOnePort : List (List String) -> Msg
parseOnePort data =
    let
        d =
            Debug.log "Parse One Airport" data
    in
    Debug.todo "parseOnePort"



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
    column [ centerX ] <| List.map routeView model.toDisplay


statusRow : Model -> Element Msg
statusRow model =
    row [ paddingXY 200 30, spacing 50 ]
        [ el [] <| text <| "routes: " ++ length model.routes
        , el [] <| text <| "airports: " ++ length model.airports
        ]


debugView : List (List String) -> Element Msg
debugView list =
    column [ Font.size 10, Font.color (rgb 0.5 0.5 0.5), centerX, spacing 3 ] <| [ text <| Debug.toString <| List.map (\item -> List.concat item) ]


routeView : AirRoute -> Element Msg
routeView ar =
    row [ spaceEvenly ]
        [ el [ paddingXY 10 3 ] <| text <| "id: " ++ String.fromInt ar.airlineId
        , el [ paddingXY 10 3 ] <| text <| "flightNr: " ++ ar.airline
        , el [ paddingXY 10 3 ] <| text <| "from: " ++ airportToString ar.origin
        , el [ paddingXY 10 3 ] <| text <| "to: " ++ airportToString ar.destinatin
        , el [ paddingXY 10 3 ] <| text <| "distance: " ++ getDistance ar.origin.location ar.destinatin.location ++ "km"
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


dataToAirports : String -> List Airport
dataToAirports data =
    data
        |> Csv.split
        |> List.map buildAirport


buildAirport : List String -> Airport
buildAirport list =
    let
        a =
            Array.fromList list
    in
    { id = Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 0 a)))
    , name = Maybe.withDefault "Not Found" (Array.get 1 a)
    , city = Maybe.withDefault "Not Found" (Array.get 2 a)
    , location =
        ( Maybe.withDefault 0 (String.toFloat (Maybe.withDefault "0" (Array.get 6 a)))
        , Maybe.withDefault 0 (String.toFloat (Maybe.withDefault "0" (Array.get 7 a)))
        )
    }


dataToAirRoutes : List Airport -> String -> List AirRoute
dataToAirRoutes ports data =
    data
        |> Csv.split
        |> List.map buildAirRoute
        |> List.map (assembleRoute ports)


buildAirRoute : List String -> AirRouteIntermediate
buildAirRoute list =
    let
        a =
            Array.fromList list
    in
    { airlineId = Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 0 a)))
    , airline = Maybe.withDefault "Not Found" (Array.get 0 a)
    , originId = Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 3 a)))
    , destinatinId = Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 5 a)))
    }


assembleRoute : List Airport -> AirRouteIntermediate -> AirRoute
assembleRoute ports route =
    let
        origin =
            List.filter (airportHasThisId route.originId) ports
                |> List.head
                |> Maybe.withDefault Data.arlanda

        destination =
            List.filter (airportHasThisId route.originId) ports
                |> List.head
                |> Maybe.withDefault Data.arlanda
    in
    AirRoute route.airlineId route.airline origin destination (round <| distance origin.location destination.location Kilometers)


airportHasThisId : Int -> Airport -> Bool
airportHasThisId id aPort =
    aPort.id == id


getAirports : Cmd Msg
getAirports =
    Http.get
        { url = "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
        , expect = Http.expectString GotAirports
        }


getAirRoutes : Cmd Msg
getAirRoutes =
    Http.get
        { url = "https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat"
        , expect = Http.expectString GotRoutes
        }


type alias IntermediateAirRoute =
    { airlineId : String
    , flightNr : String
    , origin : Int
    , destinatin : Int
    }
