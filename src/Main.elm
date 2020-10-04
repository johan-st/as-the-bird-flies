module Main exposing (..)

import Array
import Browser
import Csv
import Data exposing (toDisplayList)
import Dict exposing (Dict, empty, get)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Geodesy exposing (Coordinates, Unit(..), distance)
import Html exposing (Html)
import Http
import Time exposing (Posix)



---- MODEL ----


type Status
    = Init
    | Loading
    | Succeded
    | Failed


type alias Model =
    { routes : List Route
    , airports : Dict AirportId Airport
    , state : Status
    , toDisplay : List Route
    , airportData : List (List String)
    , routeData : List (List String)
    , aniMsg : List AniMsg
    , timeZone : Time.Zone
    , time : Posix
    , speed : Float
    }


type alias Airport =
    { id : AirportId
    , name : String
    , city : String
    , location : Coordinates
    }


portToString : Airport -> List String
portToString ap =
    [ ap.name
    , ap.city
    ]


type alias Route =
    { airlineId : AirlineId
    , airline : String
    , origin : AirportId
    , destination : AirportId
    , distance : Int
    }


type alias RouteIntermediate =
    { airlineId : AirlineId
    , airline : String
    , originId : AirportId
    , destinatinId : AirportId
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, getAirports )



---- UPDATE ----


type AniMsg
    = ParsePort
    | DoneParsePort
    | ParseRoute
    | DoneParseRoute


type Msg
    = GotAirports (Result Http.Error String)
    | GotRoutes (Result Http.Error String)
    | AdjustTimeZone Time.Zone
    | Tick Posix


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
                    -- ( { model | airportData = splitData, aniMsg = List.repeat 1000 ParsePort }, getRoutes )
                    ( { model | airportData = splitData, aniMsg = List.repeat (List.length splitData) ParsePort }, getRoutes )

                Err err ->
                    let
                        debug =
                            Debug.log "get airports failed" err
                    in
                    ( model, Cmd.none )

        GotRoutes result ->
            case result of
                Ok data ->
                    let
                        splitData =
                            data |> Csv.split

                        newAniMsg =
                            List.append model.aniMsg (List.repeat (List.length splitData) ParseRoute)
                    in
                    ( { model | routeData = splitData, aniMsg = newAniMsg }, Cmd.none )

                Err err ->
                    let
                        debug =
                            Debug.log "getting air routes failed" err
                    in
                    ( model, Cmd.none )

        Tick _ ->
            case model.aniMsg of
                head :: tail ->
                    case head of
                        ParsePort ->
                            let
                                ( airports, airportData ) =
                                    parseOneAirport model.airports model.airportData
                            in
                            ( { model | airports = airports, airportData = airportData, aniMsg = tail }, Cmd.none )

                        ParseRoute ->
                            let
                                ( routes, routeData ) =
                                    parseOneRoute model.routes model.routeData model.airports
                            in
                            ( { model | routes = routes, routeData = routeData, aniMsg = tail }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | timeZone = newZone }
            , Cmd.none
            )



-- -- SUBSCRIPTION -- --


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every model.speed Tick


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
    column [ centerX ] <| List.map (routeView model.airports) model.routes


statusRow : Model -> Element Msg
statusRow model =
    row [ paddingXY 200 30, spacing 50 ]
        [ el [] <| text <| "routes: " ++ String.fromInt (List.length model.routes)
        , el [] <| text <| "airports: " ++ length model.airports
        ]


debugView : List (List String) -> Element Msg
debugView list =
    column [ Font.size 10, Font.color (rgb 0.5 0.5 0.5), centerX, spacing 3 ] <| [ text <| Debug.toString <| List.map (\item -> List.concat item) ]


routeView : Dict AirportId Airport -> Route -> Element Msg
routeView airports ar =
    let
        getPort =
            getAirPortById airports
    in
    row [ spaceEvenly ]
        [ el [ paddingXY 10 3 ] <| text <| "id: " ++ String.fromInt ar.airlineId
        , el [ paddingXY 10 3 ] <| text <| "flightNr: " ++ ar.airline
        , el [ paddingXY 10 3 ] <| text <| "from: " ++ airportToString (getPort ar.origin)
        , el [ paddingXY 10 3 ] <| text <| "to: " ++ airportToString (getPort ar.origin)
        , el [ paddingXY 10 3 ] <| text <| "distance: " ++ String.fromInt (getDistance getPort ar.origin ar.destination) ++ " km"
        ]


getAirPortById : Dict AirportId Airport -> AirportId -> Maybe Airport
getAirPortById airports id =
    Dict.get id airports


length : Dict k v -> String
length dict =
    dict
        |> Dict.keys
        |> List.length
        |> String.fromInt


getDistance : (AirportId -> Maybe Airport) -> AirportId -> AirportId -> Int
getDistance getPort id1 id2 =
    case ( getPort id1, getPort id2 ) of
        ( Just port1, Just port2 ) ->
            distance port1.location port2.location Kilometers
                |> round

        _ ->
            0


airportToString : Maybe Airport -> String
airportToString ap =
    case ap of
        Just { id, name, city, location } ->
            name ++ city

        Nothing ->
            "Nothing here"


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
        , subscriptions = subscriptions
        }



-- DECODE


parseOneAirport : Dict AirportId Airport -> List (List String) -> ( Dict AirportId Airport, List (List String) )
parseOneAirport dict data =
    case data of
        first :: rest ->
            let
                airport =
                    buildAirport first
            in
            ( Dict.insert airport.id airport dict, rest )

        _ ->
            ( dict, data )


parseOneRoute : List Route -> List (List String) -> Dict AirportId Airport -> ( List Route, List (List String) )
parseOneRoute list data airports =
    case data of
        first :: rest ->
            let
                route =
                    buildRoute (getAirPortById airports) first

                shortList =
                    route
                        :: list
                        |> List.take 100
            in
            ( shortList, rest )

        _ ->
            ( list, data )


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


buildRoute : (AirportId -> Maybe Airport) -> List String -> Route
buildRoute getPort list =
    let
        intermediate =
            buildRouteIntermediate list

        actual =
            { airlineId = intermediate.airlineId
            , airline = intermediate.airline
            , origin = intermediate.originId
            , destination = intermediate.destinatinId
            , distance = getDistance getPort intermediate.originId intermediate.destinatinId
            }
    in
    actual


buildRouteIntermediate : List String -> RouteIntermediate
buildRouteIntermediate list =
    let
        a =
            Array.fromList list
    in
    { airlineId = Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 0 a)))
    , airline = Maybe.withDefault "Not Found" (Array.get 0 a)
    , originId = Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 3 a)))
    , destinatinId = Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 5 a)))
    }


airportHasThisId : Int -> Airport -> Bool
airportHasThisId id aPort =
    aPort.id == id


getAirports : Cmd Msg
getAirports =
    Http.get
        { url = "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
        , expect = Http.expectString GotAirports
        }


getRoutes : Cmd Msg
getRoutes =
    Http.get
        { url = "https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat"
        , expect = Http.expectString GotRoutes
        }


type alias IntermediateRoute =
    { airlineId : AirlineId
    , flightNr : String
    , origin : Int
    , destinatin : Int
    }



-- EXTRAS


type alias AirlineId =
    Int


type alias RouteId =
    Int


type alias AirportId =
    Int


initialModel : Model
initialModel =
    { routes = []
    , airports = Dict.empty
    , state = Init
    , toDisplay = []
    , airportData = []
    , routeData = []
    , aniMsg = []
    , timeZone = Time.utc
    , time = Time.millisToPosix 0
    , speed = 1
    }
