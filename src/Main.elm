module Main exposing (init, main)

import Array
import Browser
import Csv
import Dict exposing (Dict, empty, get)
import Element exposing (..)
import Element.Background as BG
import Element.Font as Font
import Element.Input as Input
import Geodesy exposing (Coordinates, Unit(..), distance)
import Html exposing (Html)
import Http



---- MODEL ----


type alias Model =
    { routes : Dict RouteId Route
    , airports : Dict AirportId Airport
    , toDisplay : List Route
    }


type alias Airport =
    { id : AirportId
    , name : String
    , city : String
    , location : Coordinates
    }


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
    ( initialModel, Cmd.batch [ getAirports, getRoutes ] )



---- UPDATE ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = GotAirports (Result Http.Error String)
    | GotRoutes (Result Http.Error String)
    | LongestSelected
    | ShortestSelected


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
                    ( { model | airports = parseAllAirports splitData }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotRoutes result ->
            case result of
                Ok data ->
                    let
                        splitData =
                            data |> Csv.split
                    in
                    ( { model | routes = parseAllRoutes splitData }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        LongestSelected ->
            let
                updatedRoutes =
                    model.routes
                        |> Dict.map (addDistancesDict (getAirPortById model.airports))

                longestFlights =
                    Dict.foldl longestExtractor [] updatedRoutes
            in
            ( { model | routes = updatedRoutes, toDisplay = longestFlights }, Cmd.none )

        ShortestSelected ->
            let
                updatedRoutes =
                    model.routes
                        |> Dict.map (addDistancesDict (getAirPortById model.airports))

                longestFlights =
                    Dict.foldl shortestExtractor [] updatedRoutes
            in
            ( { model | routes = updatedRoutes, toDisplay = longestFlights }, Cmd.none )



-- COMMANDS


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



--  View


view : Model -> Html Msg
view model =
    Element.layout [ BG.color (rgb 0.1 0.1 0.1), Font.color (rgb 0.8 0.8 0.8) ]
        (column [ width fill ]
            [ statusRow model
            , resultTable model
            ]
        )


resultTable : Model -> Element Msg
resultTable model =
    column [ width fill ]
        [ paragraph [ width fill, Font.center ]
            [ text "Built Dictionaries from airports and routes data. There is "
            , el [ Font.color (rgb 1 0.5 0) ] <| text (String.fromInt (Dict.size model.airports))
            , text " airports  and "
            , el [ Font.color (rgb 1 0.5 0) ] <| text (String.fromInt (Dict.size model.routes))
            , text " unique routes. "
            ]
        , row [ centerX, spacing 20 ]
            [ Input.button [ Font.underline ]
                { onPress = Just LongestSelected
                , label = text "Longest"
                }
            , Input.button [ Font.underline ]
                { onPress = Just ShortestSelected
                , label = text "Shortest"
                }
            ]
        , column
            [ width fill, Font.center, Font.size 18, paddingXY 100 20 ]
          <|
            List.map (routeView model.airports) model.toDisplay
        ]


statusRow : Model -> Element Msg
statusRow model =
    row [ paddingXY 200 30, spacing 50, alignRight, Font.color (rgb 0.3 0.3 0.3) ]
        [ el [] <| text <| "airports: " ++ String.fromInt (Dict.size model.airports)
        , el [] <| text <| "routes: " ++ String.fromInt (Dict.size model.routes)
        ]


routeView : Dict AirportId Airport -> Route -> Element Msg
routeView airports ar =
    let
        getPort =
            getAirPortById airports
    in
    row [ width fill, spaceEvenly, Font.color (rgb 0.5 0.5 0.5) ]
        [ el [ paddingXY 10 3 ] <| row [] [ el [] <| text "from: ", el [ Font.color (rgb 0.8 0.8 0) ] <| text (airportToString (getPort ar.origin)) ]
        , el [ paddingXY 10 3 ] <| row [] [ el [] <| text "to: ", el [ Font.color (rgb 0.8 0.8 0) ] <| text (airportToString (getPort ar.destination)) ]
        , el [ paddingXY 10 3 ] <| row [] [ el [ alignRight ] <| text "distance: ", el [ Font.color (rgb 1 0.5 0) ] <| text (String.fromInt ar.distance ++ " km") ]
        ]



-- PARSE


parseAllAirports : List (List String) -> Dict AirportId Airport
parseAllAirports data =
    let
        airports =
            data
                |> List.map buildAirportTuple
                |> Dict.fromList
    in
    airports


parseAllRoutes : List (List String) -> Dict RouteId Route
parseAllRoutes data =
    data
        |> List.map buildRouteTuple
        |> Dict.fromList


createRouteId : Route -> RouteId
createRouteId r =
    let
        id =
            if r.origin < r.destination then
                r.origin * 10000 + r.destination

            else
                r.destination * 10000 + r.origin
    in
    id


buildAirportTuple : List String -> ( AirportId, Airport )
buildAirportTuple list =
    let
        a =
            Array.fromList list

        id =
            Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 0 a)))

        name =
            Maybe.withDefault "Not Found" (Array.get 1 a)

        city =
            Maybe.withDefault "Not Found" (Array.get 2 a)

        location =
            ( Maybe.withDefault 0 (String.toFloat (Maybe.withDefault "0" (Array.get 6 a)))
            , Maybe.withDefault 0 (String.toFloat (Maybe.withDefault "0" (Array.get 7 a)))
            )
    in
    ( id, { id = id, name = name, city = city, location = location } )


buildRouteTuple : List String -> ( RouteId, Route )
buildRouteTuple list =
    let
        intermediate =
            buildRouteIntermediate list

        actual =
            { airlineId = intermediate.airlineId
            , airline = intermediate.airline
            , origin = intermediate.originId
            , destination = intermediate.destinatinId
            , distance = 0
            }
    in
    ( createRouteId actual, actual )


buildRouteIntermediate : List String -> RouteIntermediate
buildRouteIntermediate list =
    let
        a =
            Array.fromList list
    in
    { airlineId = Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 1 a)))
    , airline = Maybe.withDefault "Not Found" (Array.get 0 a)
    , originId = Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 3 a)))
    , destinatinId = Maybe.withDefault 0 (String.toInt (Maybe.withDefault "0" (Array.get 5 a)))
    }



-- EXTRAS


type alias AirlineId =
    Int


type alias RouteId =
    Int


type alias AirportId =
    Int


getAirPortById : Dict AirportId Airport -> AirportId -> Maybe Airport
getAirPortById airports id =
    Dict.get id airports


airportToString : Maybe Airport -> String
airportToString ap =
    case ap of
        Just { name } ->
            name

        Nothing ->
            "[N/A]"


longestExtractor : RouteId -> Route -> List Route -> List Route
longestExtractor _ route res =
    route
        :: res
        |> List.sortBy .distance
        |> List.reverse
        |> List.take 10


shortestExtractor : RouteId -> Route -> List Route -> List Route
shortestExtractor _ route res =
    if route.distance == 0 then
        res

    else
        route
            :: res
            |> List.sortBy .distance
            |> List.take 10


addDistancesDict : (AirportId -> Maybe Airport) -> RouteId -> Route -> Route
addDistancesDict getAirport _ route =
    let
        airport1 =
            getAirport route.origin

        airport2 =
            getAirport route.destination
    in
    case ( airport1, airport2 ) of
        ( Just a1, Just a2 ) ->
            let
                dist =
                    round <| distance a1.location a2.location Kilometers
            in
            { route | distance = dist }

        _ ->
            { route | distance = 0 }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


initialModel : Model
initialModel =
    { routes = Dict.empty
    , airports = Dict.empty
    , toDisplay = []
    }
