module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Random



---- CONSTANTS ----


width =
    800


height =
    600



---- HELPERS ----


magnitude : ( Float, Float ) -> Float
magnitude ( x, y ) =
    ((x * x) + (y * y))
        |> sqrt


randomPointGenerator : Random.Generator Point
randomPointGenerator =
    Random.map2 Tuple.pair (Random.float 0 width) (Random.float 0 height)


randomAccelerationGenerator : Random.Generator Acceleration
randomAccelerationGenerator =
    Random.map2 Tuple.pair (Random.float 0 1) (Random.float 0 1)


randomVelocityGenerator : Random.Generator Velocity
randomVelocityGenerator =
    Random.map2 Tuple.pair (Random.float 0 100) (Random.float 0 100)


randomVehicleGenerator : Random.Generator Vehicle
randomVehicleGenerator =
    Random.map3 Vehicle randomPointGenerator randomAccelerationGenerator randomVelocityGenerator


randomVehiclesGenerator : Int -> Random.Generator (List Vehicle)
randomVehiclesGenerator amount =
    Random.list amount randomVehicleGenerator



---- MODEL ----


type alias Acceleration =
    ( Float, Float )


type alias Velocity =
    ( Float, Float )


type alias Vehicle =
    { point : Point
    , acceleration : Acceleration
    , velocity : Velocity
    }


type Object
    = Food Point Float
    | Poison Point Float


type alias Model =
    { maxForce : Float
    , maxSpeed : Float
    , vehicles : List Vehicle
    , objects : List Object
    }


init : ( Model, Cmd Msg )
init =
    ( { maxForce = 0.5
      , maxSpeed = 3.0
      , vehicles = []
      , objects = []
      }
    , Random.generate NewVehicles (randomVehiclesGenerator 10)
    )



---- UPDATE ----


type Msg
    = Frame Float
    | NewVehicle Vehicle
    | NewVehicles (List Vehicle)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            let
                newPopulation =
                    List.map
                        (\vehicle ->
                            let
                                -- BOUNDARIES
                                limit =
                                    10

                                desiredX =
                                    if Tuple.first vehicle.point < limit then
                                        Just model.maxSpeed

                                    else if Tuple.first vehicle.point > (width - limit) then
                                        Just -model.maxSpeed

                                    else
                                        Nothing

                                desiredY =
                                    if Tuple.second vehicle.point < limit then
                                        Just model.maxSpeed

                                    else if Tuple.second vehicle.point > (height - limit) then
                                        Just -model.maxSpeed

                                    else
                                        Nothing

                                steer =
                                    case ( desiredX, desiredY ) of
                                        ( Just x, Just y ) ->
                                            let
                                                mag =
                                                    magnitude ( x, y )

                                                normalizedX =
                                                    x * (1 / mag) * model.maxSpeed

                                                normalizedY =
                                                    y * (1 / mag) * model.maxSpeed
                                            in
                                            Tuple.mapBoth ((-) normalizedX) ((-) normalizedY) vehicle.velocity

                                        ( Just x, Nothing ) ->
                                            let
                                                mag =
                                                    magnitude ( x, Tuple.second vehicle.velocity )

                                                normalizedX =
                                                    x * (1 / mag) * model.maxSpeed

                                                normalizedY =
                                                    Tuple.second vehicle.velocity * (1 / mag) * model.maxSpeed
                                            in
                                            Tuple.mapBoth ((-) normalizedX) ((-) normalizedY) vehicle.velocity

                                        ( Nothing, Just y ) ->
                                            let
                                                mag =
                                                    magnitude ( Tuple.first vehicle.velocity, y )

                                                normalizedX =
                                                    Tuple.first vehicle.velocity * (1 / mag) * model.maxSpeed

                                                normalizedY =
                                                    y * (1 / mag) * model.maxSpeed
                                            in
                                            Tuple.mapBoth ((-) normalizedX) ((-) normalizedY) vehicle.velocity

                                        ( Nothing, Nothing ) ->
                                            ( 0, 0 )

                                newAcceleration =
                                    Tuple.mapBoth ((+) (Tuple.first steer)) ((+) (Tuple.second steer)) vehicle.acceleration

                                -- MOTION
                                accelerationX =
                                    Tuple.first newAcceleration

                                accelerationY =
                                    Tuple.second newAcceleration

                                newVelocity =
                                    Tuple.mapBoth ((+) accelerationX) ((+) accelerationY) vehicle.velocity

                                velocityX =
                                    Tuple.first newVelocity

                                velocityY =
                                    Tuple.second newVelocity

                                newPoint =
                                    Tuple.mapBoth ((+) velocityX) ((+) velocityY) vehicle.point
                            in
                            { vehicle | velocity = newVelocity, point = newPoint, acceleration = ( accelerationX * 0, accelerationY * 0 ) }
                        )
                        model.vehicles
            in
            ( { model | vehicles = newPopulation }, Cmd.none )

        NewVehicle vehicle ->
            let
                mag =
                    magnitude vehicle.velocity

                normalizedVelocity =
                    Tuple.mapBoth
                        ((*) ((1 / mag) * model.maxSpeed))
                        ((*) ((1 / mag) * model.maxSpeed))
                        vehicle.velocity
            in
            ( { model | vehicles = vehicle :: model.vehicles }, Cmd.none )

        NewVehicles vehicles ->
            let
                normalizedVehicles =
                    List.map
                        (\vehicle ->
                            let
                                mag =
                                    magnitude vehicle.velocity

                                normalizedVelocity =
                                    Tuple.mapBoth
                                        ((*) ((1 / mag) * model.maxSpeed))
                                        ((*) ((1 / mag) * model.maxSpeed))
                                        vehicle.velocity
                            in
                            { vehicle | velocity = normalizedVelocity }
                        )
                        vehicles
            in
            ( { model | vehicles = List.append normalizedVehicles model.vehicles }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    Canvas.toHtml ( width, height )
        [ style "border" "10px solid rgba(0,0,0,0.1)"
        , style "background-color" "black"
        ]
        [ shapes [ fill Color.black ] [ rect ( 0, 0 ) width height ]
        , shapes [ fill Color.lightYellow ]
            (List.map
                (\vehicle ->
                    circle vehicle.point 8
                )
                model.vehicles
            )
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \model -> onAnimationFrameDelta Frame
        }
