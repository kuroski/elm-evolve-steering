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


maxSpeed =
    3.0



---- HELPERS ----


magnitude : ( Float, Float ) -> Float
magnitude ( x, y ) =
    ((x * x) + (y * y))
        |> sqrt


normalize : ( Float, Float ) -> ( Float, Float )
normalize ( x, y ) =
    let
        norm =
            (1 / magnitude ( x, y )) * maxSpeed
    in
    ( x * norm, y * norm )


randomPointGenerator : Random.Generator Point
randomPointGenerator =
    Random.map2 Tuple.pair (Random.float 0 width) (Random.float 0 height)


randomAccelerationGenerator : Random.Generator Acceleration
randomAccelerationGenerator =
    Random.map2 Tuple.pair (Random.float 0 1) (Random.float 0 1)


randomVelocityGenerator : Random.Generator Velocity
randomVelocityGenerator =
    Random.map2 (\x y -> normalize ( x, y )) (Random.float 0 100) (Random.float 0 100)


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
    , vehicles : List Vehicle
    , objects : List Object
    }


init : ( Model, Cmd Msg )
init =
    ( { maxForce = 0.5
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


steerForce : Point -> Velocity -> ( Float, Float )
steerForce ( pointX, pointY ) ( velocityX, velocityY ) =
    let
        limit =
            10

        desiredX =
            if pointX < limit then
                -- goes right
                Just maxSpeed

            else if pointX > (width - limit) then
                -- goes left
                Just -maxSpeed

            else
                Nothing

        desiredY =
            if pointY < limit then
                -- goes down
                Just maxSpeed

            else if pointY > (height - limit) then
                -- goes up
                Just -maxSpeed

            else
                Nothing

        ( normalizedX, normalizedY ) =
            case ( desiredX, desiredY ) of
                ( Just x, Just y ) ->
                    normalize ( x, y )

                ( Just x, Nothing ) ->
                    normalize ( x, velocityY )

                ( Nothing, Just y ) ->
                    normalize ( velocityX, y )

                ( Nothing, Nothing ) ->
                    ( velocityX, velocityY )
    in
    ( normalizedX - velocityX, normalizedY - velocityY )


updateVehiclesPopulation : List Vehicle -> List Vehicle
updateVehiclesPopulation =
    List.map
        (\vehicle ->
            let
                -- BOUNDARIES
                ( steerX, steerY ) =
                    steerForce vehicle.point vehicle.velocity

                ( accelerationX, accelerationY ) =
                    Tuple.mapBoth ((+) steerX) ((+) steerY) vehicle.acceleration

                -- MOTION
                ( velocityX, velocityY ) =
                    Tuple.mapBoth ((+) accelerationX) ((+) accelerationY) vehicle.velocity

                newPoint =
                    Tuple.mapBoth ((+) velocityX) ((+) velocityY) vehicle.point
            in
            { vehicle | point = newPoint, velocity = ( velocityX, velocityY ), acceleration = ( accelerationX * 0, accelerationY * 0 ) }
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            ( { model | vehicles = updateVehiclesPopulation model.vehicles }, Cmd.none )

        NewVehicle vehicle ->
            ( { model | vehicles = vehicle :: model.vehicles }, Cmd.none )

        NewVehicles vehicles ->
            ( { model | vehicles = List.append vehicles model.vehicles }, Cmd.none )



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
