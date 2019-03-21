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


limit : Float -> ( Float, Float ) -> ( Float, Float )
limit max ( x, y ) =
    let
        magSquare =
            magnitudeSquare ( x, y )

        squareRoot =
            sqrt magSquare
    in
    if magSquare > (max * max) then
        ( (x / squareRoot) * max, (y / squareRoot) * max )

    else
        ( x, y )


magnitudeSquare : ( Float, Float ) -> Float
magnitudeSquare ( x, y ) =
    (x * x) + (y * y)


magnitude : ( Float, Float ) -> Float
magnitude value =
    magnitudeSquare value
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
    Random.map2 (\x y -> normalize ( x, y )) (Random.float 0 1) (Random.float 0 1)


randomDnaGenerator : Random.Generator Dna
randomDnaGenerator =
    Random.map2 Dna (Random.float -3 3) (Random.float 5 100)


randomVehicleGenerator : Random.Generator Vehicle
randomVehicleGenerator =
    Random.map4 Vehicle randomPointGenerator randomAccelerationGenerator randomVelocityGenerator randomDnaGenerator


randomVehiclesGenerator : Int -> Random.Generator (List Vehicle)
randomVehiclesGenerator amount =
    Random.list amount randomVehicleGenerator


randomFoodGenerator : Random.Generator Object
randomFoodGenerator =
    Random.map2 (\a b -> Food ( a, b )) (Random.float 0 width) (Random.float 0 height)


randomFoodsGenerator : Int -> Random.Generator (List Object)
randomFoodsGenerator amount =
    Random.list amount randomFoodGenerator


objectPoint : Object -> Point
objectPoint object =
    case object of
        Food point ->
            point

        Poison point ->
            point



---- MODEL ----


type alias Acceleration =
    ( Float, Float )


type alias Velocity =
    ( Float, Float )


type alias Dna =
    { foodAttraction : Float
    , foodSense : Float
    }


type alias Vehicle =
    { point : Point
    , acceleration : Acceleration
    , velocity : Velocity
    , dna : Dna
    }


type Object
    = Food Point
    | Poison Point


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
    , Cmd.batch
        [ Random.generate NewVehicles (randomVehiclesGenerator 10)
        , Random.generate NewFoods (randomFoodsGenerator 20)
        ]
    )



---- UPDATE ----


type Msg
    = Frame Float
    | NewVehicles (List Vehicle)
    | NewFoods (List Object)


steerForce : Point -> Velocity -> ( Float, Float )
steerForce ( pointX, pointY ) ( velocityX, velocityY ) =
    let
        maxBoundary =
            10

        desiredX =
            if pointX < maxBoundary then
                -- goes right
                Just maxSpeed

            else if pointX > (width - maxBoundary) then
                -- goes left
                Just -maxSpeed

            else
                Nothing

        desiredY =
            if pointY < maxBoundary then
                -- goes down
                Just maxSpeed

            else if pointY > (height - maxBoundary) then
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
                        |> limit maxSpeed

                ( accelerationX, accelerationY ) =
                    Tuple.mapBoth ((+) steerX) ((+) steerY) vehicle.acceleration

                -- MOTION
                ( velocityX, velocityY ) =
                    Tuple.mapBoth ((+) accelerationX) ((+) accelerationY) vehicle.velocity
                        |> limit maxSpeed

                newPoint =
                    Tuple.mapBoth ((+) velocityX) ((+) velocityY) vehicle.point
            in
            { vehicle | point = newPoint, velocity = ( velocityX, velocityY ), acceleration = ( 0, 0 ) }
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            let
                -- EAT
                newObjectsPopulation =
                    model.objects
                        |> List.filter
                            (\object ->
                                let
                                    ( objectX, objectY ) =
                                        objectPoint object

                                    closestVehicle =
                                        model.vehicles
                                            |> List.foldl
                                                (\vehicle acc ->
                                                    let
                                                        ( pointX, pointY ) =
                                                            vehicle.point

                                                        distance =
                                                            magnitude ( pointX - objectX, pointY - objectY )
                                                    in
                                                    if distance < vehicle.dna.foodSense then
                                                        case acc.closestDistance of
                                                            Just closestDistance ->
                                                                if distance < closestDistance then
                                                                    { acc | closest = Just vehicle, closestDistance = Just closestDistance }

                                                                else
                                                                    acc

                                                            Nothing ->
                                                                { acc | closest = Just vehicle, closestDistance = Just distance }

                                                    else
                                                        acc
                                                )
                                                { closest = Nothing, closestDistance = Nothing }
                                in
                                case ( closestVehicle.closest, closestVehicle.closestDistance ) of
                                    ( Just vehicle, Just distance ) ->
                                        if distance < 5 then
                                            False

                                        else
                                            True

                                    _ ->
                                        True
                            )
            in
            ( { model | vehicles = updateVehiclesPopulation model.vehicles, objects = newObjectsPopulation }, Cmd.none )

        NewVehicles vehicles ->
            ( { model | vehicles = List.append vehicles model.vehicles }, Cmd.none )

        NewFoods foods ->
            ( { model | objects = List.append foods model.objects }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        objects =
            List.map
                (\object ->
                    case object of
                        Food point ->
                            shapes [ fill Color.lightGreen ]
                                [ circle point 4 ]

                        Poison point ->
                            shapes [ fill Color.lightRed ]
                                [ circle point 4 ]
                )
                model.objects
    in
    Canvas.toHtml ( width, height )
        [ style "border" "10px solid rgba(0,0,0,0.1)"
        , style "background-color" "black"
        ]
        (List.append
            [ shapes [ fill Color.black ] [ rect ( 0, 0 ) width height ]
            , shapes [ fill Color.lightYellow ]
                (List.map
                    (\vehicle ->
                        circle vehicle.point 8
                    )
                    model.vehicles
                )
            , shapes [ stroke Color.lightYellow ]
                (List.map
                    (\vehicle ->
                        circle vehicle.point vehicle.dna.foodSense
                    )
                    model.vehicles
                )
            ]
            objects
        )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \model -> onAnimationFrameDelta Frame
        }
