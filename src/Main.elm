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


findInList : (a -> Bool) -> List a -> Maybe a
findInList predicate lst =
    case lst of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                findInList predicate rest


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
        mag =
            magnitude ( x, y )
    in
    if mag > 0 then
        ( (x / mag) * maxSpeed, (y / mag) * maxSpeed )

    else
        ( 0, 0 )



{--let
        norm =
            (1 / magnitude ( x, y )) * maxSpeed
    in
    ( x * norm, y * norm )
    --}


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
    Random.map4 (\point acceleration velocity dna -> Vehicle point acceleration velocity dna Nothing)
        randomPointGenerator
        randomAccelerationGenerator
        randomVelocityGenerator
        randomDnaGenerator


randomVehiclesGenerator : Int -> Random.Generator (List Vehicle)
randomVehiclesGenerator amount =
    Random.list amount randomVehicleGenerator


randomFoodGenerator : Random.Generator Point
randomFoodGenerator =
    Random.map2 Tuple.pair (Random.float 0 width) (Random.float 0 height)


randomFoodsGenerator : Int -> Random.Generator (List Point)
randomFoodsGenerator amount =
    Random.list amount randomFoodGenerator



---- MODEL ----


type alias Acceleration =
    ( Float, Float )


type alias Velocity =
    ( Float, Float )


type alias Dna =
    { foodAttraction : Float
    , foodSense : Float
    }


type alias ClosestFood =
    { object : Point, distance : Float }


type alias Vehicle =
    { point : Point
    , acceleration : Acceleration
    , velocity : Velocity
    , dna : Dna
    , closestFood : Maybe ClosestFood
    }


type alias Model =
    { maxForce : Float
    , vehicles : List Vehicle
    , foods : List Point
    }


init : ( Model, Cmd Msg )
init =
    ( { maxForce = 0.5
      , vehicles = []
      , foods = []
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
    | NewFoods (List Point)


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
                newVehiclesPopulation =
                    model.vehicles
                        |> List.map
                            (\vehicle ->
                                let
                                    ( pointX, pointY ) =
                                        vehicle.point

                                    closestFood =
                                        model.foods
                                            |> List.foldl
                                                (\food acc ->
                                                    let
                                                        ( foodX, foodY ) =
                                                            food

                                                        distance =
                                                            magnitude ( foodX - pointX, foodY - pointY )
                                                    in
                                                    if distance < vehicle.dna.foodSense then
                                                        case acc.closestDistance of
                                                            Just closestDistance ->
                                                                if distance < closestDistance then
                                                                    { acc | closest = Just food, closestDistance = Just closestDistance }

                                                                else
                                                                    acc

                                                            Nothing ->
                                                                { acc | closest = Just food, closestDistance = Just distance }

                                                    else
                                                        acc
                                                )
                                                { closest = Nothing, closestDistance = Nothing }
                                in
                                case ( closestFood.closest, closestFood.closestDistance ) of
                                    ( Just closest, Just distance ) ->
                                        { vehicle | closestFood = Just (ClosestFood closest distance) }

                                    _ ->
                                        vehicle
                            )

                newObjectsPopulation =
                    model.foods
                        |> List.filter
                            (\point ->
                                let
                                    closestVehicle =
                                        model.vehicles
                                            |> findInList
                                                (\vehicle ->
                                                    case vehicle.closestFood of
                                                        Just closestFood ->
                                                            if closestFood.object == point then
                                                                True

                                                            else
                                                                False

                                                        Nothing ->
                                                            False
                                                )
                                in
                                case closestVehicle of
                                    Just { closestFood } ->
                                        case closestFood of
                                            Just { distance } ->
                                                if distance < 5 then
                                                    False

                                                else
                                                    True

                                            Nothing ->
                                                True

                                    Nothing ->
                                        True
                            )
            in
            ( { model | vehicles = updateVehiclesPopulation newVehiclesPopulation, foods = newObjectsPopulation }, Cmd.none )

        NewVehicles vehicles ->
            ( { model | vehicles = List.append vehicles model.vehicles }, Cmd.none )

        NewFoods foods ->
            ( { model | foods = List.append foods model.foods }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        foods =
            List.map
                (\food ->
                    shapes [ fill Color.lightGreen ]
                        [ circle food 4 ]
                )
                model.foods
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
            ]
            foods
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
