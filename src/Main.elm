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


maxForce =
    0.5


eatRadius =
    8



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
    Random.map2 (\x y -> normalize ( x, y ) |> Tuple.mapBoth ((*) maxSpeed) ((*) maxSpeed)) (Random.float 0 1) (Random.float 0 1)


randomDnaGenerator : Random.Generator Dna
randomDnaGenerator =
    Random.map2 Dna (Random.float -3 3) (Random.float 5 100)


randomVehicleGenerator : Random.Generator Vehicle
randomVehicleGenerator =
    Random.map4 (\point acceleration velocity dna -> Vehicle point acceleration velocity dna 1.0 Nothing)
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


rollTheDiceGenerator : Random.Generator Float
rollTheDiceGenerator =
    Random.float 0 1


reproduceVehicleGenerator : Vehicle -> Random.Generator ( Float, Vehicle )
reproduceVehicleGenerator vehicle =
    Random.map2
        (\adjustFoodAttraction adjustFoodSense ->
            let
                foodAttraction =
                    if adjustFoodAttraction < 0.1 then
                        Random.float -0.1 0.2

                    else
                        Random.constant vehicle.dna.foodAttraction

                foodSense =
                    if adjustFoodSense < 0.1 then
                        Random.float -20 20

                    else
                        Random.constant vehicle.dna.foodSense
            in
            ( foodAttraction, foodSense )
        )
        (Random.float 0 1)
        (Random.float 0 1)
        |> Random.andThen
            (\( foodAttractionGenerator, foodSenseGenerator ) ->
                Random.map3
                    (\foodAttraction foodSense shouldGenerate ->
                        let
                            dna =
                                vehicle.dna

                            newFoodAttraction =
                                if foodAttraction == dna.foodAttraction then
                                    dna.foodAttraction

                                else
                                    dna.foodAttraction + foodAttraction

                            newFoodSense =
                                if foodSense == dna.foodSense then
                                    dna.foodSense

                                else
                                    dna.foodSense + foodSense

                            newDna =
                                { dna | foodAttraction = newFoodAttraction, foodSense = newFoodSense }
                        in
                        ( shouldGenerate
                        , Vehicle vehicle.point
                            vehicle.acceleration
                            vehicle.velocity
                            newDna
                            1.0
                            Nothing
                        )
                    )
                    foodAttractionGenerator
                    foodSenseGenerator
                    (Random.float 0 1)
            )



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
    { food : Point, distance : Float }


type alias Vehicle =
    { point : Point
    , acceleration : Acceleration
    , velocity : Velocity
    , dna : Dna
    , health : Float
    , closestFood : Maybe ClosestFood
    }


type alias Model =
    { vehicles : List Vehicle
    , foods : List Point
    }


init : ( Model, Cmd Msg )
init =
    ( { vehicles = []
      , foods = []
      }
    , Cmd.batch
        [ Random.generate NewVehicles (randomVehiclesGenerator 1)
        , Random.generate NewFoods (randomFoodsGenerator 40)
        ]
    )



---- UPDATE ----


type Msg
    = Frame Float
    | NewVehicles (List Vehicle)
    | NewFoods (List Point)
    | RollTheDice Float
    | NewFood Point
    | NewVehicle ( Float, Vehicle )


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
            { vehicle | point = newPoint, velocity = ( velocityX, velocityY ), acceleration = ( 0, 0 ), health = vehicle.health - 0.005 }
        )


findClosestFood : List Point -> List Vehicle -> List Vehicle
findClosestFood foods vehicles =
    vehicles
        |> List.map
            (\vehicle ->
                let
                    ( pointX, pointY ) =
                        vehicle.point

                    closestFood =
                        foods
                            |> List.foldl
                                (\food acc ->
                                    let
                                        ( foodX, foodY ) =
                                            food

                                        distance =
                                            ( pointX - foodX, pointY - foodY )
                                                |> magnitude
                                    in
                                    if distance < vehicle.dna.foodSense then
                                        case acc of
                                            Just closest ->
                                                if distance < closest.distance then
                                                    Just (ClosestFood food distance)

                                                else
                                                    acc

                                            Nothing ->
                                                Just (ClosestFood food distance)

                                    else
                                        acc
                                )
                                Nothing
                in
                case closestFood of
                    Just closest ->
                        { vehicle | closestFood = Just closest }

                    _ ->
                        vehicle
            )


eatFood : List Point -> List Vehicle -> List Point
eatFood foods vehicles =
    foods
        |> List.filter
            (\point ->
                let
                    closestVehicle =
                        vehicles
                            |> findInList
                                (\vehicle ->
                                    case vehicle.closestFood of
                                        Just closestFood ->
                                            if closestFood.food == point then
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
                                if distance < eatRadius then
                                    False

                                else
                                    True

                            Nothing ->
                                True

                    Nothing ->
                        True
            )


seekFood : List Vehicle -> List Vehicle
seekFood =
    List.map
        (\vehicle ->
            case vehicle.closestFood of
                Just closestFood ->
                    let
                        ( foodX, foodY ) =
                            closestFood.food

                        ( pointX, pointY ) =
                            vehicle.point

                        ( accelerationX, accelerationY ) =
                            vehicle.acceleration

                        ( steerX, steerY ) =
                            ( foodX - pointX, foodY - pointY )
                                |> normalize
                                |> Tuple.mapBoth ((*) maxSpeed) ((*) maxSpeed)
                                |> Tuple.mapBoth ((*) vehicle.dna.foodAttraction) ((*) vehicle.dna.foodAttraction)
                                |> limit maxForce
                    in
                    if closestFood.distance < eatRadius then
                        { vehicle | closestFood = Nothing, acceleration = ( accelerationX + steerX, accelerationY + steerY ), health = vehicle.health + 0.3 }

                    else
                        { vehicle | closestFood = Nothing, acceleration = ( accelerationX + steerX, accelerationY + steerY ) }

                Nothing ->
                    { vehicle | closestFood = Nothing }
        )


killUndeads : List Vehicle -> List Vehicle
killUndeads =
    List.filter (.health >> (<=) 0.0)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
            let
                -- EAT
                newVehiclesPopulation =
                    findClosestFood model.foods model.vehicles

                newFoodPopulation =
                    eatFood model.foods newVehiclesPopulation

                -- SEEK
                newNewVehiclesPopulation =
                    newVehiclesPopulation
                        |> seekFood
                        |> updateVehiclesPopulation
                        |> killUndeads

                generateBirthVehicles =
                    newNewVehiclesPopulation
                        |> List.map (\vehicle -> Random.generate NewVehicle (reproduceVehicleGenerator vehicle))
            in
            ( { model
                | vehicles = newNewVehiclesPopulation
                , foods = newFoodPopulation
              }
            , Cmd.batch
                (List.append
                    [ Random.generate RollTheDice rollTheDiceGenerator
                    ]
                    generateBirthVehicles
                )
            )

        NewVehicles vehicles ->
            ( { model | vehicles = List.append vehicles model.vehicles }, Cmd.none )

        NewVehicle ( shouldGenerate, vehicle ) ->
            if shouldGenerate < 0.001 then
                let
                    newVehicles =
                        vehicle :: model.vehicles

                    _ =
                        Debug.log "bla" (List.length newVehicles)
                in
                ( { model | vehicles = newVehicles }, Cmd.none )

            else
                ( model, Cmd.none )

        NewFoods foods ->
            ( { model | foods = List.append foods model.foods }, Cmd.none )

        NewFood food ->
            ( { model | foods = food :: model.foods }, Cmd.none )

        RollTheDice probability ->
            if probability < 0.01 then
                ( model, Random.generate NewFood randomFoodGenerator )
                -- ( model, Cmd.none )

            else
                ( model, Cmd.none )



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
            , shapes [ stroke Color.lightYellow ]
                (List.map
                    (\vehicle ->
                        circle vehicle.point vehicle.dna.foodSense
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
