module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Keyboard exposing (Key(..))
import Keyboard.Arrows exposing (Direction(..), arrowsDirection, wasdDirection)
import Set exposing (Set)
import Time exposing (Posix)



---- MODEL ----


type alias HitPoints =
    Int


type alias Components data =
    Dict Int data


type Class
    = Player HitPoints
    | Enemy HitPoints
    | Platform Float Float
    | Particle


type Shape
    = Rect Float Float
    | Circle Float


type alias Transform =
    { position : Point
    , velocity : Point
    , rotation : Float
    }


setPosition : Point -> Transform -> Transform
setPosition v t =
    { t | position = v }


setVelocity : Point -> Transform -> Transform
setVelocity v t =
    { t | velocity = v }


setRotation : Float -> Transform -> Transform
setRotation v t =
    { t | rotation = v }


type alias Model =
    { mLastFrameTime : Maybe Time.Posix
    , pressedKeys : List Key
    , entities : Set Int
    , classes : Components Class
    , shapes : Components Shape
    , transforms : Components Transform
    }


setMLastFrameTime : Maybe Time.Posix -> Model -> Model
setMLastFrameTime v m =
    { m | mLastFrameTime = v }


setPressedKeys : List Key -> Model -> Model
setPressedKeys v m =
    { m | pressedKeys = v }


setEntities : Set Int -> Model -> Model
setEntities v m =
    { m | entities = v }


setClasses : Components Class -> Model -> Model
setClasses v m =
    { m | classes = v }


setShapes : Components Shape -> Model -> Model
setShapes v m =
    { m | shapes = v }


setTransforms : Components Transform -> Model -> Model
setTransforms v m =
    { m | transforms = v }


empty : Model
empty =
    Model Nothing [] Set.empty Dict.empty Dict.empty Dict.empty


init : ( Model, Cmd Msg )
init =
    let
        entities =
            Set.fromList <| List.range 0 5

        createComponents fn =
            entities
                |> Set.toList
                |> List.map (\id -> ( id, fn id ))
                |> Dict.fromList

        shapes =
            createComponents (\id -> Rect 50 50)

        classes =
            createComponents
                (\id ->
                    case id of
                        0 ->
                            Player 3

                        1 ->
                            Enemy 1

                        _ ->
                            Platform 50 20
                )

        transforms =
            createComponents
                (\id -> Transform ( toFloat id * 20, 20 ) ( 10, 10 ) 0)
    in
    ( empty
        |> setEntities entities
        |> setShapes shapes
        |> setClasses classes
        |> setTransforms transforms
    , Cmd.none
    )


isShooting : Model -> Bool
isShooting model =
    List.member Spacebar model.pressedKeys


directionToJoystick : Direction -> ( Float, Float )
directionToJoystick direction =
    let
        v =
            sqrt 2
    in
    case direction of
        North ->
            ( 0, -1 )

        NorthEast ->
            ( v, -v )

        East ->
            ( 1, 0 )

        SouthEast ->
            ( v, v )

        South ->
            ( 0, 1 )

        SouthWest ->
            ( -v, v )

        West ->
            ( -1, 0 )

        NorthWest ->
            ( -v, -v )

        NoDirection ->
            ( 0, 0 )


getJoystick : Model -> ( Float, Float )
getJoystick { pressedKeys } =
    directionToJoystick <|
        case arrowsDirection pressedKeys of
            NoDirection ->
                wasdDirection pressedKeys

            d ->
                d



---- UPDATE ----


type Msg
    = KeyMsg Keyboard.Msg
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        simply m =
            ( m, Cmd.none )
    in
    case msg of
        Tick time ->
            let
                delta : Float
                delta =
                    model.mLastFrameTime
                        |> Maybe.map (Time.posixToMillis >> (-) (Time.posixToMillis time))
                        |> Maybe.withDefault 0
                        |> (\n ->
                                if n == 0 then
                                    0.0

                                else
                                    1.0 / toFloat n
                           )
            in
            model
                |> setMLastFrameTime (Just time)
                |> setTransforms
                    (updateComponents
                        (updateTransform delta)
                        model.entities
                        model.transforms
                    )
                |> simply

        KeyMsg kMsg ->
            simply { model | pressedKeys = Keyboard.update kMsg model.pressedKeys }


updateTransform : Float -> Transform -> Transform
updateTransform delta t =
    let
        position =
            let
                step =
                    t.velocity |> onBoth ((*) delta)
            in
            t.position |> combine (+) step
    in
    t |> setPosition position


combine : (value -> value -> value) -> ( value, value ) -> ( value, value ) -> ( value, value )
combine op ( a1, a2 ) ( b1, b2 ) =
    ( op a1 b1, op a2 b2 )


onBoth fn =
    Tuple.mapBoth fn fn


updateComponents : (c -> c) -> Set Int -> Dict Int c -> Dict Int c
updateComponents fn entities components =
    entities
        |> Set.toList
        |> List.filterMap
            (\id ->
                Dict.get id components
                    |> Maybe.map (Tuple.pair id)
            )
        |> Dict.fromList
        |> Dict.map (\_ -> fn)



---- VIEW ----


view : Model -> Html msg
view model =
    let
        ( w, h ) =
            ( 500, 300 )
    in
    renderEntities model
        |> (++) [ clear ( 0, 0 ) w h ]
        |> Canvas.toHtml ( w, h )
            [ style "border" "1px solid black"
            , style "background-color" "lightblue"
            , style "display" "inline-block"
            , style "width" (String.fromInt w ++ "px")
            , style "height" (String.fromInt h ++ "px")
            ]


renderEntities : Model -> List Renderable
renderEntities { entities, classes, transforms } =
    let
        getClass id =
            Dict.get id classes

        getTransform id =
            Dict.get id transforms

        tryRender =
            \id -> Maybe.map2 renderEntity (getClass id) (getTransform id)
    in
    entities
        |> Set.toList
        |> List.filterMap tryRender


renderEntity : Class -> Transform -> Renderable
renderEntity class { position } =
    let
        ( w, h ) =
            ( 20, 20 )
    in
    case class of
        Player _ ->
            shapes [ fill Color.blue ] [ rect position w h ]

        Enemy _ ->
            shapes [ fill Color.red ] [ rect position w h ]

        Platform pw ph ->
            shapes [ fill Color.brown ] [ rect position pw ph ]

        Particle ->
            shapes [ fill Color.orange ] [ rect position w h ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrame Tick
        , Sub.map KeyMsg Keyboard.subscriptions
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
