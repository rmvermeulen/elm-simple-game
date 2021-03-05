module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Html.Attributes exposing (style)
import Keyboard exposing (Key(..))
import Keyboard.Arrows exposing (Direction(..), arrowsDirection, wasdDirection)
import Time exposing (Posix)



---- MODEL ----


type alias HitPoints =
    Int


type Actor
    = Player HitPoints
    | Enemy HitPoints
    | Platform Float Float
    | Particle


type alias CanvasItem =
    { actor : Actor
    , position : ( Float, Float )
    , velocity : ( Float, Float )
    }


type alias Model =
    { canvasItems : List CanvasItem
    , mLastFrameTime : Maybe Time.Posix
    , pressedKeys : List Key
    }


init : ( Model, Cmd Msg )
init =
    ( { canvasItems =
            [ { actor = Player 10
              , position = ( 10, 10 )
              , velocity = ( 0, 0 )
              }
            , { actor = Enemy 10
              , position = ( 100, 10 )
              , velocity = ( 0, 0 )
              }
            , { actor = Platform 100 20
              , position = ( 50, 50 )
              , velocity = ( 0, 0 )
              }
            ]
      , mLastFrameTime = Nothing
      , pressedKeys = []
      }
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

                updateItems =
                    let
                        joystick =
                            getJoystick model
                    in
                    List.map (updateCanvasItem delta joystick)
            in
            simply
                { model
                    | mLastFrameTime = Just time
                    , canvasItems = updateItems model.canvasItems
                }

        KeyMsg kMsg ->
            simply { model | pressedKeys = Keyboard.update kMsg model.pressedKeys }


tupleOp : (value -> value -> value) -> ( value, value ) -> ( value, value ) -> ( value, value )
tupleOp op ( a1, a2 ) ( b1, b2 ) =
    ( op a1 b1, op a2 b2 )


tAdd : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
tAdd =
    tupleOp (+)


tMultiply : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
tMultiply =
    tupleOp (*)


tOp fn =
    Tuple.mapBoth fn fn


updateCanvasItem : Float -> ( Float, Float ) -> CanvasItem -> CanvasItem
updateCanvasItem delta joystick { actor, position, velocity } =
    let
        scale =
            tOp <| (*) delta
    in
    { actor = actor
    , position = tAdd position (scale velocity)
    , velocity =
        case actor of
            Player _ ->
                tAdd velocity (scale joystick)

            _ ->
                velocity
    }



---- VIEW ----


view : Model -> Html msg
view model =
    let
        ( w, h ) =
            ( 500, 300 )
    in
    model.canvasItems
        |> List.map renderCanvasItem
        |> (++) [ clear ( 0, 0 ) w h ]
        |> Canvas.toHtml ( w, h )
            [ style "border" "1px solid black"
            , style "background-color" "lightblue"
            , style "display" "inline-block"
            , style "width" (String.fromInt w ++ "px")
            , style "height" (String.fromInt h ++ "px")
            ]


renderCanvasItem : CanvasItem -> Renderable
renderCanvasItem { actor, position } =
    let
        ( w, h ) =
            ( 20, 20 )
    in
    case actor of
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
