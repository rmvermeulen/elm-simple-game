module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Html.Attributes exposing (attribute, height, style, width)
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
    }


init : ( Model, Cmd Msg )
init =
    ( { canvasItems =
            [ { actor = Player 10
              , position = ( 10, 10 )
              , velocity = ( 1, 0 )
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
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
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
                ms =
                    model.mLastFrameTime
                        |> Maybe.map (Time.posixToMillis >> (-) (Time.posixToMillis time))
                        |> Maybe.withDefault 0

                updateItems =
                    List.map (updateCanvasItem ms)
            in
            simply
                { model
                    | mLastFrameTime = Just time
                    , canvasItems = model.canvasItems |> updateItems
                }

        _ ->
            simply model


tupleOp : (value -> value -> value) -> ( value, value ) -> ( value, value ) -> ( value, value )
tupleOp op ( a1, a2 ) ( b1, b2 ) =
    ( op a1 b1, op a2 b2 )


tAdd : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
tAdd =
    tupleOp (+)


tMultiply : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
tMultiply =
    tupleOp (*)


updateCanvasItem : Int -> CanvasItem -> CanvasItem
updateCanvasItem ms { actor, position, velocity } =
    let
        newPosition =
            tAdd position velocity
    in
    CanvasItem actor newPosition velocity



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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> onAnimationFrame Tick
        }
