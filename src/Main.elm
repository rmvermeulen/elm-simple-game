module Main exposing (..)

import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html)
import Html.Attributes exposing (attribute, height, style, width)



---- MODEL ----


type Actor
    = Player
    | Enemy
    | Platform
    | Particle


type alias CanvasItem =
    { actor : Actor
    , position : ( Float, Float )
    }


type alias Model =
    { canvasItems : List CanvasItem }


init : ( Model, Cmd Msg )
init =
    ( { canvasItems =
            [ { actor = Player
              , position = ( 10, 10 )
              }
            , { actor = Enemy
              , position = ( 100, 10 )
              }
            , { actor = Platform
              , position = ( 50, 50 )
              }
            ]
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html msg
view model =
    let
        ( w, h ) =
            ( 500, 300 )
    in
    model.canvasItems
        |> List.map renderCanvasItem
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
        color =
            case actor of
                Player ->
                    Color.blue

                Enemy ->
                    Color.red

                Platform ->
                    Color.brown

                Particle ->
                    Color.orange
    in
    shapes [ fill color ] [ rect position 50 50 ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
