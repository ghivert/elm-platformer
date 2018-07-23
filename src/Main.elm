module Main exposing (..)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Task exposing (Task)
import Update
import Time exposing (Posix)

import Setters exposing (..)

type alias Model =
  { viewport : Viewport
  , position : Int
  , tiles : List Tile
  , player : Position
  }

type alias Viewport =
  { width : Float
  , height : Float
  }

type alias Tile =
  { column : Int
  , row : Int
  , content : String
  }

type alias Position =
  { x : Int
  , y : Int
  }

type Msg
  = ViewportSize Dom.Viewport
  | ResizeWindow Int Int
  | AnimationFrame Posix

tileSize : Int
tileSize = 64

rowsNumber : Int
rowsNumber = 15

colsNumber : Int
colsNumber = 30

defaultBackgroundColor : String
defaultBackgroundColor = "#DFF6FF"

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : () -> (Model, Cmd Msg)
init = always (start, getViewport)

start : Model
start =
  Model (Viewport 0 0) 0 ground (Position 0 0)

getViewport : Cmd Msg
getViewport = Task.perform ViewportSize Dom.getViewport

ground : List Tile
ground =
  List.range 1 colsNumber
  |> List.map (\col -> Tile col rowsNumber "/assets/Tiles/platformPack_tile001.png")

saveViewportIn : Model -> Float -> Float -> (Model, Cmd Msg)
saveViewportIn model width height =
  Viewport width height
  |> setViewportIn model
  |> Update.identity

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ViewportSize { viewport } -> saveViewportIn model viewport.width viewport.height
    ResizeWindow width height -> saveViewportIn model (toFloat width) (toFloat height)
    AnimationFrame time ->
      model
      |> applyGravity
      |> Update.identity

applyGravity : Model -> Model
applyGravity ({ player } as model) =
  model

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onResize ResizeWindow
    , Browser.Events.onAnimationFrame AnimationFrame
    ]

-- View Functions

view : Model -> Document Msg
view model =
  Document
    "Elm Platformer"
    [ Html.node "style" []
      [ Html.text <| String.join "\n"
        [ "* { box-sizing: border-box; }"
        , "body { margin: 0; }"
        ]
      ]
    , Html.div
      [ Attributes.style "width" "100vw"
      , Attributes.style "height" "100vh"
      , Attributes.style "overflow" "hidden"
      , Attributes.style "background-color" defaultBackgroundColor
      ]
      [ grid model.position model.viewport
      , playerView model.player
      ]
    ]

grid : Int -> Viewport -> Html Msg
grid position { width, height } =
  Html.div
    [ Attributes.style "display" "grid"
    , Attributes.style "min-height" "100%"
    , Attributes.style "grid-template-rows" (cssRepeat rowsNumber tileSize)
    , Attributes.style "grid-template-columns" (cssRepeat colsNumber tileSize)
    , Attributes.style "position" "absolute"
    , Attributes.style "bottom" "0"
    , Attributes.style "left" (toPx position)
    ]
    (List.map tileView ground)

tileView : Tile -> Html Msg
tileView { column, row, content } =
  Html.img
    [ Attributes.src content
    , Attributes.style "grid-column-start" (String.fromInt column)
    , Attributes.style "grid-row-start" (String.fromInt row)
    ]
    []

playerView : Position -> Html Msg
playerView { x, y } =
  Html.img
    [ Attributes.src "/assets/Characters/platformChar_idle.png"
    , Attributes.style "position" "absolute"
    , Attributes.style "top" (toPx y)
    , Attributes.style "left" (toPx x)
    , Attributes.style "z-index" "1000"
    ]
    []

toPx : Int -> String
toPx position =
  String.fromInt position ++ "px"

cssRepeat : Int -> Int -> String
cssRepeat times size =
  "repeat(" ++ String.fromInt times ++ ", " ++ String.fromInt size ++ "px)"
