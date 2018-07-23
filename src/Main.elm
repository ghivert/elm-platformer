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
  , player : { position : Position }
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

halfTile : Int
halfTile = 32

playerMargin : Int
playerMargin = 16

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
  Model (Viewport 0 0) 0 ground { position = Position 0 200 }

getViewport : Cmd Msg
getViewport = Task.perform ViewportSize Dom.getViewport

ground : List Tile
ground =
  List.range 1 colsNumber
  |> List.map (\col -> Tile col 1 "/assets/Tiles/platformPack_tile001.png")

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
applyGravity ({ player, tiles } as model) =
  if isCollisionning tiles player.position then
    model
  else
    toFloat player.position.y * 0.9
    |> round
    |> toNearest64
    |> setYIn player.position
    |> setPositionIn player
    |> setPlayerIn model

toNearest64 : Int -> Int
toNearest64 number =
  List.range 1 rowsNumber
  |> List.map ((*) 64)
  |> List.map (\value -> if moreOrLess 3 value number then Just value else Nothing)
  |> List.foldr keepJustValue number

keepJustValue : Maybe Int -> Int -> Int
keepJustValue may acc =
  case may of
    Nothing -> acc
    Just value -> value

moreOrLess : Int -> Int -> Int -> Bool
moreOrLess variance reference comparable =
  if reference - variance <= comparable && comparable <= reference + variance then
    True
  else
    False

isCollisionning : List Tile -> Position -> Bool
isCollisionning tiles position =
  -- Tile between tile.column * tileSize and tile.column + 1 * tileSize
  -- Tile between tile.row * tileSize and tile.row + 1 * tileSize
  List.map (isCollisionningWithOne position) tiles
  |> List.foldr isOneTrue False

isOneTrue : Bool -> Bool -> Bool
isOneTrue value acc =
  acc || value

isCollisionningWithOne : Position -> Tile -> Bool
isCollisionningWithOne { x, y } { column, row } =
  if y == row * tileSize then
    if ((column - 1) * tileSize <= x + playerMargin && x + playerMargin + halfTile <= column * tileSize) || ((column - 1) * tileSize <= x + playerMargin + halfTile && x + playerMargin + tileSize <= column * tileSize) then
      True
    else
      False
  else
    False

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onResize ResizeWindow
    , if model.player.position.y > 5 then Browser.Events.onAnimationFrame AnimationFrame else Sub.none
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
      , playerView model.player.position
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
    , Attributes.style "grid-row-start" (String.fromInt (invertRow row))
    ]
    []

invertRow : Int -> Int
invertRow row =
  (rowsNumber - row) + 1

playerView : Position -> Html Msg
playerView { x, y } =
  Html.img
    [ Attributes.src "/assets/Characters/platformChar_idle.png"
    , Attributes.style "position" "absolute"
    , Attributes.style "bottom" (toPx y)
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
