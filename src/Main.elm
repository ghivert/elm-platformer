module Main exposing (..)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Task exposing (Task)
import Update
import Time exposing (Posix)
import Json.Decode as Decode exposing (Decoder)

import Setters exposing (..)

type alias Model =
  { viewport : Viewport
  , position : Int
  , tiles : List Tile
  , player : Player
  , keyPressed : List KeyboardKey
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

type alias Player =
  { position : Position
  , walking : Maybe Walking
  }

type Walking
  = FirstLeft
  | SecondLeft
  | FirstRight
  | SecondRight
  | Jump

type alias Position =
  { x : Int
  , y : Int
  }

type Msg
  = ViewportSize Dom.Viewport
  | ResizeWindow Int Int
  | AnimationFrame Posix
  | KeyHandling KeyAction
  | Timer Posix

type KeyAction
  = KeyDown KeyboardKey
  | KeyUp KeyboardKey

type KeyboardKey
  = KeyboardUp
  | KeyboardLeft
  | KeyboardRight
  | KeyboardSpace

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
  Model (Viewport 0 0) 0 ground { position = Position 0 200, walking = Nothing } []

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
update msg ({ player } as model) =
  case msg of
    ViewportSize { viewport } -> saveViewportIn model viewport.width viewport.height
    ResizeWindow width height -> saveViewportIn model (toFloat width) (toFloat height)
    Timer posix ->
      { player | walking =
        player.walking
        |> Maybe.map toggleWalk
      }
      |> setPlayerIn model
      |> Update.identity
    KeyHandling keyAction ->
      handleKeyAction keyAction model
      |> triggerSprite
      |> Update.identity
    AnimationFrame time ->
      model
      |> applySpeed
      |> applyGravity
      |> Update.identity

toggleWalk : Walking -> Walking
toggleWalk walking =
  case walking of
    FirstLeft -> SecondLeft
    SecondLeft -> FirstLeft
    FirstRight -> SecondRight
    SecondRight -> FirstRight
    others -> others

triggerSprite : Model -> Model
triggerSprite ({ player, keyPressed } as model) =
  { player | walking =
    case List.head keyPressed of
      Just KeyboardLeft -> activateWalk player.walking FirstLeft
      Just KeyboardRight -> activateWalk player.walking FirstRight
      Just KeyboardUp -> player.walking
      Just KeyboardSpace -> Just Jump
      Nothing -> Nothing
  }
  |> setPlayerIn model

activateWalk : Maybe Walking -> Walking -> Maybe Walking
activateWalk walking value =
  case value of
    FirstLeft ->
      case walking of
        Just FirstLeft -> Just FirstLeft
        Just SecondLeft -> Just SecondLeft
        others -> Just FirstLeft
    FirstRight ->
      case walking of
        Just FirstRight -> Just FirstRight
        Just SecondRight -> Just SecondRight
        others -> Just FirstRight
    others -> Just value

handleKeyAction : KeyAction -> Model -> Model
handleKeyAction keyAction ({ keyPressed } as model) =
  setKeyPressedIn model <|
    case keyAction of
      KeyDown key -> key :: keyPressed
      KeyUp key -> List.filter ((/=) key) keyPressed

applySpeed : Model -> Model
applySpeed ({ player, keyPressed } as model) =
  case List.head keyPressed of
    Just KeyboardLeft ->
      player.position.x - 6
      |> updatePlayerPositionX model player
    Just KeyboardRight ->
      player.position.x + 6
      |> updatePlayerPositionX model player
    _ ->
      model

updatePlayerPositionX : Model -> Player -> Int -> Model
updatePlayerPositionX model player value =
  value
  |> setXIn player.position
  |> setPositionIn player
  |> setPlayerIn model

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
  List.map (isCollisionningWithOne position) tiles
  |> List.foldr isOneTrue False

isOneTrue : Bool -> Bool -> Bool
isOneTrue value acc =
  acc || value

isCollisionningWithOne : Position -> Tile -> Bool
isCollisionningWithOne { x, y } { column, row } =
  if y == row * tileSize then
    isPlayerOnTile column x
  else
    False

isPlayerOnTile : Int -> Int -> Bool
isPlayerOnTile column x =
  isHalfPlayerLeftOnTile column x || isHalfPlayerRightOnTile column x

isHalfPlayerLeftOnTile : Int -> Int -> Bool
isHalfPlayerLeftOnTile column x =
  let playerWithoutMargin = x + playerMargin
      tileStart = (column - 1) * tileSize
      tileEnd = column * tileSize in
  tileStart <= playerWithoutMargin && playerWithoutMargin + halfTile <= tileEnd

isHalfPlayerRightOnTile : Int -> Int -> Bool
isHalfPlayerRightOnTile column x =
  let playerWithoutMargin = x + playerMargin
      tileStart = (column - 1) * tileSize
      tileEnd = column * tileSize in
  tileStart <= playerWithoutMargin + halfTile && playerWithoutMargin + tileSize <= tileEnd

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onResize ResizeWindow
    , Browser.Events.onAnimationFrame AnimationFrame
    , Sub.map (KeyHandling << KeyDown) (Browser.Events.onKeyDown keyDecoder)
    , Sub.map (KeyHandling << KeyUp) (Browser.Events.onKeyUp keyDecoder)
    , Time.every 100 Timer
    ]

keyDecoder : Decoder KeyboardKey
keyDecoder =
  Decode.andThen toKeyboardKey (Decode.field "key" Decode.string)

toKeyboardKey : String -> Decoder KeyboardKey
toKeyboardKey value =
  case value of
    "ArrowLeft" -> Decode.succeed KeyboardLeft
    "ArrowRight" -> Decode.succeed KeyboardRight
    val -> Decode.fail val

-- View Functions

view : Model -> Document Msg
view model =
  Document
    "Elm Platformer"
    [ Html.node "style" []
      [ Html.text <| String.join "\n"
        [ "* { box-sizing: border-box; }"
        , "body { margin: 0; overflow: hidden; }"
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
    , Attributes.style "grid-row-start" (String.fromInt (invertRow row))
    ]
    []

invertRow : Int -> Int
invertRow row =
  (rowsNumber - row) + 1

playerView : Player -> Html Msg
playerView { position, walking } =
  Html.img
    [ Attributes.src (chooseCharacterView walking)
    , Attributes.style "position" "absolute"
    , Attributes.style "bottom" (toPx position.y)
    , Attributes.style "left" (toPx position.x)
    , Attributes.style "z-index" "1000"
    ]
    []

chooseCharacterView : Maybe Walking -> String
chooseCharacterView walking =
  case walking of
    Just FirstLeft -> "/assets/Characters/platformChar_walk1_left.png"
    Just SecondLeft -> "/assets/Characters/platformChar_walk2_left.png"
    Just FirstRight -> "/assets/Characters/platformChar_walk1_right.png"
    Just SecondRight -> "/assets/Characters/platformChar_walk2_right.png"
    Just Jump -> "/assets/Characters/platformChar_jump.png"
    Nothing -> "/assets/Characters/platformChar_idle.png"

toPx : Int -> String
toPx position =
  String.fromInt position ++ "px"

cssRepeat : Int -> Int -> String
cssRepeat times size =
  "repeat(" ++ String.fromInt times ++ ", " ++ String.fromInt size ++ "px)"
