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
  , sprite : Sprite
  , running : List Bool
  }

type Sprite
  = FirstLeft
  | SecondLeft
  | FirstRight
  | SecondRight
  | Jump
  | Idle

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
  | KeyboardShift

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

walkSpeed : Int
walkSpeed = 6

runSpeed : Int
runSpeed = 12

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
    { viewport = Viewport 0 0
    , position = 0
    , tiles = ground
    , player =
      { position = Position 0 200
      , sprite = Idle
      , running = []
      }
    , keyPressed = []
    }

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
      { player | sprite =
        player.sprite
        |> toggleWalk
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

toggleWalk : Sprite -> Sprite
toggleWalk walking =
  case walking of
    FirstLeft -> SecondLeft
    SecondLeft -> FirstLeft
    FirstRight -> SecondRight
    SecondRight -> FirstRight
    others -> others

triggerSprite : Model -> Model
triggerSprite ({ player, keyPressed } as model) =
  { player | sprite =
    case List.head keyPressed of
      Just KeyboardLeft -> activateWalk Left player.sprite
      Just KeyboardRight -> activateWalk Right player.sprite
      Just KeyboardUp -> player.sprite
      Just KeyboardSpace -> Jump
      _ -> Idle
  }
  |> setPlayerIn model

activateWalk : Direction -> Sprite -> Sprite
activateWalk direction sprite =
  case direction of
    Left -> case sprite of
      Idle -> FirstLeft
      others -> others
    Right -> case sprite of
      Idle -> FirstRight
      others -> others

handleKeyAction : KeyAction -> Model -> Model
handleKeyAction keyAction ({ keyPressed, player } as model) =
  case keyAction of
    KeyDown key ->
      case key of
        KeyboardShift ->
          True :: player.running
          |> setRunningIn player
          |> setPlayerIn model
        others ->
          key :: keyPressed
          |> setKeyPressedIn model
    KeyUp key ->
      case key of
        KeyboardShift ->
          player.running
          |> List.tail
          |> Maybe.withDefault []
          |> setRunningIn player
          |> setPlayerIn model
        others ->
          keyPressed
          |> List.filter ((/=) key)
          |> setKeyPressedIn model

applySpeed : Model -> Model
applySpeed ({ player, keyPressed, tiles } as model) =
  case List.head keyPressed of
    Just KeyboardLeft ->
      if isHorizontalCollisionning Left tiles player.position then
        model
      else
        increaseOrDecreaseSpeed (-) model player
    Just KeyboardRight ->
      if isHorizontalCollisionning Right tiles player.position then
        model
      else
        increaseOrDecreaseSpeed (+) model player
    _ ->
      model

increaseOrDecreaseSpeed : (Int -> Int -> Int) -> Model -> Player -> Model
increaseOrDecreaseSpeed operator model player =
  let speed = if isWalking player then walkSpeed else runSpeed in
  operator player.position.x speed
  |> toNearest64
  |> updatePlayerPositionX model player

isWalking : Player -> Bool
isWalking = List.isEmpty << .running

type Direction
  = Left
  | Right

isHorizontalCollisionning : Direction -> List Tile -> Position -> Bool
isHorizontalCollisionning direction tiles position =
  tiles
  |> List.map (isHorizontalCollisionningWithOne direction position)
  |> isOneTrue

isHorizontalCollisionningWithOne : Direction -> Position -> Tile -> Bool
isHorizontalCollisionningWithOne direction { x, y } { column, row } =
  let playerLeftSide = x + playerMargin
      playerRightSide = playerLeftSide + tileSize in
  case direction of
    Left ->
      if playerLeftSide <= 0 then
        True
      else if playerLeftSide == column * tileSize then
        isSameAltitude row y
      else
        False
    Right ->
      if playerRightSide >= colsNumber * tileSize then
        True
      else if playerRightSide == (column - 1) * tileSize then
        isSameAltitude row y
      else
        False

isSameAltitude : Int -> Int -> Bool
isSameAltitude row y =
  isHalfPlayerTopOnTile row y || isHalfPlayerBottomOnTile row y

isHalfPlayerTopOnTile : Int -> Int -> Bool
isHalfPlayerTopOnTile row y =
  let tileStart = (row - 1) * tileSize
      tileEnd = row * tileSize in
  tileStart <= y && y + halfTile <= tileEnd

isHalfPlayerBottomOnTile : Int -> Int -> Bool
isHalfPlayerBottomOnTile row y =
  let tileStart = (row - 1) * tileSize
      tileEnd = row * tileSize in
  tileStart <= y + halfTile && y + tileSize <= tileEnd

updatePlayerPositionX : Model -> Player -> Int -> Model
updatePlayerPositionX model player value =
  value
  |> setXIn player.position
  |> setPositionIn player
  |> setPlayerIn model

applyGravity : Model -> Model
applyGravity ({ player, tiles } as model) =
  if isVerticalCollisionning tiles player.position then
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

isVerticalCollisionning : List Tile -> Position -> Bool
isVerticalCollisionning tiles position =
  tiles
  |> List.map (isVerticalCollisionningWithOne position)
  |> isOneTrue

isOneTrue : List Bool -> Bool
isOneTrue =
  List.foldr (\value acc -> acc || value) False

isVerticalCollisionningWithOne : Position -> Tile -> Bool
isVerticalCollisionningWithOne { x, y } { column, row } =
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
    "Shift" -> Decode.succeed KeyboardShift
    val -> Decode.fail (Debug.log "val" val)

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
      [ grid model
      , playerView model.player
      ]
    ]

grid : Model -> Html Msg
grid { position, viewport, tiles } =
  Html.div
    [ Attributes.style "display" "grid"
    , Attributes.style "min-height" "100%"
    , Attributes.style "grid-template-rows" (cssRepeat rowsNumber tileSize)
    , Attributes.style "grid-template-columns" (cssRepeat colsNumber tileSize)
    , Attributes.style "position" "absolute"
    , Attributes.style "bottom" "0"
    , Attributes.style "left" (toPx position)
    ]
    (List.map tileView tiles)

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
playerView { position, sprite } =
  Html.img
    [ Attributes.src (chooseCharacterView sprite)
    , Attributes.style "position" "absolute"
    , Attributes.style "bottom" (toPx position.y)
    , Attributes.style "left" (toPx position.x)
    , Attributes.style "z-index" "1000"
    ]
    []

chooseCharacterView : Sprite -> String
chooseCharacterView sprite =
  case sprite of
    FirstLeft -> "/assets/Characters/platformChar_walk1_left.png"
    SecondLeft -> "/assets/Characters/platformChar_walk2_left.png"
    FirstRight -> "/assets/Characters/platformChar_walk1_right.png"
    SecondRight -> "/assets/Characters/platformChar_walk2_right.png"
    Jump -> "/assets/Characters/platformChar_jump.png"
    Idle -> "/assets/Characters/platformChar_idle.png"

toPx : Int -> String
toPx position =
  String.fromInt position ++ "px"

cssRepeat : Int -> Int -> String
cssRepeat times size =
  "repeat(" ++ String.fromInt times ++ ", " ++ String.fromInt size ++ "px)"
