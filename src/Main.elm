module Main exposing (..)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Lazy as Html
import Task exposing (Task)
import Update
import Time exposing (Posix)
import Json.Decode as Decode exposing (Decoder)
import Maybe.Extra as Maybe

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
  , jumping : Maybe Float
  , jumpTimer : Maybe Float
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
  | JumpTimer Posix

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

jumpTimerInterval : Float
jumpTimerInterval = 10

jumpTimerIncreasing : Float
jumpTimerIncreasing = 0.003

gravityPower : Float
gravityPower = 0.9

jumpDecceleration : Float
jumpDecceleration = 0.0125

baseJumpForce : Float
baseJumpForce = 1.300

baseJumpTimer : Float
baseJumpTimer = 1000

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
      { position = Position 32 200
      , jumping = Nothing
      , jumpTimer = Nothing
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
    ViewportSize { viewport } ->
      saveViewportIn model viewport.width viewport.height
    ResizeWindow width height ->
      saveViewportIn model (toFloat width) (toFloat height)
    Timer posix ->
      { player | sprite =
        player.sprite
        |> toggleWalk
      }
      |> setPlayerIn model
      |> Update.identity
    JumpTimer _ ->
      { player
        | jumping = Maybe.map ((+) jumpTimerIncreasing) player.jumping
        , jumpTimer = player.jumpTimer
          |> Maybe.andThen
            (\timer ->
              if timer - jumpTimerInterval == 0 then
                Nothing
              else
                Just (timer - jumpTimerInterval)
            )
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
      |> applyJump
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
    if Maybe.isJust player.jumping then
      Jump
    else
      case List.head keyPressed of
        Just KeyboardLeft -> activateWalk Left player.sprite
        Just KeyboardRight -> activateWalk Right player.sprite
        Just KeyboardUp -> player.sprite
        _ -> Idle
  }
  |> setPlayerIn model

activateWalk : Direction -> Sprite -> Sprite
activateWalk direction sprite =
  case direction of
    Left -> case sprite of
      Idle -> FirstLeft
      FirstRight -> FirstLeft
      SecondRight -> SecondLeft
      others -> others
    Right -> case sprite of
      Idle -> FirstRight
      FirstLeft -> FirstRight
      SecondLeft -> SecondRight
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
        KeyboardSpace ->
          if Maybe.isJust player.jumping then
            model
          else
            { player
              | jumping = Just baseJumpForce
              , jumpTimer = Just baseJumpTimer
              , sprite = Jump
            }
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
        KeyboardSpace ->
          { player | jumpTimer = Nothing }
          |> setPlayerIn model
        others ->
          keyPressed
          |> List.filter ((/=) key)
          |> setKeyPressedIn model

applySpeed : Model -> Model
applySpeed ({ player, keyPressed, tiles, position } as model) =
  case List.head keyPressed of
    Just KeyboardLeft ->
      if isHorizontalCollisionning Left model then
        model
      else
        increaseOrDecreaseSpeed Left (-) (+) model
    Just KeyboardRight ->
      if isHorizontalCollisionning Right model then
        model
      else
        increaseOrDecreaseSpeed Right (+) (-) model
    _ ->
      model

increaseOrDecreaseSpeed
   : Direction
  -> (Int -> Int -> Int)
  -> (Int -> Int -> Int)
  -> Model
  -> Model
increaseOrDecreaseSpeed direction playerOperator worldOperator model =
  let speed = if isWalking model.player then walkSpeed else runSpeed in
  if isNeededToMoveWorld direction model then
    worldOperator model.position speed
    |> setPositionIn model
  else
    playerOperator model.player.position.x speed
    |> toNearest64
    |> updatePlayerPositionX model model.player

isNeededToMoveWorld : Direction -> Model -> Bool
isNeededToMoveWorld direction ({ player } as model)=
  isNeededToMoveWorldRight direction model
  || isNeededToMoveWorldLeft direction model

isNeededToMoveWorldRight : Direction -> Model -> Bool
isNeededToMoveWorldRight direction { player, viewport, position } =
  isPlayerOnHalfScreenSize player viewport
    && isRight direction
    && isWorldNotFinished position viewport

isPlayerOnHalfScreenSize : Player -> Viewport -> Bool
isPlayerOnHalfScreenSize { position } { width } =
  position.x + 8 * tileSize >= round width

isWorldNotFinished : Int -> Viewport -> Bool
isWorldNotFinished position { width } =
  (round width) - position + halfTile <= colsNumber * tileSize

isNeededToMoveWorldLeft : Direction -> Model -> Bool
isNeededToMoveWorldLeft direction { position, player } =
  player.position.x <= halfTile && isLeft direction && position < 0

isWalking : Player -> Bool
isWalking = List.isEmpty << .running

type Direction
  = Left
  | Right

isRight : Direction -> Bool
isRight direction =
  case direction of
    Right -> True
    Left -> False

isLeft : Direction -> Bool
isLeft = not << isRight

isHorizontalCollisionning : Direction -> Model -> Bool
isHorizontalCollisionning direction ({ position, tiles, player } as model) =
  let playerSides = computePlayerSides player.position in
  isCollisionningWithTiles direction playerSides player tiles
  || isCollisionningOnLeftSide direction player.position.x position
  || isCollisionningOnRightSide direction (Tuple.second playerSides) position

isCollisionningWithTiles
   : Direction
  -> (Int, Int)
  -> Player
  -> List Tile
  -> Bool
isCollisionningWithTiles direction playerSides { position } tiles =
  tiles
  |> List.map (isHorizontalCollisionningWithOne direction playerSides position)
  |> isOneTrue

computePlayerSides : Position -> (Int, Int)
computePlayerSides { x } =
  let playerLeftSide = x + playerMargin
      playerRightSide = playerLeftSide + tileSize in
  (playerLeftSide, playerRightSide)

isCollisionningOnRightSide : Direction -> Int -> Int -> Bool
isCollisionningOnRightSide direction playerRightSide position =
  case direction of
    Left -> False
    Right ->
      isPlayerOnRightScreenEdge playerRightSide position
      && isBackgroundPositionAtEnd position

isPlayerOnRightScreenEdge : Int -> Int -> Bool
isPlayerOnRightScreenEdge playerRightSide position =
  playerRightSide - position >= (colsNumber - 1) * tileSize

isBackgroundPositionAtEnd : Int -> Bool
isBackgroundPositionAtEnd position =
  position <= colsNumber * tileSize

isCollisionningOnLeftSide : Direction -> Int -> Int -> Bool
isCollisionningOnLeftSide direction playerLeftSide position =
  case direction of
    Right -> False
    Left ->
      isPlayerOnLeftScreenEdge playerLeftSide
      && isBackgroundPositionAtBeginning position

isPlayerOnLeftScreenEdge : Int -> Bool
isPlayerOnLeftScreenEdge playerLeftSide =
  playerLeftSide <= halfTile

isBackgroundPositionAtBeginning : Int -> Bool
isBackgroundPositionAtBeginning position =
  position >= 0

isHorizontalCollisionningWithOne
   : Direction
  -> (Int, Int)
  -> Position
  -> Tile
  -> Bool
isHorizontalCollisionningWithOne direction playerSides { y } { column, row } =
  let (playerLeftSide, playerRightSide) = playerSides in
  case direction of
    Left ->
      if isPlayerOnLeftTileSide playerLeftSide column then
        isSameAltitude row y
      else
        False
    Right ->
      if isPlayerOnRightTileSide playerRightSide column then
        isSameAltitude row y
      else
        False

isPlayerOnLeftTileSide : Int -> Int -> Bool
isPlayerOnLeftTileSide playerLeftSide column =
  playerLeftSide == column * tileSize

isPlayerOnRightTileSide : Int -> Int -> Bool
isPlayerOnRightTileSide playerRightSide column =
  playerRightSide == (column - 1) * tileSize

isSameAltitude : Int -> Int -> Bool
isSameAltitude row y =
  let tileStartAndEnd = computeTileStartAndEnd row in
  isHalfPlayerTopOnTile tileStartAndEnd y
  || isHalfPlayerBottomOnTile tileStartAndEnd y

computeTileStartAndEnd : Int -> (Int, Int)
computeTileStartAndEnd row =
  let tileStart = (row - 1) * tileSize
      tileEnd = row * tileSize in
  (tileStart, tileEnd)

isHalfPlayerTopOnTile : (Int, Int) -> Int -> Bool
isHalfPlayerTopOnTile (tileStart, tileEnd) y =
  tileStart <= y && y + halfTile <= tileEnd

isHalfPlayerBottomOnTile : (Int, Int) -> Int -> Bool
isHalfPlayerBottomOnTile (tileStart, tileEnd) y =
  tileStart <= y + halfTile && y + tileSize <= tileEnd

updatePlayerPositionX : Model -> Player -> Int -> Model
updatePlayerPositionX model player value =
  value
  |> setXIn player.position
  |> setPositionIn player
  |> setPlayerIn model

applyJump : Model -> Model
applyJump ({ player } as model) =
  case player.jumping of
    Just value ->
      toFloat player.position.y * value
      |> round
      |> setYIn player.position
      |> setPositionIn player
      |> setJumping (if value - jumpDecceleration > 1.0 then Just (value - jumpDecceleration) else Just 1.0)
      |> setPlayerIn model
    Nothing ->
      model

applyGravity : Model -> Model
applyGravity ({ player, tiles } as model) =
  if isVerticalCollisionning tiles player.position then
    if Maybe.isJust player.jumping then
      { player | jumping = Nothing, sprite = Idle, jumpTimer = Nothing }
      |> setPlayerIn model
      |> triggerSprite
    else
      model
  else
    toFloat player.position.y * gravityPower
    |> round
    |> toNearest64
    |> setYIn player.position
    |> setPositionIn player
    |> setPlayerIn model

toNearest64 : Int -> Int
toNearest64 number =
  List.range 1 rowsNumber
  |> List.map ((*) 64)
  |> List.map
    (\value -> if moreOrLess 3 value number then Just value else Nothing)
  |> List.foldr keepJustValue number

keepJustValue : Maybe Int -> Int -> Int
keepJustValue may acc =
  case may of
    Nothing -> acc
    Just value -> value

moreOrLess : Int -> Int -> Int -> Bool
moreOrLess variance reference comparable =
  if reference - variance <= comparable
     && comparable <= reference + variance then
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
  tileStart <= playerWithoutMargin
  && playerWithoutMargin + halfTile <= tileEnd

isHalfPlayerRightOnTile : Int -> Int -> Bool
isHalfPlayerRightOnTile column x =
  let playerWithoutMargin = x + playerMargin
      tileStart = (column - 1) * tileSize
      tileEnd = column * tileSize in
  tileStart <= playerWithoutMargin + halfTile
  && playerWithoutMargin + tileSize <= tileEnd

subscriptions : Model -> Sub Msg
subscriptions ({ player } as model) =
  Sub.batch
    [ Browser.Events.onResize ResizeWindow
    , Browser.Events.onAnimationFrame AnimationFrame
    , Sub.map (KeyHandling << KeyDown) (Browser.Events.onKeyDown keyDecoder)
    , Sub.map (KeyHandling << KeyUp) (Browser.Events.onKeyUp keyDecoder)
    , Time.every 100 Timer
    , case player.jumpTimer of
      Nothing -> Sub.none
      Just _ -> Time.every jumpTimerInterval JumpTimer
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
    " " -> Decode.succeed KeyboardSpace
    " " -> Decode.succeed KeyboardSpace
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
      [ grid model
      , playerView model.player
      ]
    ]

grid : Model -> Html Msg
grid { position, tiles } =
  Html.div
    [ Attributes.style "position" "absolute"
    , Attributes.style "bottom" "0"
    , Attributes.style "left" (toPx position)
    ]
    [ Html.lazy gridContent tiles ]

gridContent : List Tile -> Html Msg
gridContent tiles =
  Html.div
    [ Attributes.style "display" "grid"
    , Attributes.style "min-height" "100%"
    , Attributes.style "grid-template-rows" (cssRepeat rowsNumber tileSize)
    , Attributes.style "grid-template-columns" (cssRepeat colsNumber tileSize)
    ]
    (List.map (Html.lazy tileView) tiles)

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
