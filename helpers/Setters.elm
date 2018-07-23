module Setters exposing (..)

setViewport
   : a
  -> { b | viewport : a }
  -> { b | viewport: a }
setViewport v o =
    { o | viewport = v }

setViewportIn
   : { b | viewport : a }
  -> a
  -> { b | viewport : a }
setViewportIn o v =
    { o | viewport = v }

setPlayer
   : a
  -> { b | player : a }
  -> { b | player : a }
setPlayer v o =
  { o | player = v }

setPlayerIn
   : { b | player : a }
  -> a
  -> { b | player : a }
setPlayerIn o v =
  { o | player = v }

setPosition
   : a
  -> { b | position : a }
  -> { b | position : a }
setPosition v o =
  { o | position = v }

setPositionIn
   : { b | position : a }
  -> a
  -> { b | position : a }
setPositionIn o v =
  { o | position = v }

setKeyPressed
   : a
  -> { b | keyPressed : a }
  -> { b | keyPressed : a }
setKeyPressed v o =
  { o | keyPressed = v }

setKeyPressedIn
   : { b | keyPressed : a }
  -> a
  -> { b | keyPressed : a }
setKeyPressedIn o v =
  { o | keyPressed = v }
setX
   : a
  -> { b | x : a }
  -> { b | x : a }
setX v o =
  { o | x = v }

setXIn
   : { b | x : a }
  -> a
  -> { b | x : a }
setXIn o v =
  { o | x = v }

setY
   : a
  -> { b | y : a }
  -> { b | y : a }
setY v o =
  { o | y = v }

setYIn
   : { b | y : a }
  -> a
  -> { b | y : a }
setYIn o v =
  { o | y = v }
