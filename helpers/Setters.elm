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
