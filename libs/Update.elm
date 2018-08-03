module Update exposing (..)

identity : model -> (model, Cmd msg)
identity model =
  (model, Cmd.none)
