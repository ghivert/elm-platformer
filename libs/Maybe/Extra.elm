module Maybe.Extra exposing (..)

isJust : Maybe a -> Bool
isJust value =
  case value of
    Just _ -> True
    Nothing -> False

isNothing : Maybe a -> Bool
isNothing value =
  case value of
    Just _ -> False
    Nothing -> True
