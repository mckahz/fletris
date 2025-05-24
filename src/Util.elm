module Util exposing (chunksOf)

chunksOf : Int -> List a -> List (List a)
chunksOf size list =
  if List.isEmpty list
    then []
    else List.take size list :: chunksOf size (List.drop size list)
