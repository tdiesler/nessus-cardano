
module Cardano.PlutusExample.Scratch where

import Prelude

match :: [Integer] -> [Integer] -> Bool
match datum numbers = case datum of
  [low,high] -> foldl (\b x -> b && low <= x && x < high) True numbers
  _          -> False
