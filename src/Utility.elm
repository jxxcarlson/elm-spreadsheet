module Utility exposing (roundTo)


roundTo : Int -> Float -> Float
roundTo k x =
    let
        factor =
            10.0 ^ toFloat k
    in
    toFloat (round (factor * x)) / factor
