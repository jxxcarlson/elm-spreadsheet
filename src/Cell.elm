module Cell exposing
    ( Cell, Formula(..), Value(..), Row, Col
    , render
    , Op(..), realValue, stringFromOp
    )

{-| Cell specifies the kind content of Spreadsheet cells may have.


## Types

@docs Cell, Formula, Value, Row, Col


## Functions

@docs parse, render

-}

import Either exposing (Either(..))
import Parser exposing ((|.), (|=), Parser)
import Utility


{-| -}
type alias Cell =
    Either Formula Value


{-| -}
type alias Col =
    Int


{-| -}
type alias Row =
    Int


{-| -}
type Formula
    = RowOp Op Col Col
    | ColOp Op Row Row


type Op
    = NoOp
    | Add
    | AddRange


{-| -}
type Value
    = Integer Int
    | Real Float
    | Boolean Bool
    | String String
    | Undefined


realValue : Cell -> Maybe Float
realValue cell =
    case cell of
        Left _ ->
            Nothing

        Right (Real x) ->
            Just x

        _ ->
            Nothing


stringFromOp : Op -> String
stringFromOp op =
    case op of
        Add ->
            "Add"

        AddRange ->
            "AddRange"

        NoOp ->
            "NoOp"


opFromString : String -> Op
opFromString str =
    case str of
        "add" ->
            Add

        "addRange" ->
            AddRange

        _ ->
            NoOp


{-| -}
render : Cell -> String
render cell =
    case cell of
        Left (RowOp op i j) ->
            "row " ++ stringFromOp op ++ " " ++ String.fromInt (i + 1) ++ " " ++ String.fromInt (j + 1)

        Left (ColOp op i j) ->
            "col " ++ stringFromOp op ++ " " ++ String.fromInt (i + 1) ++ " " ++ String.fromInt (j + 1)

        Right (Integer k) ->
            String.fromInt k

        Right (Real x) ->
            x |> Utility.roundTo 2 |> String.fromFloat

        Right Undefined ->
            "-"

        Right (Boolean b) ->
            stringOfBoolean b

        Right (String s) ->
            s


stringOfBoolean : Bool -> String
stringOfBoolean b =
    if b then
        "True"

    else
        "False"
