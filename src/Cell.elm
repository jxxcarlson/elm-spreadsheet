module Cell exposing
    ( Cell, Formula(..), Value(..), Row, Col
    , render
    , Index, Op(..), Operands(..), RawOperands, mapReal, opFromString, realValue, stringFromOp
    )

{-| Cell specifies the kind content of Spreadsheet cells may have.


## Types

@docs Cell, Formula, Value, Row, Col


## Functions

@docs parse, render

-}

import Either exposing (Either(..))
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


type alias Index =
    { row : Int, col : Int }


type alias RawOperands =
    { left : Index, right : Index }


mapReal : (Float -> Float) -> Cell -> Cell
mapReal f cell =
    case cell of
        Right (Real x) ->
            Right (Real (f x))

        _ ->
            cell


type Operands
    = Pair RawOperands
    | Range RawOperands


stringFromOperands operands =
    case operands of
        Pair { left, right } ->
            stringFromIndex left ++ " " ++ stringFromIndex right

        Range { left, right } ->
            stringFromIndex left ++ ":" ++ stringFromIndex right


stringFromIndex : Index -> String
stringFromIndex { row, col } =
    "{ row = " ++ String.fromInt row ++ ", col = " ++ String.fromInt col ++ "}"


{-| -}
type Formula
    = Formula Op Operands


type Op
    = NoOp
    | Add
    | Sub
    | Mul
    | Div


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
            "add"

        Sub ->
            "sub"

        Mul ->
            "mul"

        Div ->
            "div"

        NoOp ->
            "NoOp"


opFromString : String -> Op
opFromString str =
    case str of
        "add" ->
            Add

        "sub" ->
            Sub

        "mul" ->
            Mul

        "div" ->
            Div

        _ ->
            NoOp


{-| -}
render : Cell -> String
render cell =
    case cell of
        Left (Formula op operands) ->
            "row " ++ stringFromOp op ++ " " ++ stringFromOperands operands

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
