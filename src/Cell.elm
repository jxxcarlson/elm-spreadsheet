module Cell exposing
    ( Cell, Formula(..), Value(..), Index, Op(..), Operands(..), RawOperands
    , render, isValue, mapReal, opFromString, realValue, stringFromOp
    )

{-| Cell specifies the kind content of Spreadsheet cells may have.


## Types

@docs Cell, Formula, Value, Index, Op, Operands, RawOperands


## Functions

@docs render, isValue, mapReal, opFromString, realValue, stringFromOp

-}

import Either exposing (Either(..))
import Utility


{-| -}
type alias Cell =
    Either Formula Value


{-| Examples:

    add A2,H10  -- add the two cells
    add A2:H2   -- add the cells in column 2 from row A through row H
    add B4:B9   -- add the cells in row B from column 2 through column 9
    add A2:C9   -- add the cells in the region with corners A2 and C9

    mul A1:H1 A3:H3 -- take the dot product of the column vectors A1:H1 and A3:H3
    mul

-}
type Formula
    = Formula Op Operands


{-| -}
type Operands
    = Pair RawOperands
    | Range RawOperands


{-| -}
type alias RawOperands =
    { left : Index, right : Index }


{-| -}
type alias Index =
    { row : Int, col : Int }


{-| -}
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


{-| -}
isValue : Cell -> Bool
isValue cell =
    case cell of
        Left _ ->
            False

        Right Undefined ->
            False

        Right _ ->
            True


{-| -}
mapReal : (Float -> Float) -> Cell -> Cell
mapReal f cell =
    case cell of
        Right (Real x) ->
            Right (Real (f x))

        _ ->
            cell


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
realValue : Cell -> Maybe Float
realValue cell =
    case cell of
        Left _ ->
            Nothing

        Right (Real x) ->
            Just x

        _ ->
            Nothing


{-| -}
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


{-| -}
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
