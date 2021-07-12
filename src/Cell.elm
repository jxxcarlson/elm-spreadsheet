module Cell exposing
    ( Cell, Formula(..), Value(..), Index, Op(..), Operands(..), RawOperands
    , render, isValue, mapReal, opFromString, opFromString2, realValue, stringFromOp
    )

{-| Cell specifies the kind content of Spreadsheet cells may have.


## Types

@docs Cell, Formula, Value, Index, Op, Operands, RawOperands


## Functions

@docs render, isValue, mapReal, opFromString, opFromString2, realValue, stringFromOp

-}

import Either exposing (Either(..))
import Utility


{-| Parse the text representation to see the internal representation of a cell:

    > parse "3.1"
    Right (Real 3.1)

-}
type alias Cell =
    Either Formula Value


{-| Examples:

Parse the text representation to see the internal representation of a formula:

    > parse "=A2+B3"
    Left (Formula Add (Pair { left = { col = 1, row = 0 }, right = { col = 2, row = 1 } }))

    > parse "=sum(A2:A8)"
    Left (Formula Add (Range { left = { col = 1, row = 0 }, right = { col = 7, row = 0 } }))

    > parse "=sum(B3:B9)"
    Left (Formula Add (Range { left = { col = 2, row = 1 }, right = { col = 8, row = 1 } }))

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
            stringFromIndex left ++ "," ++ stringFromIndex right

        Range { left, right } ->
            stringFromIndex left ++ ":" ++ stringFromIndex right


stringFromIndex : Index -> String
stringFromIndex { row, col } =
    intToRowCode row ++ (String.trim <| String.fromInt (col + 1))


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

        "sum" ->
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
opFromString2 : String -> Op
opFromString2 str =
    case str of
        "+" ->
            Add

        "-" ->
            Sub

        "*" ->
            Mul

        "/" ->
            Div

        _ ->
            NoOp


{-| -}
render : Cell -> String
render cell =
    case cell of
        Left (Formula op operands) ->
            stringFromOp op ++ " " ++ stringFromOperands operands

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


intToRowCode : Int -> String
intToRowCode k =
    if k < 26 then
        Char.fromCode (k + 65) |> String.fromChar

    else
        let
            r =
                modBy 26 k

            c =
                Char.fromCode (r + 65) |> String.fromChar

            q =
                (k // 26) - 1
        in
        intToRowCode q ++ c


stringOfBoolean : Bool -> String
stringOfBoolean b =
    if b then
        "True"

    else
        "False"
