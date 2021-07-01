module CellParser2 exposing (..)

{-
   ## Types

   @docs Cell, Formula, Value, Row, Col


   ## Functions

   @docs parse, render

-}

import Cell exposing (..)
import Either exposing (Either(..))
import Parser exposing ((|.), (|=), Parser)
import Utility
import UtilityParser as U
import XString


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
parse : String -> Cell
parse input =
    case Parser.run cellParser input of
        Ok cell ->
            cell

        Err _ ->
            Right Undefined


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


cellParser : Parser Cell
cellParser =
    Parser.oneOf [ Parser.backtrackable opParser |> Parser.map Left, valueParser |> Parser.map Right ]


opParser =
    Parser.oneOf [ rowOpParser, colOpParser ]


rowOpParser : Parser Formula
rowOpParser =
    Parser.succeed RowOp
        |. Parser.symbol "row"
        |. Parser.spaces
        |= (string |> Parser.map opFromString)
        -- XString.oneCharWithPredicate (\c -> c == '+' || c == '*' || c == '-' || c == '/')
        |. Parser.spaces
        |= (Parser.int |> Parser.map (\x -> x - 1))
        |. Parser.spaces
        |= (Parser.int |> Parser.map (\x -> x - 1))


colOpParser : Parser Formula
colOpParser =
    Parser.succeed ColOp
        |. Parser.symbol "col"
        |. Parser.spaces
        |= (string |> Parser.map opFromString)
        |. Parser.spaces
        |= (Parser.int |> Parser.map (\x -> x - 1))
        |. Parser.spaces
        |= (Parser.int |> Parser.map (\x -> x - 1))


valueParser : Parser Value
valueParser =
    Parser.oneOf [ Parser.backtrackable (U.integer |> Parser.map Integer), U.float |> Parser.map Real ]


string : Parser String
string =
    XString.withPredicates (\c -> Char.isAlpha c || isSymbol c) Char.isAlpha


isSymbol : Char -> Bool
isSymbol str =
    List.member str [ '+', '-', '*', '/' ]
