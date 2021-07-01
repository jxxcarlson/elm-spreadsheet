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
import ParserUtil
import Utility
import UtilityParser as U
import XString


type alias Index =
    { row : Int, col : Int }


type AnnotatedIndex
    = P Index
    | R Index


type alias RawOperands =
    { left : Index, right : Index }


type alias AnnotatedOperands =
    { left : Index, right : AnnotatedIndex }


type Operands
    = Pair RawOperands
    | Range RawOperands



-- formulaParser :


transform : AnnotatedOperands -> Operands
transform { left, right } =
    case right of
        P index ->
            Pair { left = left, right = index }

        R index ->
            Range { left = left, right = index }


{-|

    > run C.parseIndex "a2"
    Ok { col = 1, row = 0 }

-}
indexParser : Parser Index
indexParser =
    Parser.succeed (\i j -> { row = i - 1, col = j - 1 })
        |= (XString.withPredicates Char.isAlpha Char.isAlpha |> Parser.map order)
        |= Parser.int


operandsParser : Parser Operands
operandsParser =
    Parser.succeed (\a b -> transform { left = a, right = b })
        |= indexParser
        |= annotatedIndexParser


annotatedIndexParser : Parser AnnotatedIndex
annotatedIndexParser =
    Parser.oneOf [ trailingOperandParser1 |> Parser.map P, trailingOperandParser2 |> Parser.map R ]


trailingOperandParser1 : Parser Index
trailingOperandParser1 =
    ParserUtil.second (XString.oneCharWithPredicate (\c -> c == ' ')) indexParser


trailingOperandParser2 : Parser Index
trailingOperandParser2 =
    ParserUtil.second (XString.oneCharWithPredicate (\c -> c == ':')) indexParser


order : String -> Int
order str =
    let
        indices =
            str
                |> String.toLower
                |> String.toList
                |> List.map (\c -> Char.toCode c - 96)
                |> List.reverse
    in
    order_ indices


order_ : List Int -> Int
order_ indices =
    case indices of
        [] ->
            0

        first :: [] ->
            first

        first :: rest ->
            first + 26 * order_ rest


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
    Parser.oneOf [ Parser.backtrackable opParser2 |> Parser.map Left, valueParser |> Parser.map Right ]


opParser2 =
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
