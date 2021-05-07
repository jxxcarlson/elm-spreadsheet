module Cell exposing (Cell, Formula(..), Value(..), parse, render)

{- (Cell, Formula(..), Value(..)) -}

import Either exposing (Either(..))
import Parser exposing ((|.), (|=), Parser)
import ParserTools as T
import UtilityParser as U
import XString


type alias Cell =
    Either Formula Value


type Formula
    = RowOp String Int Int
    | ColOp String Int Int


type Value
    = Integer Int
    | Real Float
    | Boolean Bool
    | String String
    | Undefined


parse : String -> Cell
parse input =
    case Parser.run cellParser input of
        Ok cell ->
            cell

        Err _ ->
            Right Undefined


render : Cell -> String
render cell =
    case cell of
        Left (RowOp op i j) ->
            "row " ++ op ++ " " ++ String.fromInt i ++ " " ++ String.fromInt j

        Left (ColOp op i j) ->
            "col " ++ op ++ " " ++ String.fromInt i ++ " " ++ String.fromInt j

        Right (Integer k) ->
            String.fromInt k

        Right (Real x) ->
            String.fromFloat x

        Right Undefined ->
            ""

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
        |= string
        -- XString.oneCharWithPredicate (\c -> c == '+' || c == '*' || c == '-' || c == '/')
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= Parser.int


colOpParser : Parser Formula
colOpParser =
    Parser.succeed ColOp
        |. Parser.symbol "col"
        |. Parser.spaces
        |= string
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= Parser.int


valueParser : Parser Value
valueParser =
    Parser.oneOf [ Parser.backtrackable (U.integer |> Parser.map Integer), U.float |> Parser.map Real ]


string : Parser String
string =
    XString.withPredicates (\c -> Char.isAlpha c || isSymbol c) Char.isAlpha


isSymbol : Char -> Bool
isSymbol str =
    List.member str [ '+', '-', '*', '/' ]
