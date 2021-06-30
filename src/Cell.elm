module Cell exposing
    ( Cell, Formula(..), Value(..), Row, Col, Location
    , parse, render
    )

{-| Cell specifies the kind content of Spreadsheet cells may have.


## Types

@docs Cell, Formula, Value, Row, Col


## Functions

@docs parse, render

-}

import Either exposing (Either(..))
import Parser exposing ((|.), (|=), Parser)
import ParserTools as T
import Utility
import UtilityParser as U
import XString


{-| -}
type alias Cell =
    Either Formula Value


{-| -}
type alias Col =
    Int


{-| -}
type alias Row =
    Int

type alias Location = (Int, Int)


{-| -}
type Formula
    = RowOp String Col Col
    | ColOp String Row Row


{-| -}
type Value
    = Integer Int
    | Real Float
    | Boolean Bool
    | String String
    | Undefined


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
            "row " ++ op ++ " " ++ String.fromInt (i + 1) ++ " " ++ String.fromInt (j + 1)

        Left (ColOp op i j) ->
            "col " ++ op ++ " " ++ String.fromInt (i + 1) ++ " " ++ String.fromInt (j + 1)

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
        |= string
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
        |= string
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
