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
    = ColumnOp String Int Int


type Value
    = Integer Int
    | Real Float
    | Boolean Bool
    | String String
    | Undefined


parse : String -> Maybe Cell
parse input =
    case Parser.run cellParser input of
        Ok cell ->
            Just cell

        Err _ ->
            Nothing


render : Cell -> String
render cell =
    case cell of
        Left (ColumnOp op i j) ->
            op ++ " " ++ String.fromInt i ++ " " ++ String.fromInt j

        Right (Integer k) ->
            String.fromInt k

        Right (Real x) ->
            String.fromFloat x

        _ ->
            "undefined"


cellParser : Parser Cell
cellParser =
    Parser.oneOf [ Parser.backtrackable opParser |> Parser.map Left, valueParser |> Parser.map Right ]


opParser : Parser Formula
opParser =
    Parser.succeed ColumnOp
        |= XString.oneCharWithPredicate (\c -> c == '+' || c == '*' || c == '-' || c == '/')
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= Parser.int


valueParser : Parser Value
valueParser =
    Parser.oneOf [ Parser.backtrackable (U.integer |> Parser.map Integer), U.float |> Parser.map Real ]
