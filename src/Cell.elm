module Cell exposing (Cell, Formula(..), Value(..), parse, render)

{- (Cell, Formula(..), Value(..)) -}

import Either exposing (Either(..))
import Parser exposing (Parser)
import ParserTools as T
import UtilityParser as U
import XString


type alias Cell =
    Either Formula Value


type Formula
    = OpSymbol String


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
        Left (OpSymbol op) ->
            op

        Right (Integer k) ->
            String.fromInt k

        Right (Real x) ->
            String.fromFloat x

        _ ->
            "undefined"


cellParser : Parser Cell
cellParser =
    Parser.oneOf [ Parser.backtrackable opParser |> Parser.map (OpSymbol >> Left), valueParser |> Parser.map Right ]


opParser : Parser String
opParser =
    T.first (XString.oneCharWithPredicate (\c -> c == '+' || c == '*' || c == '-' || c == '/')) Parser.end


valueParser : Parser Value
valueParser =
    Parser.oneOf [ Parser.backtrackable (U.integer |> Parser.map Integer), U.float |> Parser.map Real ]
