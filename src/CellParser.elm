module CellParser exposing (parse, indexParser)

{-|

@docs parse, indexParser

-}

import Cell exposing (..)
import Either exposing (Either(..))
import Parser exposing ((|.), (|=), Parser)
import ParserUtil
import Utility
import UtilityParser as U
import XString


type AnnotatedIndex
    = P Index
    | R Index


type alias AnnotatedOperands =
    { left : Index, right : AnnotatedIndex }


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


formulaParser : Parser Formula
formulaParser =
    Parser.succeed (\op operands -> Formula op operands)
        |= opParser
        |. Parser.spaces
        |= operandsParser


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
    ParserUtil.second (XString.oneCharWithPredicate (\c -> c == ',')) indexParser


trailingOperandParser2 : Parser Index
trailingOperandParser2 =
    ParserUtil.second (XString.oneCharWithPredicate (\c -> c == ':')) indexParser


inverseOrder : Int -> String
inverseOrder k =
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
        inverseOrder q ++ c


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


{-|

    > parse "3.1"
    Right (Real 3.1)

    > parse "add A2,B3"
    Left (Formula Add (Pair { left = { col = 1, row = 0 }, right = { col = 2, row = 1 } }))

    > parse "add A2:A8"
    Left (Formula Add (Range { left = { col = 1, row = 0 }, right = { col = 7, row = 0 } }))mula Add (Pair { left = { col = 1, row = 0 }, right = { col = 7, row = 0 } }))

    > parse "add B3:B9"
    Left (Formula Add (Range { left = { col = 2, row = 1 }, right = { col = 8, row = 1 } }))

-}
parse : String -> Cell
parse input =
    case Parser.run cellParser input of
        Ok cell ->
            cell

        Err _ ->
            Right Undefined


stringOfBoolean : Bool -> String
stringOfBoolean b =
    if b then
        "True"

    else
        "False"


cellParser : Parser Cell
cellParser =
    Parser.oneOf [ formulaParser |> Parser.map Left, valueParser |> Parser.map Right ]


opParser : Parser Op
opParser =
    string |> Parser.map opFromString


valueParser : Parser Value
valueParser =
    Parser.oneOf [ Parser.backtrackable (U.integer |> Parser.map toFloat |> Parser.map Real), U.float |> Parser.map Real ]


string : Parser String
string =
    XString.withPredicates (\c -> Char.isAlpha c || isSymbol c) Char.isAlpha


isSymbol : Char -> Bool
isSymbol str =
    List.member str [ '+', '-', '*', '/' ]
