module SpreadsheetTests exposing (..)

import Array2D
import Cell exposing (..)
import CellParser
import Either exposing (Either(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser
import Spreadsheet exposing (..)
import Test exposing (..)
import Utility


suite : Test
suite =
    describe "the Spreadsheet package"
        [ describe "The Spreadsheet module"
            [ test "conversion to Array2D String" <|
                \_ ->
                    ss1
                        |> textSpreadSheetFromListList
                        |> Maybe.andThen (Array2D.get 3 1)
                        |> Expect.equal (Just "add A2:C2")
            , test "Parse column formula" <|
                \_ ->
                    ss1
                        |> spreadSheetFromListList
                        |> Maybe.andThen (getCell 3 1)
                        |> Expect.equal
                            (Just (Left (Formula Add (Range { left = { col = 1, row = 0 }, right = { col = 1, row = 2 } }))))
            , test "test evalFormula" <|
                let
                    formula =
                        Formula Add (Pair { left = { col = 1, row = 0 }, right = { col = 1, row = 1 } })
                in
                \_ ->
                    ss1
                        |> spreadSheetFromListList
                        |> Maybe.map (evalFormula 3 1 formula)
                        |> Maybe.andThen (getCell 3 1)
                        |> Expect.equal
                            (Just (Right (Real 2.5)))
            , test "test evalCell" <|
                let
                    cell =
                        Left <| Formula Add (Pair { left = { col = 1, row = 0 }, right = { col = 1, row = 1 } })
                in
                \_ ->
                    ss1
                        |> spreadSheetFromListList
                        |> Maybe.map (evalCell 3 1 cell)
                        |> Maybe.andThen (getCell 3 1)
                        |> Expect.equal
                            (Just (Right (Real 2.5)))
            , test "Column formula: is add computation correct?" <|
                \_ ->
                    ss1a
                        |> spreadSheetFromListList
                        |> Maybe.map evalSheet
                        |> Maybe.andThen (getCell 3 1)
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 3.1)))

            --, test "Column formula: is addRange computation correct?" <|
            --    \_ ->
            --        ss1
            --            |> spreadSheetFromListList
            --            |> Maybe.map eval
            --            |> Maybe.andThen (getCell 3 1)
            --            |> Expect.equal (Just (Right (Real 3.4)))
            --, test "Column formula: is add computation correct?" <|
            --    \_ ->
            --        ss1b
            --            |> spreadSheetFromListList
            --            |> Maybe.map eval
            --            |> Maybe.andThen (getCell 3 1)
            --            |> Expect.equal (Just (Right (Real 2.5)))
            , test "Cell.parse on formula" <|
                \_ ->
                    Parser.run CellParser.cellParser "add A2:Z2" |> Expect.equal (Ok (Left (Formula Add (Range { left = { col = 1, row = 0 }, right = { col = 1, row = 25 } }))))
            , test "Cell.parse on decimal number" <|
                \_ ->
                    Parser.run CellParser.cellParser "1.2" |> Expect.equal (Ok (Right (Real 1.2)))
            , test "Cell.parse on whole number" <|
                \_ ->
                    Parser.run CellParser.cellParser "3" |> Expect.equal (Ok (Right (Integer 3)))
            , test "Cell2.parseIndex" <|
                \_ ->
                    Parser.run CellParser.indexParser "z2" |> Expect.equal (Ok { col = 1, row = 25 })
            , test "Cell2.parseIndex,  2 digits" <|
                \_ ->
                    Parser.run CellParser.indexParser "aa2" |> Expect.equal (Ok { col = 1, row = 26 })
            ]
        ]



-- DATA


s1 =
    [ "100", "1.1", "a" ]


s1a =
    [ "100", "1.7", "a" ]


s2 =
    [ "120", "1.4", "b" ]


s3 =
    [ "140", "0.9", "c" ]


s4 =
    [ "-", "add A2:C2", "d" ]


s4b =
    [ "-", "add A2,C2", "d" ]


s4a =
    [ "-", "add A2,B2", "d" ]



{-
           1       2   3
       A   "100" "1.1" "a"
       B   "120" "1.4" "b"
       C   "140" "0.9" "c"
       D   "-" "add A2:C2" "d"

   When evaluated, cell D2 = 3.4
-}


ss1 =
    [ s1, s2, s3, s4 ]


ss1a =
    [ s1a, s2, s3, s4a ]


ss1b =
    [ s1, s2, s3, s4b ]
