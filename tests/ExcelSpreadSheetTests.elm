module ExcelSpreadSheetTests exposing (..)

import Array2D
import Cell exposing (..)
import CellParserExcel
import Either exposing (Either(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser
import Spreadsheet exposing (..)
import Test exposing (..)
import Utility


readFromList_ =
    Spreadsheet.readFromList CellParserExcel.parse


read_ =
    Spreadsheet.read CellParserExcel.parse


suite : Test
suite =
    describe "the Spreadsheet package"
        [ describe "The Spreadsheet module"
            [ test "Parse column formula" <|
                \_ ->
                    ss1
                        |> readFromList_
                        |> getCell 3 1
                        |> Expect.equal
                            (Just (Left (Formula Add (Range { left = { col = 1, row = 0 }, right = { col = 1, row = 2 } }))))
            , test "test evalFormula" <|
                let
                    formula =
                        Formula Add (Pair { left = { col = 1, row = 0 }, right = { col = 1, row = 1 } })
                in
                \_ ->
                    ss1
                        |> readFromList_
                        |> evalFormula 3 1 formula
                        |> getCell 3 1
                        |> Expect.equal
                            (Just (Right (Real 2.5)))
            , test "evalSheet (1): is add computation correct?" <|
                \_ ->
                    s2
                        |> readFromList_
                        |> eval
                        |> getCell 3 1
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 3.1)))
            , test "evalSheet (2): is add computation correct?" <|
                \_ ->
                    s3
                        |> readFromList_
                        |> eval
                        |> getCell 3 1
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 3.1)))
            , test "evalSheet (3): is add computation correct?" <|
                \_ ->
                    s4
                        |> readFromList_
                        |> eval
                        |> getCell 3 1
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 21.7)))
            , test "evalSheet (4): is add computation correct?" <|
                \_ ->
                    s5
                        |> readFromList_
                        |> eval
                        |> getCell 3 1
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 90.0)))
            , test "evalSheet (5): is add computation correct?" <|
                \_ ->
                    s5
                        |> readFromList_
                        |> eval
                        |> getCell 1 1
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 92.0)))
            , test "evalSheet (6): is add computation correct?" <|
                \_ ->
                    s5
                        |> readFromList_
                        |> eval
                        |> getCell 1 1
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 92.0)))
            , test "evalSheet (7): is the spreadsheet evaluated?" <|
                \_ ->
                    s5
                        |> readFromList_
                        |> eval
                        |> Spreadsheet.isEvaluated
                        |> Expect.equal True

            -- , skip <| test "evalSheet (8): is the spreadsheet evaluated?" <|
            --     \_ ->
            --         s5
            --             |> readFromList_
            --             |> eval
            --             |> Spreadsheet.isEvaluated                                                                                        |> Maybe.map Spreadsheet.isEvaluated
            --             |> Expect.equal True
            , test "eval (9): is the spreadsheet evaluated?" <|
                \_ ->
                    s5
                        |> readFromList_
                        |> eval
                        |> Spreadsheet.isEvaluated
                        |> Expect.equal True
            , test "Column formula: is addRange computation correct?" <|
                \_ ->
                    s6
                        |> readFromList_
                        |> eval
                        |> getCell 3 1
                        |> Expect.equal (Just (Right (Real 15.0)))
            , test "Row formula: is add computation correct?" <|
                \_ ->
                    s7
                        |> readFromList_
                        |> eval
                        |> getCell 2 3
                        |> Expect.equal (Just (Right (Real 24.0)))
            , test "Is spreadsheet correctly evaluated?" <|
                \_ ->
                    s8
                        |> readFromList_
                        |> eval
                        |> getCell 3 2
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 404.0)))
            , test "Is spreadsheet correctly evaluated???" <|
                \_ ->
                    s8b
                        |> read_
                        |> eval
                        |> getCell 3 1
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 3.4)))
            , test "Is the pipeline OK?" <|
                \_ ->
                    s8b
                        |> read_
                        |> eval
                        |> print
                        |> Expect.equal "100; 1.1; 110\n120; 1.4; 168\n140; 0.9; 126\n-; 3.4; 404"
            , test "Cell.parse on formula" <|
                \_ ->
                    CellParserExcel.parse "=sum(A2:Z2)" |> Expect.equal (Left (Formula Add (Range { left = { col = 1, row = 0 }, right = { col = 1, row = 25 } })))
            , test "Cell.parse on decimal number" <|
                \_ ->
                    CellParserExcel.parse "1.2" |> Expect.equal (Right (Real 1.2))
            , test "Cell.parse on whole number" <|
                \_ ->
                    CellParserExcel.parse "3" |> Expect.equal (Right (Real 3))
            , test "Cell2.parseIndex" <|
                \_ ->
                    Parser.run CellParserExcel.indexParser "z2" |> Expect.equal (Ok { col = 1, row = 25 })
            , test "Cell2.parseIndex,  2 digits" <|
                \_ ->
                    Parser.run CellParserExcel.indexParser "aa2" |> Expect.equal (Ok { col = 1, row = 26 })
            ]
        ]



-- DAT


ss1 =
    [ [ "100.0", "1.1", "a" ]
    , [ "120.0", "1.4", "b" ]
    , [ "140.0", "0.9", "c" ]
    , [ "-", "=sum(A2:C2)", "d" ]
    ]


s2 =
    [ [ "100.0", "1.7", "a" ]
    , [ "120.0", "1.4", "b" ]
    , [ "140.0", "0.9", "c" ]
    , [ "-", "=A2+B2", "d" ]
    ]


s3 =
    [ [ "100.0", "1.7", "a" ]
    , [ "120.0", "1.4", "b" ]
    , [ "140.0", "0.9", "c" ]
    , [ "-", "=A2+B2", "d" ]
    ]


s4 =
    [ [ "100.0", "1.7", "a" ]
    , [ "120.0", "=C1-B1 ", "b" ]
    , [ "140.0", "0.9", "c" ]
    , [ "-", "=A2+B2", "d" ]
    ]


s5 =
    [ [ "1.0", "2.0", "3.0" ]
    , [ "4.0", "=A2+D2", "5.0" ]
    , [ "6.0", "7.0", "8.0" ]
    , [ "9.0", "=D1*D3", "10.0" ]
    ]


s6 =
    [ [ "1.0", "2.0", "3.0" ]
    , [ "11.0", "5.0", "6.0" ]
    , [ "7.0", "8.0", "9.0" ]
    , [ "1.0", "=sum(A2:C2)", "2.0" ]
    ]


s7 =
    [ [ "1.0", "2.0", "3.0", "4.0" ]
    , [ "4.0", "5.0", "6.0", "7.0" ]
    , [ "7.0", "8.0", "9.0", "=sum(C1:C3)" ]
    , [ "1.0", "1", "2.0", "3.0" ]
    ]


s8 =
    [ [ "100.0", "1.1", "=A1*A2" ]
    , [ "120.0", "1.4", "=B1*B2" ]
    , [ "140.0", "0.9", "=C1*C2" ]
    , [ "-", "=sum(A2:C2)", "=sum(A3:C3)" ]
    ]


s8b =
    """

100; 1.1; =A1*A2
120; 1.4; =B1*B2
140; 0.9; =C1*C2
-;   =sum(A2:C2); =sum(A3:C3)

"""
