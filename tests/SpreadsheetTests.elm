module SpreadsheetTests exposing (..)

import Array2D
import Cell exposing (..)
import CellParser1
import CellParser2
import Either exposing (Either(..))
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser
import Spreadsheet exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "the Spreadsheet package"
        [ describe "The Spreadsheet module"
            [ test "conversion to Array2D String" <|
                \_ ->
                    ss1
                        |> textSpreadSheetFromListList
                        |> Maybe.andThen (Array2D.get 3 1)
                        |> Expect.equal (Just "col addRange 1 3")
            , test "Parse column formula" <|
                \_ ->
                    ss1
                        |> spreadSheetFromListList
                        |> Maybe.andThen (getCell 3 1)
                        |> Expect.equal (Just (Left (ColOp AddRange 0 2)))
            , test "Column formula: is addRange computation correct?" <|
                \_ ->
                    ss1
                        |> spreadSheetFromListList
                        |> Maybe.map eval
                        |> Maybe.andThen (getCell 3 1)
                        |> Expect.equal (Just (Right (Real 3.4)))
            , test "Column formula: is add computation correct?" <|
                \_ ->
                    ss1b
                        |> spreadSheetFromListList
                        |> Maybe.map eval
                        |> Maybe.andThen (getCell 3 1)
                        |> Expect.equal (Just (Right (Real 2.5)))
            , test "Cell.parse on formula" <|
                \_ ->
                    CellParser1.parse "col addRange 1 2" |> Expect.equal (Left (ColOp AddRange 0 1))
            , test "Cell.parse on decimal number" <|
                \_ ->
                    CellParser1.parse "1.2" |> Expect.equal (Right (Real 1.2))
            , test "Cell.parse on whole number" <|
                \_ ->
                    CellParser1.parse "3" |> Expect.equal (Right (Integer 3))
            , test "Cell2.parseIndex" <|
                \_ ->
                    Parser.run CellParser2.indexParser "z2" |> Expect.equal (Ok { col = 1, row = 25 })
            , test "Cell2.parseIndex,  2 digits" <|
                \_ ->
                    Parser.run CellParser2.indexParser "aa2" |> Expect.equal (Ok { col = 1, row = 26 })
            ]
        ]



-- DATA


s1 =
    [ "100", "1.1", "a" ]


s2 =
    [ "120", "1.4", "b" ]


s3 =
    [ "140", "0.9", "c" ]


s4 =
    [ "-", "col addRange 1 3", "d" ]


s4b =
    [ "-", "col add 1 2", "d" ]



{-
           1       2   3
       A   "100" "1.1" "a"
       B   "120" "1.4" "b"
       C   "140" "0.9" "c"
       D   "-" "col addRange 1 3" "d"

   When evaluated, cell D2 = 3.4
-}


ss1 =
    [ s1, s2, s3, s4 ]


ss1b =
    [ s1, s2, s3, s4b ]
