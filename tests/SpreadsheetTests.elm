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
            , test "evalSheet (1): is add computation correct?" <|
                \_ ->
                    ss1a
                        |> spreadSheetFromListList
                        |> Maybe.map evalSheet
                        |> Maybe.andThen (getCell 3 1)
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 3.1)))
            , test "evalSheet (2): is add computation correct?" <|
                \_ ->
                    ss1c
                        |> spreadSheetFromListList
                        |> Maybe.map evalSheet
                        |> Maybe.andThen (getCell 3 1)
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 3.1)))
            , test "evalSheet (3): is add computation correct?" <|
                \_ ->
                    ss1d
                        |> spreadSheetFromListList
                        |> Maybe.map evalSheet
                        |> Maybe.andThen (getCell 3 1)
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 21.7)))
            , test "evalSheet (4): is add computation correct?" <|
                \_ ->
                    ss2
                        |> spreadSheetFromListList
                        |> Maybe.map evalSheet
                        |> Maybe.andThen (getCell 3 1)
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 90.0)))
            , test "evalSheet (5): is add computation correct?" <|
                \_ ->
                    ss2
                        |> spreadSheetFromListList
                        |> Maybe.map evalSheet
                        |> Maybe.andThen (getCell 1 1)
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.notEqual (Just (Right (Real 92.0)))
            , test "evalSheet (6): is add computation correct?" <|
                \_ ->
                    ss2
                        |> spreadSheetFromListList
                        |> Maybe.map evalSheet
                        |> Maybe.map evalSheet
                        |> Maybe.andThen (getCell 1 1)
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 92.0)))

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
    [ "100.0", "1.1", "a" ]


s1a =
    [ "100.0", "1.7", "a" ]


s2 =
    [ "120.0", "1.4", "b" ]


s2d =
    [ "120.0", "sub C1,B1 ", "b" ]


s3 =
    [ "140.0", "0.9", "c" ]


s4 =
    [ "-", "add A2:C2", "d" ]


s4b =
    [ "-", "add A2,C2", "d" ]


s4a =
    [ "-", "add A2,B2", "d" ]


s4c =
    [ "-", "add A2,B2", "d" ]



{-

   ss1:
              1       2   3
          A   "100" "1.1" "a"
          B   "120" "1.4" "b"
          C   "140" "0.9" "c"
          D   "-" "add A2:C2" "d"

      When evaluated, cell D2 = 3.4
-}
{-

   ss1c:
              1       2   3
          A   "100.0" "1.7" "a"
          B   "120.0" "1.4" "b"
          C   "140.0" "0.9" "c"
          D   "-" "add A1,B2" "d"

      When evaluated, cell D2 = 101.4
-}


ss1 =
    [ s1, s2, s3, s4 ]


ss1a =
    [ s1a, s2, s3, s4a ]


ss1c =
    [ s1a, s2, s3, s4c ]


ss1b =
    [ s1, s2, s3, s4b ]


ss1d =
    [ s1a, s2d, s3, s4a ]


ss2 =
    [ [ "1.0", "2.0", "3.0" ]
    , [ "4.0", "add A2,D2", "5.0" ]
    , [ "6.0", "7.0", "8.0" ]
    , [ "9.0", "mul D1,D3", "10.0" ]
    ]
