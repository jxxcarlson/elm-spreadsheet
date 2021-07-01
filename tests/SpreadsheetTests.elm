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
            [ 
             test "Parse column formula" <|
                \_ ->
                    ss1
                        |> readFromList
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
                        |> readFromList
                        |> Maybe.map (evalFormula 3 1 formula)
                        |> Maybe.andThen (getCell 3 1)
                        |> Expect.equal
                            (Just (Right (Real 2.5)))
            , test "evalSheet (1): is add computation correct?" <|
                \_ ->
                    s2
                        |> readFromList
                        |> Maybe.map eval
                        |> Maybe.andThen (getCell 3 1)
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 3.1)))
            , test "evalSheet (2): is add computation correct?" <|
                \_ ->
                    s3
                        |> readFromList
                        |> Maybe.map eval
                        |> Maybe.andThen (getCell 3 1)
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 3.1)))
            , test "evalSheet (3): is add computation correct?" <|
                \_ ->
                    s4
                        |> readFromList
                        |> Maybe.map eval
                        |> Maybe.andThen (getCell 3 1)
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 21.7)))
            , test "evalSheet (4): is add computation correct?" <|
                \_ ->
                    s5
                        |> readFromList
                        |> Maybe.map eval
                        |> Maybe.andThen (getCell 3 1)
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 90.0)))
            , test "evalSheet (5): is add computation correct?" <|
                \_ ->
                    s5
                        |> readFromList
                        |> Maybe.map eval
                        |> Maybe.andThen (getCell 1 1)
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 92.0)))
            , test "evalSheet (6): is add computation correct?" <|
                \_ ->
                    s5
                        |> readFromList
                        |> Maybe.map eval
                        |> Maybe.map eval
                        |> Maybe.andThen (getCell 1 1)
                        |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                        |> Expect.equal (Just (Right (Real 92.0)))
            , test "evalSheet (7): is the spreadsheet evaluated?" <|
                \_ ->
                    s5
                        |> readFromList
                        |> Maybe.map eval
                        |> Maybe.map eval
                        |> Maybe.map Spreadsheet.isEvaluated
                        |> Expect.equal (Just True)
            , test "evalSheet (8): is the spreadsheet evaluated?" <|
                \_ ->
                    s5
                        |> readFromList
                        |> Maybe.map eval
                        |> Maybe.map Spreadsheet.isEvaluated
                        |> Expect.equal (Just True)
            , test "eval (9): is the spreadsheet evaluated?" <|
                \_ ->
                    s5
                        |> readFromList
                        |> Maybe.map eval
                        |> Maybe.map Spreadsheet.isEvaluated
                        |> Expect.equal (Just True)
            , test "Column formula: is addRange computation correct?" <|
                \_ ->
                    s6
                        |> readFromList
                        |> Maybe.map eval
                        |> Maybe.andThen (getCell 3 1)
                        |> Expect.equal (Just (Right (Real 15.0)))
            , test "Row formula: is add computation correct?" <|
                    \_ ->
                        s7
                            |> readFromList
                            |> Maybe.map eval
                            |> Maybe.andThen (getCell 2 3)
                            |> Expect.equal (Just (Right (Real 24.0)))
           ,   test "Is spreadsheet correctly evaluated?" <|
                    \_ ->
                        s8
                            |> readFromList
                            |> Maybe.map eval
                            |> Maybe.andThen (getCell 3 2)
                            |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                            |> Expect.equal (Just (Right (Real 404.0)))   
            , test "Is spreadsheet correctly evaluated???" <|
                    \_ ->
                        s8b
                            |> read
                            |> Maybe.map eval
                            |> Maybe.andThen (getCell 3 1)
                            |> Maybe.map (Cell.mapReal (Utility.roundTo 1))
                            |> Expect.equal (Just (Right (Real 3.4)))                           
            , test "Cell.parse on formula" <|
                \_ ->
                    Parser.run CellParser.cellParser "add A2:Z2" |> Expect.equal (Ok (Left (Formula Add (Range { left = { col = 1, row = 0 }, right = { col = 1, row = 25 } }))))
            , test "Cell.parse on decimal number" <|
                \_ ->
                    Parser.run CellParser.cellParser "1.2" |> Expect.equal (Ok (Right (Real 1.2)))
            , test "Cell.parse on whole number" <|
                \_ ->
                    Parser.run CellParser.cellParser "3" |> Expect.equal (Ok (Right (Real 3)))
            , test "Cell2.parseIndex" <|
                \_ ->
                    Parser.run CellParser.indexParser "z2" |> Expect.equal (Ok { col = 1, row = 25 })
            , test "Cell2.parseIndex,  2 digits" <|
                \_ ->
                    Parser.run CellParser.indexParser "aa2" |> Expect.equal (Ok { col = 1, row = 26 })
            ]
        ]



-- DAT


ss1 =
    [ [ "100.0", "1.1", "a" ]
    , [ "120.0", "1.4", "b" ]
    , [ "140.0", "0.9", "c" ]
    , [ "-", "add A2:C2", "d" ]
    ]


s2 =
    [ [ "100.0", "1.7", "a" ]
    , [ "120.0", "1.4", "b" ]
    , [ "140.0", "0.9", "c" ]
    , [ "-", "add A2,B2", "d" ]
    ]


s3 =
    [ [ "100.0", "1.7", "a" ]
    , [ "120.0", "1.4", "b" ]
    , [ "140.0", "0.9", "c" ]
    , [ "-", "add A2,B2", "d" ]
    ]


s4 =
    [ [ "100.0", "1.7", "a" ]
    , [ "120.0", "sub C1,B1 ", "b" ]
    , [ "140.0", "0.9", "c" ]
    , [ "-", "add A2,B2", "d" ]
    ]


s5 =
    [ [ "1.0", "2.0", "3.0" ]
    , [ "4.0", "add A2,D2", "5.0" ]
    , [ "6.0", "7.0", "8.0" ]
    , [ "9.0", "mul D1,D3", "10.0" ]
    ]


s6 =
    [ [ "1.0", "2.0", "3.0" ]
    , [ "11.0", "5.0", "6.0" ]
    , [ "7.0", "8.0", "9.0" ]
    , [ "1.0", "add A2:C2", "2.0" ]
    ]



{-
   column 2
   slice: 2, 5, 8
   result = 15

          1         2
      A "1.0",    "2.0",      "3.0"
      B "11.0",   "5.0",       "6.0"
      C "7.0",    "8.0",       "9.0"
      D "1.0",   "add A2:C2", "2.0"


-}


s7 =
    [ [ "1.0", "2.0", "3.0", "4.0" ]
    , [ "4.0", "5.0", "6.0", "7.0" ]
    , [ "7.0", "8.0", "9.0", "add C1:C3" ]
    , [ "1.0", "1", "2.0", "3.0" ]
    ]


s8 = 
     [ ["100.0",      "1.1",       "mul A1,A2"]
      , ["120.0",     "1.4" ,      "mul B1,B2"]
      , ["140.0",     "0.9",       "mul C1,C2"]
       , [  "-" ,    "add A2:C2",    "add A3:C3"]
     ]

s8a = 
      [ ["100.0",      "1.1",       "mul A1,A2"]
      , ["120.0",     "1.4" ,       "mul B1,B2"]
      , ["140.0",     "0.9",       "mul C1,C2"]
      , [  "-" ,    "add A2:C2",    "add A3:C3"]
     ]    

s8b = """

100; 1.1; mul A1,A2
120; 1.4; mul B1,B2
140; 0.9; mul C1,C2
-;   add A2:C2; add A3:C3

"""   