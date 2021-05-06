module Spreadsheet exposing (..)

import Cell exposing (Cell, Formula(..), Value(..))
import Dict exposing (Dict)
import Either exposing (Either(..))
import List.Extra
import Maybe.Extra



{-
   > Spreadsheet.parse textSheet
   [[Right (Real 100),Right (Real 120),Right (Real 140)],[Right (Real 1.1),Right (Real 1.4),Right (Real 0.9)],[Left (OpSymbol "*"),Left (OpSymbol "*"),Left (OpSymbol "*")]]
       : Spreadsheet

   > Spreadsheet.parse textSheet |>  opRealColumns 2 0 1
   [[Right (Real 100),Right (Real 120),Right (Real 140)],[Right (Real 1.1),Right (Real 1.4),Right (Real 0.9)],[Right (Real 110.00000000000001),Right (Real 168),Right (Real 126)]]
      : Spreadsheet

-}


type alias Row =
    Int


type alias Col =
    Int


type alias SpreadsheetColumn =
    List Cell


type alias Spreadsheet =
    List SpreadsheetColumn


type alias TextSpreadsheet =
    List TextColumn


type alias TextColumn =
    List String


{-|

    > textSheet
    [["100.0","120.0","140.0"],["1.1","1.4","0.9"],["*","*","*"]]

    > textSheet |> computeReal 2 0 1
    [["100","120","140"],["1.1","1.4","0.9"],["110.00000000000001","168","126"]]

-}
computeReal : Col -> Col -> Col -> TextSpreadsheet -> TextSpreadsheet
computeReal opCol sourceCol1 sourceCol2 text =
    text
        |> parse
        |> opRealColumns opCol sourceCol1 sourceCol2
        |> render


render : Spreadsheet -> TextSpreadsheet
render sheet =
    List.map renderColumn sheet


renderColumn : List Cell -> List String
renderColumn cells =
    List.map Cell.render cells


parse : TextSpreadsheet -> Spreadsheet
parse text =
    List.map parseColumn text


parseColumn : List String -> List Cell
parseColumn cells =
    List.map Cell.parse cells |> Maybe.Extra.values


getColumn : Col -> Spreadsheet -> Maybe SpreadsheetColumn
getColumn col sheet =
    List.Extra.getAt col sheet


getCell : Row -> Col -> Spreadsheet -> Maybe Cell
getCell row col sheet =
    Maybe.andThen (List.Extra.getAt row) (getColumn col sheet)


opRealColumns : Col -> Col -> Col -> Spreadsheet -> Spreadsheet
opRealColumns i j k sheet =
    case opRealColumns_ i j k sheet of
        Nothing ->
            sheet

        Just newColumn ->
            List.Extra.setAt i newColumn sheet


opRealColumns_ : Col -> Col -> Col -> Spreadsheet -> Maybe SpreadsheetColumn
opRealColumns_ i j k sheet =
    case ( getColumn i sheet, getColumn j sheet, getColumn k sheet ) of
        ( Just col1, Just col2, Just col3 ) ->
            List.map3 applyRealOp col1 col2 col3 |> (Maybe.Extra.values >> Just)

        _ ->
            Nothing


opRealDict : Dict String (Float -> Float -> Float)
opRealDict =
    Dict.fromList
        [ ( "+", (+) )
        , ( "-", (-) )
        , ( "*", (*) )
        , ( "/", (/) )
        ]


applyRealOp : Cell -> Cell -> Cell -> Maybe Cell
applyRealOp opCell c1 c2 =
    case ( opCell, c1, c2 ) of
        ( Left (OpSymbol opSymbol), Right (Real x), Right (Real y) ) ->
            case Dict.get opSymbol opRealDict of
                Just op ->
                    Just (Right (Real (op x y)))

                Nothing ->
                    Just (Right Undefined)

        _ ->
            Nothing


testSheetCol1 =
    [ Right (Real 100), Right (Real 120), Right (Real 140) ]


textCol1 =
    [ "100.0", "120.0", "140.0" ]


testSheetCol2 =
    [ Right (Real 1.1), Right (Real 1.4), Right (Real 0.9) ]


textCol2 =
    [ "1.1", "1.4", "0.9" ]


testSheetCol3 =
    [ Left (OpSymbol "*"), Left (OpSymbol "*"), Left (OpSymbol "*") ]


textCol3 =
    [ "*", "*", "*" ]


testSheet =
    [ testSheetCol1, testSheetCol2, testSheetCol3 ]


textSheet =
    [ textCol1, textCol2, textCol3 ]
