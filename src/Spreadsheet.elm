module Spreadsheet exposing (..)

import Dict exposing (Dict)
import Either exposing (Either(..))
import List.Extra
import Maybe.Extra


type Value
    = Integer Int
    | Real Float
    | Boolean Bool
    | String String
    | Undefined


type alias Row =
    Int


type alias Col =
    Int


type alias Cell =
    -- { row : Row, col : Col, value : Either Value Formula }
    Either Formula Value



--type Formula
--    = CellFormula Row Col Cell
--    | RowFormula Row Col Col Cell
--    | ColFormula Col Row Row Cell
--    | MatrixFormula Row Row Col Col Cell


type Formula
    = OpSymbol String


type alias SpreadsheetColumn =
    List Cell


type alias Spreadsheet =
    List SpreadsheetColumn



--evalColumn : Int -> Spreadsheet -> Spreadsheet
--evalColumn col sheet =


getColumn : Col -> Spreadsheet -> Maybe SpreadsheetColumn
getColumn col sheet =
    List.Extra.getAt col sheet


getCell : Row -> Col -> Spreadsheet -> Maybe Cell
getCell row col sheet =
    Maybe.andThen (List.Extra.getAt row) (getColumn col sheet)


opRealColumns_ : Col -> Col -> Col -> Spreadsheet -> Maybe SpreadsheetColumn
opRealColumns_:wq i j k sheet =
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


testSheetCol2 =
    [ Right (Real 1.1), Right (Real 1.4), Right (Real 0.9) ]


testSheetCol3 =
    [ Left (OpSymbol "*"), Left (OpSymbol "*"), Left (OpSymbol "*") ]


testSheet =
    [ testSheetCol1, testSheetCol2, testSheetCol3 ]
