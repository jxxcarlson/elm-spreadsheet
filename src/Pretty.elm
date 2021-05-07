module Pretty exposing (maxWidths, print, printCols, printList, printS)

import List.Extra



-- print : List (List String) -> String


print table =
    List.map2 (\w c -> ( w, c )) (maxWidths (List.Extra.transpose table)) (List.Extra.transpose table)
        |> List.map (\( w, c ) -> printList (w + 4) ' ' " " c)
        |> String.join "\n"


printS : List (List String) -> String
printS table =
    table
        |> List.Extra.transpose
        |> List.map (printList 15 ' ' " ")
        |> String.join "\n"


printCols1 table =
    List.map2 (\w c -> ( w, c )) (maxWidths table) (List.Extra.transpose table)


printCols table =
    List.map2 (\w c -> ( w, c )) (maxWidths table) table
        |> List.map (\( w, c ) -> printList w ' ' " " c)
        |> String.join "\n"



-- |> List.map (\( w, c ) -> printList w ' ' " " c)
-- |> printList 0 ' ' "\n"


maxWidths : List (List String) -> List Int
maxWidths columns =
    List.map maxWidth columns


maxWidth : List String -> Int
maxWidth items =
    items |> List.map String.length |> List.maximum |> Maybe.withDefault 0


printList : Int -> Char -> String -> List String -> String
printList width padding joiner items =
    List.map (String.padLeft width padding) items |> String.join joiner
