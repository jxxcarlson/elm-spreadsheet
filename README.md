# Elm-Spreadsheet

Elm-spreadsheet provides the means to represent and compute with
spreadsheets in Elm. A spreadsheet is a two-dimensional array
of cells, each of which may contain either data (values) or formulas.
To evaluate a spreadsheet is to apply the formulas to create
a new spreadsheet.  If the new spreadsheet contains no formulas,
we say that it is _fully evaluated._

## Representation

Spreadsheets can be represented in a variety of ways.

1. As a `String`
    
    ```
      100.0             1.1       row * 0 1
      120.0             1.4       row * 0 1
      140.0             0.9       row * 0 1
          -     col sum 0 2     col sum 0 2
    ```
2. As a `List (List String)`
    
    ```elm
    > TestData.text
    [
         ["100.0","120.0","140.0","-"]
        ,["1.1","1.4","0.9","col sum 0 2"]
        ,["row * 0 1","row * 0 1","row * 0 1","col sum 0 2"]
    ]
    ```

3. As a `List Cell`

    ```elm
    > parse TestData.text
    [[Right (Real 100),Right (Real 120),Right (Real 140),Right Undefined],[Right (Real 1.1),Right (Real 1.4),Right (Real 0.9),Left (ColOp "sum" 0 2)],[Left (RowOp "*" 0 1),Left (RowOp "*" 0 1),Left (RowOp "*" 0 1),Left (ColOp "sum" 0 2)]]
    ```
   
    where

    ```elm
    type alias Cell =
        Either Formula Value
        
    type Formula
        = RowOp String Col Col
        | ColOp String Row Row
        
    type Value
        = Integer Int
        | Real Float
        | Boolean Bool
        | String String
        | Undefined
    ```
    One can convert this format back into a text represention: 
   
    ```elm
    render : List Cell -> List (List String)
    render sheet =
        List.map renderColumn sheet
    ```
   
## Evaluation

One can evaluate a spreadsheet using the `eval` function.  
The code below


```elm
  TestData.text |> parse |> eval |> render |> Pretty.print
```

yields the data

```
  100    1.1    110.0
  120    1.4    168.0
  140    0.9    126.0
    -    3.4    404.0
```

For more on how `eval` works, please see NOTES.md.