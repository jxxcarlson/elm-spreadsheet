module ParserTools exposing
    ( Step(..)
    , between
    , char
    , first
    , loop
    , many
    , manyNonEmpty
    , manySeparatedBy
    , maybe
    , oneChar
    , optional
    , optionalList
    , second
    , text
    , third
    )

import Parser as Parser exposing ((|.), (|=), Parser)


type Problem
    = EndOfInput


{-| Apply a parser zero or more times and return a list of the results.
-}
many : Parser a -> Parser (List a)
many p =
    Parser.loop [] (manyHelp p)


manySeparatedBy : Parser () -> Parser a -> Parser (List a)
manySeparatedBy sep p =
    manyNonEmpty_ p (second sep p)


manyHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
manyHelp p vs =
    Parser.oneOf
        [ Parser.end |> Parser.map (\_ -> Parser.Done (List.reverse vs))
        , Parser.succeed (\v -> Parser.Loop (v :: vs))
            |= p
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse vs))
        ]


manyNonEmpty : Parser a -> Parser (List a)
manyNonEmpty p =
    p
        |> Parser.andThen (\x -> manyWithInitialList [ x ] p)


manyNonEmpty_ : Parser a -> Parser a -> Parser (List a)
manyNonEmpty_ p q =
    p
        |> Parser.andThen (\x -> manyWithInitialList [ x ] q)


manyWithInitialList : List a -> Parser a -> Parser (List a)
manyWithInitialList initialList p =
    Parser.loop initialList (manyHelp p)


{-| Running `optional p` means run p, but if it fails, succeed anyway
-}
optional : Parser () -> Parser ()
optional p =
    Parser.oneOf [ p, Parser.succeed () ]


{-| Running `optional p` means run p. If the parser succeeds with value _result_,
return _Just result_ . If the parser failes, return Nothing.
-}
maybe : Parser a -> Parser (Maybe a)
maybe p =
    Parser.oneOf [ p |> Parser.map (\x -> Just x), Parser.succeed () |> Parser.map (\_ -> Nothing) ]


{-| Running `optionalList p` means run p, but if it fails, succeed anyway,
returning the empty list
-}
optionalList : Parser (List a) -> Parser (List a)
optionalList p =
    Parser.oneOf [ p, Parser.succeed () |> Parser.map (\_ -> []) ]


{-| running `first p q` means run p, then run q
and return the result of running p.
-}
first : Parser a -> Parser b -> Parser a
first p q =
    p |> Parser.andThen (\x -> q |> Parser.map (\_ -> x))


{-| running `second p q` means run p, then run q
and return the result of running q.
-}
second : Parser a -> Parser b -> Parser b
second p q =
    p |> Parser.andThen (\_ -> q)


third : Parser a -> Parser b -> Parser c -> Parser c
third p q r =
    p |> Parser.andThen (\_ -> q) |> Parser.andThen (\_ -> r)


{-| Running between p q r runs p, then q, then r, returning the result of q:

> run (between (SchemeParser.symbol "[") SchemeParser.int (SchemeParser.symbol "]")) "[12]"
> Ok 12

-}
between : Parser a -> Parser b -> Parser c -> Parser b
between p q r =
    p |> Parser.andThen (\_ -> q) |> Parser.andThen (\x -> r |> Parser.map (\_ -> x))


{-| textPS = "text prefixText stopCharacters": Get the longest string
whose first character satisfies the prefixTest and whose remaining
characters are not in the list of stop characters. SpreadsheetTests:

    line =
        textPS (\c -> Char.isAlpha) [ '\n' ]

recognizes lines that start with an alphabetic character.

-}
textPS : (Char -> Bool) -> List Char -> Parser { start : Int, finish : Int, content : String }
textPS prefixTest stopChars =
    Parser.succeed (\start finish content -> { start = start, finish = finish, content = String.slice start finish content })
        |= Parser.getOffset
        |. Parser.chompIf (\c -> prefixTest c)
        |. Parser.chompWhile (\c -> not (List.member c stopChars))
        |= Parser.getOffset
        |= Parser.getSource


{-| textPS = "text prefixText stopCharacters": Get the longest string
whose first character satisfies the prefixTest and whose remaining
characters are not in the list of stop characters. SpreadsheetTests:

    line =
        textPS (\c -> Char.isAlpha) [ '\n' ]

recognizes lines that start with an alphabetic character.

-}
text : (Char -> Bool) -> (Char -> Bool) -> Parser { start : Int, finish : Int, content : String }
text prefixTest predicate =
    Parser.succeed (\start finish content -> { start = start, finish = finish, content = String.slice start finish content })
        |= Parser.getOffset
        |. Parser.chompIf (\c -> prefixTest c)
        |. Parser.chompWhile (\c -> predicate c)
        |= Parser.getOffset
        |= Parser.getSource


char : (Char -> Bool) -> Parser { start : Int, finish : Int, content : String }
char prefixTest =
    Parser.succeed (\start finish content -> { start = start, finish = finish, content = String.slice start finish content })
        |= Parser.getOffset
        |. Parser.chompIf (\c -> prefixTest c)
        |= Parser.getOffset
        |= Parser.getSource


oneChar : Parser String
oneChar =
    Parser.succeed (\begin end data -> String.slice begin end data)
        |= Parser.getOffset
        |. Parser.chompIf (\c -> True)
        |= Parser.getOffset
        |= Parser.getSource



-- LOOP


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s nextState =
    case nextState s of
        Loop s_ ->
            loop s_ nextState

        Done b ->
            b
