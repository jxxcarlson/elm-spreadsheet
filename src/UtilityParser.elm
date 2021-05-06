module UtilityParser exposing (float, integer, negativeFloat, negativeInteger, positiveFloat, positiveInteger)

import Parser as P exposing (Parser)
import ParserTools as T
import XString


integer : Parser Int
integer =
    P.oneOf [ negativeInteger, positiveInteger ]


positiveInteger : Parser Int
positiveInteger =
    P.int


negativeInteger : Parser Int
negativeInteger =
    T.second (XString.char '-') positiveInteger |> P.map (\x -> -x)


float : Parser Float
float =
    P.oneOf [ negativeFloat, positiveFloat ]


positiveFloat : Parser Float
positiveFloat =
    P.float


negativeFloat : Parser Float
negativeFloat =
    T.second (XString.char '-') positiveFloat |> P.map (\x -> -x)
