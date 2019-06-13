module MMParser exposing
    ( MMExpr(..)
    , inline
    , inlineList
    , inlineMath
    , line
    , mathBlock
    , ordinaryText
    , parseToBlocks
    , rawBlock
    , rawBlocks
    , rawTextBlock
    )

import Parser exposing (..)
import Parser.Extras exposing (many)


type MMExpr
    = MMList (List MMExpr)
    | RawBlock String
    | Block (List MMExpr)
    | MathDisplayBlock String
      -- inline
    | OrdinaryText String
    | ItalicText String
    | InlineMath String


parseToBlocks : String -> Result (List DeadEnd) MMExpr
parseToBlocks str =
    Parser.run rawBlocks str


line : Parser String
line =
    getChompedString <|
        succeed ()
            |. chompUntil "\n"


{-| Unparsed paragraph
-}
rawTextBlock : Parser MMExpr
rawTextBlock =
    (succeed ()
        |. chompUntil "\n\n"
        |. spaces
    )
        |> getChompedString
        |> map String.trim
        |> map RawBlock


{-| }$$ ... $$
-}
mathBlock : Parser MMExpr
mathBlock =
    (succeed ()
        |. symbol "$$"
        |. chompWhile (\c -> c /= '$')
        |. symbol "$$"
        |. spaces
    )
        |> getChompedString
        |> map String.trim
        |> map (String.dropLeft 2)
        |> map (String.dropRight 2)
        |> map MathDisplayBlock


rawBlock =
    oneOf [ mathBlock, rawTextBlock ]


rawBlocks : Parser MMExpr
rawBlocks =
    many rawBlock
        |> map MMList



--
-- INLINE
--


ordinaryText : Parser MMExpr
ordinaryText =
    chompWhile (\c -> not <| List.member c [ '$', '*' ])
        |> getChompedString
        |> map OrdinaryText


inlineMath : Parser MMExpr
inlineMath =
    (succeed ()
        |. symbol "$"
        |. chompUntil "$"
        |. symbol "$"
    )
        |> getChompedString
        |> map (String.dropLeft 1)
        |> map InlineMath


inline : Parser MMExpr
inline =
    oneOf [ ordinaryText, inlineMath ]


inlineList : Parser MMExpr
inlineList =
    many inline
        |> map MMList
