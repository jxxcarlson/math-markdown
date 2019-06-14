module MMParser exposing
    ( MMBlock(..)
    , MMInline(..)
    , inline
    , inlineList
    , inlineMath
    , italicText
    , mathBlock
    , ordinaryText
    , rawBlock
    , rawBlocks
    , rawTextBlock
    )

import Parser exposing (..)
import Parser.Extras exposing (many)


type MMBlock
    = MMList (List MMBlock)
    | Block MMBlock
    | RawBlock String
    | MathDisplayBlock String


type MMInline
    = OrdinaryText String
    | ItalicText String
    | InlineMath String
    | MMInlineList MMInline
    | Error (List MMInline)



-- parseToBlocks : String -> Result (List DeadEnd) MMExpr
--
-- parseToBlocks str =
--     Parser.run rawBlocks str
--         |> decodeResult
--
-- |> List.map parseRawBlock
-- |> Parser.map inlineList
--
-- line : Parser String
-- line =
--     getChompedString <|
--         succeed ()
--             |. chompUntil "\n"


{-|

> run rawTextBlock "a b c\\n\\n"
> Ok (RawBlock ("a b c"))

-}
rawTextBlock : Parser MMBlock
rawTextBlock =
    (succeed ()
        |. chompUntil "\n\n"
        |. spaces
    )
        |> getChompedString
        |> map String.trim
        |> map RawBlock


{-|

> run mathBlock "$$a^2 - 7$$\\n\\n"
> Ok (MathDisplayBlock ("a^2 - 7"))

-}
mathBlock : Parser MMBlock
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


{-|

> run rawBlocks "$$a^2 - 7$$\\n\\nho ho ho!!\\n\\n"
> Ok (MMList [MathDisplayBlock ("a^2 - 7"),RawBlock ("ho ho ho!!")])

-}
rawBlocks : Parser MMBlock
rawBlocks =
    many rawBlock
        |> map MMList



--
-- INLINE
--


{-|

> run ordinaryText "abc"
> Ok (OrdinaryText "abc")

-}
ordinaryText : Parser MMInline
ordinaryText =
    (succeed ()
        |. chompIf (\c -> not <| List.member c [ '$', '*', '\n' ])
        |. chompWhile (\c -> not <| List.member c [ '$', '*', '\n' ])
    )
        |> getChompedString
        |> map OrdinaryText


{-|

> run italicText "_abc_"
> Ok (ItalicText "abc")

-}
italicText : Parser MMInline
italicText =
    (succeed ()
        |. symbol "*"
        |. chompWhile (\c -> c /= '*')
        |. symbol "*"
        |. spaces
    )
        |> getChompedString
        |> map (String.dropLeft 1)
        |> map (String.replace "*" "")
        |> map ItalicText


{-|

> run inlineMath "$a^5 = 3$"
> Ok (InlineMath ("a^5 = 3"))

-}
inlineMath : Parser MMInline
inlineMath =
    (succeed ()
        |. symbol "$"
        |. chompWhile (\c -> c /= '$')
        |. symbol "$"
        |. spaces
    )
        |> getChompedString
        |> map (String.dropLeft 1)
        |> map (String.dropRight 1)
        |> map InlineMath


{-|

> run inline "$a^5 = 1$"
> Ok (InlineMath ("a^5 = 1"))

> run inline "_abc_"
> Ok (ItalicText "abc")

> run inline "hahaha"
> Ok (OrdinaryText "hahaha")

-}
inline : Parser MMInline
inline =
    oneOf [ italicText, inlineMath, ordinaryText ]


{-|

> run inlineList "_foo_ hahaha\\nnhohoh\\nn$a^6 + 2$"
> Ok [ItalicText ("foo "),OrdinaryText "hahaha",OrdinaryText "nhohoh",OrdinaryText "n",InlineMath ("a^6 + 2")]

-}
inlineList : Parser (List MMInline)
inlineList =
    many inline


decodeInlineError : DeadEnd -> MMInline
decodeInlineError err =
    OrdinaryText "error"



-- |> map MMInlineList
-- -- parseRawBlock : RawBlock String -> MMExpr
-- -- MMList (List MMExpr)
-- -- parseRawBlock : MMExpr
-- --
--
--
-- parseRawBlock : MMBlock -> MMInline
-- parseRawBlock (RawBlock str) =
--     let
--         result =
--             Parser.run inlineList str
--     in
--     case result of
--         Ok mmExpr ->
--             Block mmExpr
--
--         Err err ->
--             Error (List.map decodeError err)
--
--
--
--
-- --
-- -- decodeResult : Result (List DeadEnd) MMExpr -> MMExpr
-- -- decodeResult result =
-- --     case result of
-- --         Ok result_ ->
-- --             result_
-- --
-- --         Err err ->
-- --             List.map decodeError err
-- --                 |> MMList
-- -- Error (Result (List DeadEnd) MMExpr)
