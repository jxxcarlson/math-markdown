module MMParser exposing
    ( MMBlock(..)
    , MMInline(..)
    , block
    , blocks
    , closeBlock
    , headingBlock
    , inline
    , inlineList
    , inlineMath
    , italicText
    , mathBlock
    , ordinaryText
    , parseUntil
    , rawTextBlock
    , runBlocks
    , runBlocks1
    )

import Parser exposing (..)
import Parser.Extras exposing (many)


type MMBlock
    = MMList (List MMBlock)
    | Block MMBlock
    | RawBlock String
    | HeadingBlock Int String
    | MathDisplayBlock String
    | ClosedBlock MMInline


type MMInline
    = OrdinaryText String
    | ItalicText String
    | BoldText String
    | InlineMath String
    | MMInlineList (List MMInline)
    | Error (List MMInline)


{-|

> closeBlock <| RawBlock ("_foo_ ha ha ha\\nho ho ho\\n$a^6 + 2$")
> ClosedBlock (MMInlineList [ItalicText ("foo "),OrdinaryText ("ha ha ha"),OrdinaryText ("ho ho ho"),InlineMath ("a^6 + 2")])

-}
closeBlock : MMBlock -> MMBlock
closeBlock block_ =
    case block_ of
        RawBlock str ->
            runInlineList str |> ClosedBlock

        _ ->
            block_


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


type alias PrefixedString =
    { prefix : String, text : String }


{-|

> run headingBlock "### foo bar \\n\\n"
> Ok (HeadingBlock 3 ("foo bar"))

-}
headingBlock : Parser MMBlock
headingBlock =
    (succeed PrefixedString
        |= parseWhile (\c -> c == '#')
        |= parseUntil "\n\n"
    )
        |> map (\ps -> HeadingBlock (String.length ps.prefix) (String.trim ps.text))


{-|

> run (parseUntil ";;") "a b c;;"
> Ok ("a b c") : Result (List P.DeadEnd) String

-}
parseUntil : String -> Parser String
parseUntil end =
    chompUntil end |> getChompedString


parseWhile : (Char -> Bool) -> Parser String
parseWhile accepting =
    chompWhile accepting |> getChompedString


block =
    oneOf [ mathBlock, rawTextBlock ]


{-|

> run rawBlocks "$$a^2 - 7$$\\n\\nho ho ho!!\\n\\n"
> Ok (MMList [MathDisplayBlock ("a^2 - 7"),RawBlock ("ho ho ho!!")])

-}
blocks : Parser MMBlock
blocks =
    many block
        |> map MMList


{-|

> str
> "_foo_ ha ha ha\\nho ho ho\\n$a^6 + 2$\\n\\n$$a^2 = 3$$\\n\\n" : String

> runBlocks str
> MMList [RawBlock ("_foo_ ha ha ha\nho ho ho\n$a^6 + 2$"),MathDisplayBlock ("a^2 = 3")]

    : MMBlock

-}
runBlocks1 : String -> MMBlock
runBlocks1 str =
    Parser.run blocks str
        |> resolveBlockResult


{-|

> str
> "_foo_ ha ha ha\\nho ho ho\\n$a^6 + 2$\\n\\n$$a^2 = 3$$\\n\\n" : String

> runBlocks2 str
> [ClosedBlock (MMInlineList [ItalicText ("foo "),OrdinaryText ("ha ha ha"),OrdinaryText ("ho ho ho"),InlineMath ("a^6 + 2")]),MathDisplayBlock ("a^2 = 3")]

    : List MMBlock

-}
runBlocks : String -> List MMBlock
runBlocks str =
    case Parser.run (many block) str of
        Ok list ->
            List.map closeBlock list

        Err _ ->
            [ ClosedBlock (OrdinaryText "Error") ]



-- |> resolveBlockResult


resolveBlockResult : Result (List DeadEnd) MMBlock -> MMBlock
resolveBlockResult result =
    case result of
        Ok res_ ->
            res_

        Err err ->
            List.map decodeBlockError err
                |> MMInlineList
                |> ClosedBlock


decodeBlockError : DeadEnd -> MMInline
decodeBlockError err =
    OrdinaryText "error"



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


boldText : Parser MMInline
boldText =
    (succeed ()
        |. symbol "**"
        |. chompWhile (\c -> c /= '*')
        |. symbol "**"
        |. spaces
    )
        |> getChompedString
        |> map (String.dropLeft 1)
        |> map (String.replace "**" "")
        |> map BoldText


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
    oneOf [ boldText, italicText, inlineMath, ordinaryText ]


{-|

> run inlineList "_foo_ hahaha\\nnhohoh\\nn$a^6 + 2$"
> Ok [ItalicText ("foo "),OrdinaryText "hahaha",OrdinaryText "nhohoh",OrdinaryText "n",InlineMath ("a^6 + 2")]

-}
inlineList : Parser (List MMInline)
inlineList =
    many inline


runInlineList : String -> MMInline
runInlineList str =
    run inlineList str
        |> resolveInlineResult


resolveInlineResult : Result (List DeadEnd) (List MMInline) -> MMInline
resolveInlineResult result =
    case result of
        Ok res_ ->
            res_ |> MMInlineList

        Err err ->
            List.map decodeInlineError err
                |> MMInlineList


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
