module MMParser exposing (MMExpr(..), line, rawBlock, rawBlocks)

import Parser exposing (..)
import Parser.Extras exposing (many)


type MMExpr
    = MMList (List MMExpr)
    | RawBlock String


line : Parser String
line =
    getChompedString <|
        succeed ()
            |. chompUntil "\n"


rawBlock : Parser MMExpr
rawBlock =
    (succeed ()
        |. chompUntil "\n\n"
        |. spaces
    )
        |> getChompedString
        |> map String.trim
        |> map RawBlock


rawBlocks : Parser MMExpr
rawBlocks =
    many rawBlock
        |> map MMList
