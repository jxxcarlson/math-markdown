module Block exposing (..)

{-| This module exports just one function,
intended to turn a string into a list
of logical paragraphs. It operates as a
finite-state machine.


# API

@docs logicalParagraphify

-}

import MMParser
import Regex
import LineType exposing(BlockType(..), BalancedType(..), MarkdownType(..))

type ParserState
    = Start
    | InBlock BlockType Level
    | IgnoreLine
    | Error

type Block = Block  BlockType Level Content
type alias Level = Int
type alias Content = String


type alias ParserRecord =
    { currentParagraph : String
    , paragraphList : List String
    , state : ParserState
    }

type ParserState
    = Start
    | InBlock Block
    | IgnoreLine
    | Error



type alias ParserRecord =
    { currentBlock: Block
    , blockList : List Block
    , state : ParserState
    }
