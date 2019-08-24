module Block exposing
    ( parseToTree, parse, runFSM
    , Block, stringOfBlockTree)

{-| A markdown document is parsed into a tree
of Blocks using

    parseToTree : String -> Tree Block

This function applies

    parse : String -> List Block

and then the partially applied function

    HTree.fromList rootBlock blockLevel :
       List Block -> Tree Block

This last step is possible because the elements of `List Block`
are annotated by their level.
The `parse` function operated by running a finite-state machine.
Thie machine has type

    type FSM
        = FSM State (List Block)

where the three possible states are defined by

    type State
        = Start
        | InBlock Block
        | Error

If the FSM consumes all its input and no error
id encountered, then the `(List Block)` component of the FSM contains
the result of parsing the input string into blocks.

@docs Block, parseToTree, parse, runFSM

-}

import HTree
import LineType exposing (BalancedType(..), BlockType(..), MarkdownType(..))
import Tree exposing (Tree)


type State
    = Start
    | InBlock Block
    | Error

type FSM
    = FSM State (List Block)


{-| A Block is defined as follows:

    type Block
        = Block BlockType Level Content

    type alias Level =
        Int

    type alias Content =
        String

-}
type Block = Block BlockType Level Content


type alias Level =
    Int


type alias Content =
    String


blockLevel : Block -> Int
blockLevel (Block _ k _) =
    k


type_ : Block -> BlockType
type_ (Block bt _ _) =
    bt


typeOfState : State -> Maybe BlockType
typeOfState s =
    case s of
        Start ->
            Nothing

        InBlock b ->
            Just (type_ b)

        Error ->
            Nothing





rootBlock =
    Block (MarkdownBlock Plain) 0 "*"


{-|

    parseToTree  "- One\nsome stuff\n- Two\nMore stuff"
    -->    Tree (Block (MarkdownBlock Plain) 0 "*") [
    -->      Tree (Block (MarkdownBlock UListItem) 1 ("- One\nsome stuff\n")) []
    -->      ,Tree (Block (MarkdownBlock UListItem) 1 ("- Two\nMore stuff\n")) []
    -->    ]

-}
parseToTree : String -> Tree Block
parseToTree str =
    str
        |> parse
        |> HTree.fromList rootBlock blockLevel


{-|

    parse "- One\nsome stuff\n- Two\nMore stuff"
    --> [ Block (MarkdownBlock UListItem)
    -->    1 ("- One\nsome stuff\n")
    -->  ,Block (MarkdownBlock UListItem)
    -->    1 ("- Two\nMore stuff\n")
    --> ]

-}
parse : String -> List Block
parse str =
    runFSM str |> flush


{-|

    runFSM  "- One\nsome stuff\n- Two\nMore stuff"
    --> FSM (InBlock (Block (MarkdownBlock UListItem)
    -->        1 ("- Two\nMore stuff\n")))
    -->     [Block (MarkdownBlock UListItem)
    -->        1 ("- One\nsome stuff\n")]

-}
runFSM : String -> FSM
runFSM str =
    let
        folder : String -> FSM -> FSM
        folder =
            \line fsm -> nextState line fsm
    in
    List.foldl folder initialFSM (splitIntoLines str)


flush : FSM -> List Block
flush fsm =
    case stateOfFSM fsm of
        Start ->
            List.reverse (blockListOfFSM fsm)

        Error ->
            List.reverse (blockListOfFSM fsm)

        InBlock b ->
            List.reverse (b :: blockListOfFSM fsm)


stateOfFSM : FSM -> State
stateOfFSM (FSM state_ _) =
    state_


blockListOfFSM : FSM -> List Block
blockListOfFSM (FSM _ blockList_) =
    blockList_


splitIntoLines : String -> List String
splitIntoLines str =
    str |> String.lines
        |> List.map (\l -> l ++ "\n")



initialFSM : FSM
initialFSM =
    FSM Start []


nextState : String -> FSM -> FSM
nextState str fsm =
    case stateOfFSM fsm of
        Start ->
            nextStateS str fsm

        InBlock _ ->
            nextStateB str fsm

        Error ->
            fsm


nextStateS : String -> FSM -> FSM
nextStateS line (FSM state blockList) =
    case LineType.get line of
        ( _, Nothing ) ->
            FSM Error blockList

        ( level, Just blockType ) ->
            FSM (InBlock (Block blockType level (Debug.log "START" line))) blockList


nextStateB1 : String -> FSM -> FSM
nextStateB1 line fsm =
    fsm


nextStateB : String -> FSM -> FSM
nextStateB line fsm =
    case LineType.get line of
        ( _, Nothing ) ->
            FSM Error (blockListOfFSM fsm)

        ( level, Just lineType ) ->

            -- process balanced block
            if LineType.isBalanced lineType then
              processBalancedBlock lineType line fsm

                -- add text
            else if lineType == MarkdownBlock Plain then
                addLineToFSM (Debug.log "PLAIN" line) fsm


            else if LineType.isMarkDown lineType then
                case stateOfFSM fsm of
                    -- add current block to block list and
                    -- start new block with the current line and lineType
                    InBlock block_ ->
                        FSM (InBlock (Block lineType (LineType.level (Debug.log "MD1" line)) line))
                            (block_ :: blockListOfFSM fsm)

                    _ ->
                        fsm

            else
                fsm

processBalancedBlock1 : BlockType -> String -> FSM -> FSM
processBalancedBlock1 lineType line fsm =
    if Just lineType == typeOfState (stateOfFSM fsm) then
         -- the currently processed block should be closed
        case stateOfFSM fsm of
            InBlock block_ ->
                FSM Start (addLineToBlock line block_ :: blockListOfFSM fsm)

            _ ->
                fsm
    -- open balanced block
    else
      fsm

processBalancedBlock : BlockType -> String -> FSM -> FSM
processBalancedBlock lineType line fsm =
    if Just lineType == typeOfState (stateOfFSM fsm) then
         -- the currently processed block should be closed
        case stateOfFSM fsm of
            InBlock block_ ->
                FSM Start (addLineToBlock (Debug.log "CLOSE" line) block_ :: blockListOfFSM fsm)

            _ ->
                fsm
    -- open balanced block
    else
      case stateOfFSM fsm of
        InBlock block_ ->
            FSM (InBlock (Block lineType (LineType.level (Debug.log "OPEN" line)) line)) (block_ :: blockListOfFSM fsm)

        _ ->
            fsm


nextStateB2 : String -> FSM -> FSM
nextStateB2 line fsm =
    case LineType.get line of
        ( _, Nothing ) ->
            FSM Error (blockListOfFSM fsm)

        ( level, Just lineType ) ->
            -- close balanced block
            if LineType.isBalanced lineType then
              if  Just lineType == typeOfState (stateOfFSM fsm) then
                case stateOfFSM fsm of
                    InBlock block_ ->
                        FSM Start (addLineToBlock line block_ :: blockListOfFSM fsm)

                    _ ->
                        fsm
              -- open balanced block
              else
                FSM (InBlock (Block lineType (LineType.level line) line)) (blockListOfFSM fsm)


                -- add text to block

            else if lineType == MarkdownBlock Plain then
                addLineToFSM line fsm
                -- start new block

            else if LineType.isMarkDown lineType then
                case stateOfFSM fsm of
                    InBlock block_ ->
                        FSM (InBlock (Block lineType (LineType.level line) line))
                            (block_ :: blockListOfFSM fsm)

                    _ ->
                        fsm

            else
                fsm

addLineToFSM : String -> FSM -> FSM
addLineToFSM str (FSM state_ blocks_) =
    case state_ of
        Start ->
            FSM state_ blocks_

        Error ->
            FSM state_ blocks_

        InBlock block_ ->
            FSM (addLineToState str state_) blocks_


addLineToState : String -> State -> State
addLineToState str state_ =
    case state_ of
        Start ->
            Start

        Error ->
            Error

        InBlock block_ ->
            InBlock (addLineToBlock str block_)


addLineToBlock : String -> Block -> Block
addLineToBlock str (Block blockType_ level_ content_) =
    Block blockType_ level_ (content_ ++ str)


-- STRING --

stringOfBlockTree : Tree Block -> String
stringOfBlockTree tree =
    tree
     |> Tree.flatten
     |> List.map stringOfBlock
     |> String.join "\n"


stringOfBlock : Block -> String
stringOfBlock (Block bt lev_ content_) =
    String.repeat (2 * lev_) " "
    ++
    LineType.stringOfBlockType bt
    ++
    " (" ++ String.fromInt lev_ ++ ") "
    ++ "\n" ++ indent lev_ content_

indent : Int -> String -> String
indent k str =
    str
      |> String.split "\n"
      |> List.map (\s -> (String.repeat (2 * k) " ") ++ s)
      |> String.join "\n"