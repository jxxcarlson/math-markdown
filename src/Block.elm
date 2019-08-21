module Block exposing (..)

{-| This module exports just one function,
intended to turn a string into a list
of logical paragraphs. It operates as a
finite-state machine.




-}


import LineType exposing(BlockType(..), BalancedType(..), MarkdownType(..))


type FSM = FSM State BlockInfo


stateOfFSM : FSM -> State
stateOfFSM (FSM state_ _) = state_

blockInfoOfFSM : FSM -> BlockInfo
blockInfoOfFSM (FSM _ blockInfo_) = blockInfo_

blockListOfFSM : FSM -> List Block
blockListOfFSM fsm =
    blockInfoOfFSM fsm |> .list



parse : String -> List Block
parse str =
    let
        folder : String -> FSM -> FSM
        folder = (\line fsm -> nextState line fsm)
    in
    List.foldl folder initialFSM (String.lines str)
      |> blockListOfFSM

nextState : String -> FSM -> FSM
nextState str fsm = fsm

initialFSM : FSM
initialFSM = FSM Start initialBlockInfo

initialBlockInfo : BlockInfo
initialBlockInfo = { current  = Nothing, list = []}


type Block = Block  BlockType Level Content
type alias Level = Int
type alias Content = String


type alias BlockInfo =
    { current : Maybe Block
    , list : List Block
    }

type State
    = Start
    | InBlock Block


