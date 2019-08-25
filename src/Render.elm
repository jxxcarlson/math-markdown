module Render exposing (..)

import Block exposing(MMBlock(..), BlockContent(..))
import LineType exposing(BlockType(..), MarkdownType(..))
import MMInline exposing(MMInline(..))
import Tree exposing(Tree)


toHtmlString1 : Tree MMBlock -> String
toHtmlString1 tree =
    "... rendered content ..."


toHtmlString : Tree MMBlock -> String
toHtmlString tree =
    Tree.foldl (\block str -> renderBlock block ++ str) "" tree

renderBlock : MMBlock -> String
renderBlock block =
    case block of
        (MMBlock (MarkdownBlock Plain) level blockContent) -> renderBlockContent blockContent
        _ -> "mmInline\n"

renderBlockContent : BlockContent -> String
renderBlockContent blockContent =
      case blockContent of
          M mmInline -> MMInline.render mmInline
          T str -> str


