module MMRender exposing (..)

import Html exposing (Html)
import MMParser exposing (MMBlock(..), MMInline(..))


render : List MMBlock -> List (Html a)
