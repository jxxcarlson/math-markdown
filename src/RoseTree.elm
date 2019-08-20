module RoseTree exposing (..)

type RoseTree a = RoseTree a (List (RoseTree a))

r1 = RoseTree "A" []

r2 = RoseTree "colors" [RoseTree "red" [], RoseTree "blue" [], RoseTree "green" []]

r3 = RoseTree "sounds" [RoseTree "whisper" [], RoseTree "cello" [], RoseTree "shout" []]

r4 = RoseTree "sensations" [r2, r3]