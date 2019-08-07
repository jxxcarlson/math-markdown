module TestStrings exposing (str1, str2)


str1 =
    """
  This is a test.


- one, la di dah

- two

- three

*Italic:* **this is bold!**

"""



-- [ClosedBlock (MMInlineList [])
--  ,ClosedBlock (MMInlineList [OrdinaryText ("This is a test.")])
--  ,ListItemBlock 1 (MMInlineList [OrdinaryText "one"])
--  ,ListItemBlock 1 (MMInlineList [OrdinaryText "two"])
--  ,ListItemBlock 1 (MMInlineList [OrdinaryText "three"])
--  ,ClosedBlock (MMInlineList [ItalicText ("Italic: "),BoldText ("this is bold!")])]


str2 =
    """
  This is a test.

- one
la di dah do day

- two

- three

*Italic:* **this is bold!**

"""



--
-- [ClosedBlock (MMInlineList [])
-- ,ClosedBlock (MMInlineList [OrdinaryText ("This is a test.")])
-- ,ListItemBlock 1 (MMInlineList [OrdinaryText "one"])
-- ,ClosedBlock (MMInlineList [OrdinaryText ("- a  - b")])
-- ,ListItemBlock 1 (MMInlineList [OrdinaryText "two"])
-- ,ListItemBlock 1 (MMInlineList [OrdinaryText "three"])
-- ,ClosedBlock (MMInlineList [ItalicText ("Italic: "),BoldText ("this is bold!")])]
