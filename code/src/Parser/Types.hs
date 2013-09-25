module Parser.Types where

data Action = ComputerAction ComputerVerb | PhysicalAction PhysicalVerb | Unknown deriving (Show, Eq)

data ComputerVerb = HelpCmd | LogoutCmd deriving (Show, Eq)

data PhysicalVerb = Help | Look | LookAt String | WalkTo String deriving (Show, Eq)

