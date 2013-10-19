module Parser.Types where

data Action = ComputerAction ComputerVerb | PhysicalAction PhysicalVerb | Unknown deriving (Show, Eq)

data ComputerVerb = HelpCmd | LogoutCmd deriving (Show, Eq)

data PhysicalVerb = Help | Look | LookAt Noun | WalkTo Noun deriving (Show, Eq)

data Noun = Self | Your Noun | Noun String deriving (Show, Eq)

