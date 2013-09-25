{-# LANGUAGE RecordWildCards #-}
module Description where

import           Types
import           Debug.Trace
import           Data.List   ( intercalate )


-- | Describing Things | --

-- | Pluralize takes a list of things and a list of statements. The number
-- of things determines which statement is picked. If there are zero things
-- the zero'th statement will be picked. If there are more things than
-- statements the last statement will be picked. If no things and no
-- statements ar given, god help us all.
pluralize :: Show a => [a] -> [String] -> String
pluralize []  []      = "nothing"
pluralize []  (s:_)   = s
pluralize _   (s:[])  = s
pluralize (_:t's) (_:s's) = pluralize t's s's
pluralize t's s's = trace (show (t's, s's)) "something else"


-- | Finding Things by Description | --

matchesDescription :: String -> Description -> Bool
matchesDescription name Description{..} =
    _descTitle == name || _descShort == name


-- | Describing Descriptions | --

passiveDescName :: Description -> String
passiveDescName Description{..} = unwords [ _pfxPassive _descPfx
                                          , _descShort
                                          ]

activeDescName :: Description -> String
activeDescName Description{..} = unwords [ _pfxActive _descPfx
                                         , _descShort
                                         ]

headingDesc :: Char -> Description -> String
headingDesc c Description{..} = concat [ _descTitle
                                       , "\n"
                                       , replicate (length _descTitle) c
                                       , "\n"
                                       ]

h1Desc :: Description -> String
h1Desc = headingDesc '='

h2Desc :: Description -> String
h2Desc = headingDesc '-'


-- | Making things describable | --

-- | Like show but qualitative and interesting.
class Describable a where
     describe :: a -> String

-- | Worlds | --

instance Describable World where
    describe ComputerWorld{} = "The computer."
    describe StartWorld{}    = "The start screen."
    describe RealWorld{_loc=loc} = describe loc

-- | Locations | --

instance Describable Location where
    describe Location{_locDesc = desc, ..} =
        concat [ h1Desc desc
               , _descLong desc
               , "\n"
               , show (pi * (_locRadMeters ** 2))
               , " meters^2 in area.\n"
               , "There "
               , pluralize _locNodes [ "is no place"
                                     , "is one place"
                                     , "are a couple places"
                                     , "are some places"
                                     , "many places"
                                     ]
               , " "
               , pluralize _locNodes [ "you can go from here."
                                     , "you can get to from here "
                                     , "you can get to from here, "
                                     ]
               , pluralize _locNodes [ ""
                                     , activeDescName $ _locDesc $ head _locNodes
                                     , intercalate ", " $ map (passiveDescName . _locDesc) _locNodes
                                     ]
               , "\n"
               ]


-- | Items | --

instance Describable Item where
    describe Item {_itemDesc = desc} =
        concat [ h2Desc desc
               , _descLong desc
               , "\n"
               ]


