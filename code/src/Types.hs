{-# LANGUAGE RecordWildCards #-}

module Types where


import           Data.DateTime


data Game = Game { _hasQuit :: Bool
                 , _player  :: Player
                 , _world   :: World
                 , _time    :: DateTime
                 } deriving (Show, Eq)


data Player = Player { _inventory    :: Location
                     , _health       :: IntWithMax
                     , _lastLocation :: Maybe World
                     } deriving (Show, Eq)


data World = StartWorld { _loc :: Location } 
           | ComputerWorld { _loc :: Location } 
           | RealWorld { _loc :: Location } deriving (Show, Eq)


data Location = Location { _locDesc       :: Description
                         , _locRadMeters  :: Double
                         , _locItems      :: [Item]
                         , _locNodes      :: [Location]
                         } deriving (Show, Eq)



-- | Since this is a text based game we need a lot of description info.
-- The different fields will be used in different contexts.
data Description = Description { _descTitle :: String -- ^ The formal title
                               , _descName  :: String -- ^ An informal name
                               , _descShort :: String -- ^ A short description
                               , _descLong  :: String -- ^ A detailed description
                               , _descPfx   :: Prefix -- ^ Context based prefixes
                               } deriving (Show, Eq)


-- | Context based prefixes for describable things.
data Prefix = Prefix { _pfxActive  :: String -- ^ Eg. the, your
                     , _pfxPassive :: String -- ^ Eg. a
                     } deriving (Show, Eq)


data Item = Item { _itemDesc :: Description
                 } deriving (Show, Eq)


data LocatedItem = LocatedItem { _locatedIn    :: Location
                               , _locatedItem  :: Item
                               } deriving (Show, Eq)


data IntWithMax = IntWithMax Int Int deriving (Show, Eq)


data Event = GameStartEvent deriving (Show, Eq)


type Step = (Game -> IO Game)


