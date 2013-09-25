module Locations where

import           Types
import qualified Data.Map as M


emptyLocation :: Location
emptyLocation =
    Location { _locNodes = []
             , _locItems = []
             , _locRadMeters = 0
             , _locDesc = Description { _descTitle = "The Abyss"
                                      , _descName  = "abyss"
                                      , _descShort = "an infinite abyss"
                                      , _descLong  = concat [ "A void of unfathomable empty space, the abyss "
                                                            , "sprawls out in every direction. Every direction "
                                                            , "except inward, towards yourself."
                                                            ]
                                      , _descPfx   = Prefix { _pfxActive = "the"
                                                            , _pfxPassive = "an"
                                                            }
                                      }
             }


aHomeOfficeLocation :: Location
aHomeOfficeLocation =
    Location { _locDesc = aHomeOfficeDescription
             , _locRadMeters = 1.5
             , _locNodes = []
             , _locItems = []
             }

aHomeOfficeDescription :: Description
aHomeOfficeDescription = Description { _descTitle = "Home Office"
                                     , _descName  = "home office"
                                     , _descShort = "an average home office"
                                     , _descLong  = "This is a room in a house where work is done."
                                     , _descPfx   =
                                         Prefix { _pfxActive  = "the"
                                                , _pfxPassive = "a"
                                                }
                                     }



yourHomeOfficeLocation :: Location
yourHomeOfficeLocation = aHomeOfficeLocation { _locDesc = yourHomeOfficeDescription }


yourHomeOfficeDescription :: Description
yourHomeOfficeDescription = aHomeOfficeDescription { _descTitle = "Your Home Office"
                                                   , _descLong  = "The room in your house where you do your work."
                                                   }


homeOfficeNodes :: M.Map String Location
homeOfficeNodes = M.fromList [ ("hallway", homeHallwayLocation)
                             ]


homeHallwayLocation :: Location
homeHallwayLocation = emptyLocation
homeBathroomLocation :: Location
homeBathroomLocation = emptyLocation
homeLivingRoomLocation :: Location
homeLivingRoomLocation = emptyLocation
homeBedroomLocation :: Location
homeBedroomLocation = emptyLocation

