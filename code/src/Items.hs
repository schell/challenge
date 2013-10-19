module Items where

import Types


computerItem :: Item
computerItem =
    Item { _itemDesc = Description { _descTitle = "Green Screen Computer"
                                   , _descName  = "computer"
                                   , _descShort = "an old computer"
                                   , _descLong  = concat [ "It seems well used. "
                                                         , "The plastic housing has yellowed over the years, "
                                                         , "leaving you with the suspicion that it's about ten "
                                                         , "years old. It has a modem though, which is pretty cool."
                                                         ]
                                   , _descPfx   = Prefix { _pfxActive  = "the"
                                                         , _pfxPassive = "a"
                                                         }
                                   }
         , _itemRadius = 0.1
         }
