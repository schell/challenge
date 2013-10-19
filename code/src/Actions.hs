{-# LANGUAGE RecordWildCards #-}

module Actions where

import           Parser.Types
import           Types
import           Description
import           Data.DateTime
import           Control.Applicative ( (<|>) )


runActions :: [Action] -> Step
runActions [] g      = return g

runActions (PhysicalAction Look:a's) g = do
    g' <- lookWorld g
    runActions a's g'

runActions (PhysicalAction (WalkTo noun):a's) g = do
    g' <- case noun of
              Noun loc -> walkToLoc loc g
              Your loc -> walkToYourLoc loc g
              Self     -> howAmINotMyself g
    runActions a's g'

runActions (PhysicalAction (LookAt noun):a's) g = do
    g' <- case noun of
              Noun n    -> lookAt n g
              Self      -> lookAtSelf g
              Your Self -> lookAtSelf g
              Your n    -> lookAtYour n g
    runActions a's g'

runActions (PhysicalAction Help:a's) g = do
    g' <- help g
    runActions a's g'

runActions _ g = putStrLn "That action has not been created yet." >> return g


help :: Step
help g = putStrLn helpString >> return g
    where helpString = "You are in the physical world and can " ++
                       "look at things, walk to places and ask for help."



lookAt :: String -> Step
lookAt name g = do
    let mLocItem = locateItem name (_loc $ _world g) <|>
                   locateItem name (_inventory $ _player g)

    case mLocItem of
        Just item -> lookAtLocatedItem item
        Nothing   -> putStrLn $ "You look around but can't find " ++ name

    return g { _time = addSeconds 3 $ _time g }


locateItem :: String -> Location -> Maybe LocatedItem
locateItem name l@Location{_locItems = items} =
    case filter (\Item{_itemDesc=desc} -> matchesDescription name desc) items of
        []    -> Nothing
        (i:_) -> Just LocatedItem { _locatedIn   = l
                                  , _locatedItem = i
                                  }


lookAtLocatedItem :: LocatedItem -> IO ()
lookAtLocatedItem (LocatedItem Location{..} i@Item{..}) =
    putStrLn $ concat [ "You find "
                      , passiveDescName _itemDesc
                      , " in "
                      , passiveDescName _locDesc
                      , "\n"
                      , describe i
                      ]


walkToLoc :: String -> Step
walkToLoc loc g = do
    let w = _world g
    case w of
        RealWorld{_loc = curLoc} ->
            case locateLocation loc curLoc  of
                Just location' -> do let d = _locRadius curLoc * 2
                                     putStrLn $ "You walk "++ show d ++" meters."
                                     return $ g { _world = RealWorld location'
                                                , _time = addSeconds (floor d) $ _time g
                                                }
                Nothing        -> do putStrLn $ "There is no location called " ++ loc
                                     return g
        _                  -> return g

walkToYourLoc :: String -> Step
walkToYourLoc loc g = do
    let w = _world g  


locateLocation :: String -> Location -> Maybe Location
locateLocation name loc =
    case filter (matchesDescription name . _locDesc) $ _locNodes loc of
        []  -> Nothing
        l:_ -> Just l


locateLocationFromGame :: String -> Game -> Maybe Location
locateLocationFromGame 

quitWorld :: Step
quitWorld g = return $ g { _hasQuit = True }


passWorld :: Step
passWorld g = do
    putStrLn "You remain idle for five seconds, just thinking about nothing."
    return g { _time = addSeconds 5 $ _time g }


lookWorld :: Step
lookWorld g = do
    putStrLn $ describe $ _world g
    return g


quitComputer :: Step
quitComputer _ = undefined


--helpComputer :: Step
--helpComputer g = do
--    putStr $ concat [ "    Commands: "
--                    , unwords $ M.keys $ commandMap $ _world g
--                    , "\n"
--                    , "    >"
--                    ]
--    return g


--thinkWorld :: Step
--thinkWorld g = do
--    putStrLn $ concat [ "You think about what you're doing, who and where you are. You decide you can safely "
--                      , intercalate ", " $ M.keys $ commandMap $ _world g
--                      , " or do nothing. There's always the option of inaction."
--                      ]
--    return g


