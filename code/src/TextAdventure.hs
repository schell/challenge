module Main where

import           Types
import           Locations
import           Parser.Parser
import           Actions
import           System.IO
import           Data.DateTime
import           Data.Time.Format
import           System.Locale
import           Control.Monad  ( when )
import           Data.Char      ( toLower )



main :: IO ()
main = do
    let g = emptyGame
    putStrLn "Hello. Type `quit` at any time to quit."
    _ <- stepIO _hasQuit step g
    putStrLn "Goodbye."


emptyGame :: Game
emptyGame = Game { _hasQuit = False
                 , _player  = emptyPlayer
                 , _world   = emptyWorld
                 , _time    = fromGregorian 1994 4 5 23 1 1
                 }


emptyPlayer :: Player
emptyPlayer = Player { _inventory = emptyLocation
                     , _health = IntWithMax 100 100
                     , _lastLocation = Nothing
                     }


emptyWorld :: World
emptyWorld = RealWorld { _loc = yourHomeOfficeLocation }


-- | Takes a predicate, a stepping function and a
-- game state and loops the stepping function over
-- the game state while the predicate evaluates false.
stepIO :: (Game -> Bool) -- ^ The predicate.
       -> Step           -- ^ The stepping function.
       -> Game           -- ^ The game state.
       -> IO Game
stepIO p s g
    | p g       = return g
    | otherwise = s g >>= stepIO p s


-- | The stepping function.
step :: Step
step g = do
    g' <- case _world g of
              StartWorld{} -> do showStartScreen
                                 return $ g { _world = RealWorld yourHomeOfficeLocation
                                            , _time = addSeconds 1 $ _time g
                                            }
              _          -> return g

    hFlush stdout
    cmd <- fmap (fmap toLower) getLine

    putStrLn ""

    if cmd == "quit"
      then return $ g { _hasQuit = True }
      else do g'' <- case parseAction (_world g') cmd of
                         Right a's -> runActions a's g'
                         Left m    -> do putStrLn m
                                         putStrLn cmd
                                         return g'

              when (_time g /= _time g'') $
                  putStrLn $ formatTime defaultTimeLocale "%l:%M:%S%P %B %Y" $ _time g
              let l  = _loc $ _world g
                  l' = _loc $ _world g''
              when (l /= l') $
                  print l'
              return g''


showStartScreen :: IO ()
showStartScreen = putStr $ concat
    [ "You are sitting at a computer, its flickering green glow is dim but hypnotic. You can't remember the last time you worked on a machine this old. Where did it come from?\n"
    , "The text on the screen says:\n"
    , "    Welcome to TextAdventure.\n"
    , "    Type `help` at any time for a description of your options.\n"
    , "    >"
    ]


