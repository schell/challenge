{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Arrows #-}
module Main where

import Prelude hiding (putStrLn, getLine)
import Control.Applicative
import Control.Monad
import Control.Monad.State.Strict
import Data.Char
import Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Data.Map (Map)
import qualified Data.Map as M
import Data.Hashable
import System.Exit
import System.Environment

main :: IO ()
main = do
    args  <- getArgs
    debug <- case args of
        "debug":_ -> return True
        _         -> return False
    runGame emptyGame{ gameIsDebug = debug }
        where runGame g = execStateT loop g >>= runGame

render :: Game -> IO ()
render = putStrLn . ("    " `append`) . intercalate "\n    " . gameMsgs

loop :: GameT ()
loop = do
    g <- get
    liftIO $ render g
    put g{ gameMsgs = [] }

    i <- liftIO $ getInput
    applyInput i

applyInput :: Input -> GameT ()
applyInput Quit = do
    addMsg "goodbye"
    get >>= liftIO . render
    liftIO exitSuccess
applyInput (Err e) = do
    debug <- gets gameIsDebug
    when debug $ liftIO $ putStrLn $ "! " `append` e
applyInput Look = do
   pos <- gets (playerPosition . gamePlayer)
   wld <- gets gameWorld
   case M.lookup pos wld of
       Nothing -> addMsg $ B.unwords ["You are falling (or are you floating?)"
                                     ,"through an infinite void."
                                     ]
       Just room -> addMsg $ roomDescription room
applyInput Pass = return ()

addMsg :: ByteString -> GameT ()
addMsg m = modify $ \g@Game{ gameMsgs = ms} -> g{ gameMsgs = ms ++ [m] }

emptyGame :: Game
emptyGame = Game { gamePlayer = emptyPlayer
                 , gameWorld = emptyWorld
                 , gameMsgs = ["Welcome to Caltrops"]
                 , gameIsDebug = False
                 }

emptyPlayer :: Player
emptyPlayer = Player { playerPosition = (0,0,0)
                     , playerBag = mempty
                     }

emptyWorld :: Map (Int,Int,Int) Space
emptyWorld = mk 0 0 0 (Room "Starting room") mempty
    where mk x y z s = M.insert (x,y,z) s

getInput :: IO Input
getInput = do
    ln <- getLine
    return $ case parseOnly input ln of
        Left err -> Err $ pack err
        Right i  -> i

input :: Parser Input
input = wait <|>
        look <|>
        move <|>
        quit <?> "input"

wait :: Parser Input
wait = Pass <$ string "wait" <?> "wait"

look :: Parser Input
look = Look <$ string "look" <?> "look"

quit :: Parser Input
quit = Quit <$ string "quit" <?> "quit"

move :: Parser Input
move = Move <$> direction <?> "move"

direction :: Parser Direction
direction = up    <|>
            down  <|>
            north <|>
            east  <|>
            south <|>
            west  <?> "direction"
        where up = Up <$ string "up" <?> "up"
              down = Down <$ string "down" <?> "down"
              north = North <$ string "north" <?> "north"
              east = East <$ string "east" <?> "east"
              south = South <$ string "south" <?> "south"
              west = West <$ string "west" <?> "west"

type GameT = StateT Game IO
data Input = Pass | Quit | Look | Move Direction | Err ByteString deriving (Show, Eq)
data Direction = Up | Down | North | East | South | West deriving (Show, Eq)
data Game = Game { gamePlayer :: Player
                 , gameWorld  :: Map (Int,Int,Int) Space
                 , gameMsgs   :: [ByteString]
                 , gameIsDebug:: Bool
                 }
data Player = Player { playerPosition :: (Int, Int, Int)
                     , playerBag :: Map ByteString Object
                     } deriving (Eq, Show)
data Space = Room { roomDescription :: ByteString } deriving (Show, Eq)

instance Show Object where
    show (Object s) = show s

instance Eq Object where
    a == b = hash a == hash b

instance Hashable Object where
    hashWithSalt s (Object o) = s `hashWithSalt` o

data Object where
    Object :: (Hashable a, Show a) => a -> Object
