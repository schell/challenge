{-# LANGUAGE OverloadedStrings #-}
module Parser.Parser where

import           Types
import           Parser.Types
import           Data.Attoparsec.Text
import           Control.Applicative  ( optional, (<$>), (<|>) )
import           Data.Char
import qualified Data.Text  as T


parseAction :: World -> String -> Either String [Action]
parseAction w =
    let action = case w of
                     ComputerWorld{} -> computerAction
                     _               -> physicalAction
    in parseOnly (filter (/= Unknown) <$> manyTill (action <|> unknown) endOfInput) . T.pack


-- | Actions | --

unknown :: Parser Action
unknown = do
    skipWhile (not . isSpace)
    optional space
    return Unknown


computerAction :: Parser Action
computerAction = ComputerAction <$> computerVerb


physicalAction :: Parser Action
physicalAction = PhysicalAction <$> physicalVerb


-- | Verbs | --


verbOptSpace :: T.Text -> b -> Parser b
verbOptSpace s c = do
    string s
    optional space
    return c


computerVerb :: Parser ComputerVerb
computerVerb = verbHelpCmd <|> verbLogoutCmd


verbHelpCmd :: Parser ComputerVerb
verbHelpCmd = verbOptSpace "help" HelpCmd


verbLogoutCmd :: Parser ComputerVerb
verbLogoutCmd = verbOptSpace "logout" LogoutCmd


physicalVerb :: Parser PhysicalVerb
physicalVerb =
    verbHelp   <|>
    verbLookAt <|>
    verbLook   <|>
    verbWalkTo


verbHelp :: Parser PhysicalVerb
verbHelp = verbOptSpace "help" Help


verbLook :: Parser PhysicalVerb
verbLook = verbOptSpace "look" Look


verbLookAt :: Parser PhysicalVerb
verbLookAt = do
    string "look "
    skipOptionalWord "at"
    skipOptionalWord "the"
    n <- noun
    return $ LookAt n


verbWalkTo :: Parser PhysicalVerb
verbWalkTo = do
    string "walk "
    skipOptionalWord "to"
    skipOptionalWord "the"
    n <- noun
    return $ WalkTo n

skipOptionalWord :: T.Text -> Parser ()
skipOptionalWord s = do
    optional $ skipWhile isSpace
    optional $ string s
    optional $ skipWhile isSpace
    return ()


noun :: Parser Noun
noun =
    yourNoun <|>
    selfNoun <|>
    Noun . T.unpack <$> Data.Attoparsec.Text.takeWhile (not . isSpace)

selfNoun :: Parser Noun
selfNoun = do
    string "self" <|> string "myself"
    return Self

yourNoun :: Parser Noun
yourNoun = do
    string "your"
    n <- noun
    return $ Your n

