{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Data.List (find)
import           Control.Monad (when, void)
import           UnliftIO.Concurrent
import           Data.Text (isPrefixOf, toLower, Text, pack, unpack)
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as B

import           Discord
import           Discord.Types
import qualified Discord.Requests as R

import Text.Regex.Posix

spellRegex :: String
spellRegex = "\\[\\[([^]]*)\\]\\]"

getMentionedSpells :: String -> [String]
getMentionedSpells input = 
    let  
         strLs = (input =~ spellRegex) :: [[String]]
    in  map (head . tail) strLs

data Level = Var Int | Gen deriving Show
$(deriveJSON defaultOptions ''Level)
data Spell = Spell {
    name::Text,
    level:: (Text, Text),
    parameters::Text,
    text::Text,
    source:: (Text,Int)
    } deriving Show
$(deriveJSON defaultOptions ''Spell)


readGrimoire fileName = do
    file <- B.readFile fileName
    let Just grimoire = decode file :: Maybe [Spell]
    return grimoire


lookupSpell :: [Spell] -> Text -> Maybe Spell
lookupSpell grimoire target = find (\s -> name s == target) grimoire

postSpell ::ChannelId -> Maybe Spell -> DiscordHandler ()
postSpell channel spell = do
    void $ restCall (R.CreateMessage channel (pack.show $ spell))

-- | Replies "pong" to every message that starts with "ping"
-- autoLibrarian :: IO ()
autoLibrarian grimoire = do
    token <- TIO.readFile "tokenFile"
    userFacingError <- runDiscord $ def
             { discordToken = token
             , discordOnEvent = eventHandler grimoire
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             }
    TIO.putStrLn userFacingError

-- eventHandler :: Event -> DiscordHandler ()
eventHandler grimoire event = case event of
    MessageCreate m -> when( not( fromBot  m)) $ do
        void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "book")
        let matches = getMentionedSpells . unpack. messageContent $ m
            descriptions = map (lookupSpell grimoire . pack) matches
        sequence.map (postSpell (messageChannelId m)) $  descriptions
        return ()
    _ -> do
        return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

main :: IO ()
main = do
    grimoire <- readGrimoire "grimoire.json"
    autoLibrarian grimoire
