{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Data.List (find)
import           Control.Monad (when, void)
import           UnliftIO.Concurrent
import           Data.Text (isPrefixOf, toUpper, Text, pack, unpack)
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as B
import           Data.Text.Encoding
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
    range:: Text,
    duration::Text,
    target::Text,
    tags::[Text],
    text::Text,
    source:: (Text,Int)
    } deriving Show
$(deriveJSON defaultOptions ''Spell)


readGrimoire fileName = do
    file <- B.readFile fileName
    let Just grimoire = decode file :: Maybe [Spell]
    return grimoire


lookupSpell :: [Spell] -> Text -> Maybe Spell
lookupSpell grimoire target = find (\s -> name s == toUpper target) grimoire

postSpell ::ChannelId -> Maybe Spell -> DiscordHandler ()
postSpell channel spell = do
    let spellCont = renderSpell spell
    if T.length spellCont < 2000 
        then
            void $ restCall (R.CreateMessage channel (renderSpell spell))
        else
            void $ restCall (R.CreateMessageDetailed channel (def {R.messageDetailedFile= Just ("spell.md", encodeUtf8 spellCont)}))


renderSpell :: Maybe Spell -> Text
renderSpell Nothing      = "Spell not found"
renderSpell (Just spell) = "### " <> name spell<> "\n**" 
                            <> (fst.level$ spell) <> " " <> (snd.level $ spell) <> "**\n"
                            <> "**R:** " <> range spell <>", **D:** "<> duration spell <>",**T:** " <> target spell <> ", " <> T.intercalate ", " (tags spell) <> "\n" 
                            <> text spell <> "\n> "
                            <> (fst.source$ spell)<> " " <> (T.show . snd.source $ spell)

autoLibrarian :: [Spell] ->  IO ()
autoLibrarian grimoire = do
    token <- TIO.readFile "tokenFile"
    userFacingError <- runDiscord $ def
             { discordToken = token
             , discordOnEvent = eventHandler grimoire
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             }
    TIO.putStrLn userFacingError

eventHandler :: [Spell] -> Event -> DiscordHandler ()
eventHandler grimoire event = case event of
    MessageCreate m -> when( not( fromBot  m)) $ do
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
