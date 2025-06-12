{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Main where
import qualified Data.List as L
import Data.Array ((!)) 
import qualified Data.Array as Arr
import           Data.Ord
import           Control.Monad (when, void)
import           Data.Text (toUpper, Text, pack, unpack)
import           Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text.Encoding
import           Discord
import           Discord.Types
import qualified Discord.Requests as R

import Definitions
import Calculator
import Lookup
import Searcher

postSpells :: Message -> [Spell] -> DiscordHandler ()
postSpells message spells = do
  let spellTexts = map renderSpell $ L.nub spells
  let spellChunk = T.concat . L.intersperse "\n\n" $ spellTexts
  respond message spellChunk

postSummaries :: Message -> [Spell] -> DiscordHandler ()
postSummaries message spells = do
  let summaries = map spellSignature spells
  respond message (T.concat . L.intersperse "\n" $ summaries)

-- | Build MessageReference from Message
--   Might already exist, but I couldn't find it.
getReference :: Message -> MessageReference
getReference m = MessageReference {referenceMessageId = Just $ messageId m,
                                   referenceChannelId = Just $ messageChannelId m,
                                   referenceGuildId   = messageGuildId   m,
                                   failIfNotExists    = False
                                  }

respond :: Message -> Text -> DiscordHandler ()
respond message content = do
  let channel = messageChannelId message
  if T.length content <= 800
        then
            void $ restCall (R.CreateMessageDetailed channel (def {R.messageDetailedContent = content, R.messageDetailedReference = Just $ getReference message}))
        else
            void $ restCall (R.CreateMessageDetailed channel (def {R.messageDetailedFile= Just ("spell.md", encodeUtf8 content), R.messageDetailedReference = Just $ getReference message}))

autoLibrarian :: [Spell] ->  IO ()
autoLibrarian grimoire = do
    token <- TIO.readFile "tokenFile"
    userFacingError <- runDiscord $ def
             { discordToken = token
             , discordOnEvent = eventHandler grimoire
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             }
    TIO.putStrLn userFacingError


lookupMessage :: [Spell] -> Message -> DiscordHandler ()
lookupMessage grimoire m = do
  let matches = getMentionedSpells . unpack. messageContent $ m
      descriptions = map (lookupSpell grimoire . pack) matches
  postSpells m descriptions


searchMessage :: [Spell] -> Message -> DiscordHandler ()
searchMessage grimoire m = do
  let
    predicate = getPredicate . messageContent $ m
  case predicate of
    Right pred -> do
      let matching = filter pred grimoire
      postSummaries m matching
    Left message -> respond m $ T.pack message

calcMessage :: Message -> DiscordHandler ()
calcMessage m = do
  let reply = parseCalc.unpack.messageContent $ m
  void $ restCall (R.CreateMessage (messageChannelId m) (T.pack reply))

eventHandler :: [Spell] -> Event -> DiscordHandler ()
eventHandler grimoire event = case event of
    MessageCreate m -> when(not(fromBot  m)) $ do
        if "!calc" `T.isPrefixOf` messageContent m then calcMessage m
          else if "!find" `T.isPrefixOf` messageContent m then  searchMessage grimoire m
            else lookupMessage grimoire m
    _ignored -> do
        return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

main :: IO ()
main = do
    grimoire <- readGrimoire "grimoire.json"
    autoLibrarian grimoire
