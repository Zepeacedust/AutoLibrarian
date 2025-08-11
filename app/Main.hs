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

postThings :: Message -> [Postable] -> DiscordHandler ()
postThings message things = do
  let spellTexts = map render $ L.nub things
  let spellChunk = T.concat . L.intersperse "\n\n" $ spellTexts
  respond message spellChunk

postSummaries :: Message -> [Spell] -> DiscordHandler ()
postSummaries message spells = do
  let summaries = map sign spells
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

autoLibrarian :: [Spell] -> [Merit] ->  IO ()
autoLibrarian grimoire merits = do
    token <- TIO.readFile "tokenFile"
    userFacingError <- runDiscord $ def
             { discordToken = token
             , discordOnEvent = eventHandler grimoire merits
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             }
    TIO.putStrLn userFacingError


lookupMessage :: [Spell] -> [Merit] -> Message -> DiscordHandler ()
lookupMessage grimoire merits m = do
  let matches = getMentionedSpells . unpack . messageContent $ m
      descriptions = map (PostableSpell . lookupSpell grimoire . pack) matches
  let meritMatches = getMentionedMerits . unpack . messageContent $ m
      meritText = map (PostableMerit . lookupVirtue merits . pack) meritMatches
  postThings m (descriptions ++ meritText)


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

eventHandler :: [Spell] -> [Merit] -> Event -> DiscordHandler ()
eventHandler grimoire merits event = case event of
    MessageCreate m -> when(not(fromBot  m)) $ do
        if "!calc" `T.isPrefixOf` messageContent m then calcMessage m
          else if "!find" `T.isPrefixOf` messageContent m then  searchMessage grimoire m
            else lookupMessage grimoire merits m
    _ignored -> do
        return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

main :: IO ()
main = do
    grimoire <- readJson "grimoire.json"
    merits <- readJson "merits.json"
    autoLibrarian grimoire merits
