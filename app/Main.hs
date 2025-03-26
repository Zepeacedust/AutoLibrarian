{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Data.List (minimumBy)
import Data.Array ((!)) 
import qualified Data.Array as Arr
import           Data.Ord
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


diff :: Text -> Text -> Int
diff a b = memo 0 0 where
    helperFunc :: Int -> Int -> Int
    helperFunc x y
        | T.index a x == T.index b y = modded
        | otherwise = minimum  [
            (1+subbed),
            (1+added)
            ]
        where
            modded = memo (x+1) (y+1)
            added  = memo (x)   (y+1)
            subbed = memo (x+1)   (y)
    memo x y
      | x >= lA = lB - y
      | y >= lB = lA - x
      | otherwise     = arr ! (x, y)
    arr = Arr.listArray bound [helperFunc x y | (x,y) <- Arr.range bound]
    bound = ((0,0),(lA, lB))
    lA = T.length a
    lB = T.length b

readGrimoire fileName = do
    file <- B.readFile fileName
    let Just grimoire = decode file :: Maybe [Spell]
    return grimoire


lookupSpell :: [Spell] -> Text -> Spell
lookupSpell grimoire target = minimumBy (comparing (diff (toUpper target) . name)) grimoire

postSpell ::ChannelId -> Spell -> DiscordHandler ()
postSpell channel spell = do
    let spellCont = renderSpell spell
    if T.length spellCont < 400 
        then
            void $ restCall (R.CreateMessage channel (renderSpell spell))
        else
            void $ restCall (R.CreateMessageDetailed channel (def {R.messageDetailedFile= Just ("spell.md", encodeUtf8 spellCont)}))


renderSpell :: Spell -> Text
renderSpell spell = "### " <> name spell<> "\n**" 
                            <> (fst.level$ spell) <> " " <> (snd.level $ spell) <> "**\n"
                            <> "**R:** " <> range spell <>", **D:** "<> duration spell <>", **T:** " <> target spell <> ", " <> T.intercalate ", " (tags spell) <> "\n" 
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
