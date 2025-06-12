{-#Language OverloadedStrings#-}
module Searcher where

import qualified Data.Text as T
import Definitions
import Text.Parsec.Prim
import Text.Parsec
import Data.Maybe (fromMaybe)

import Text.Read (readMaybe)


spaceWrapped :: Parsec String () a -> Parsec String () a
spaceWrapped inner = do
  spaces
  out <- inner
  spaces
  return out

getPredicate :: T.Text -> Either String (Spell -> Bool)
getPredicate text = case parse parseDisjunct "" searchString of
  Right pred -> Right pred
  Left error -> Left . show $ error
  where
    searchString = T.unpack . fromMaybe text .T.stripPrefix "!find" $ text

parseDisjunct :: Parsec String () (Spell -> Bool)
parseDisjunct = spaceWrapped $ do
  lhs <- parseConjunct
  others <- many $ do
    _ <- spaceWrapped . string' $ "||"
    parseConjunct
  return $ foldr (\a b spell ->  a spell || b spell) lhs others


parseConjunct :: Parsec String () (Spell -> Bool)
parseConjunct = spaceWrapped $ do
  conjuncts <- many parsePredicate
  return $ foldl (\a b spell -> a spell && b spell) (const True) conjuncts

parsePredicate :: Parsec String () (Spell -> Bool)
parsePredicate = parseNegation <|> parseParens <|> parseFilter


parseNegation :: Parsec String () (Spell -> Bool)
parseNegation = spaceWrapped $ do
  _ <- char '!'
  inner <- parseFilter <|> parseParens
  return $ \spell -> not.inner $ spell

parseParens :: Parsec String () (Spell -> Bool)
parseParens = spaceWrapped $ do
  _ <- char '('
  inner <- parseDisjunct
  _ <- char ')'
  return inner

data Relation = Has | LessThan | LessEq | GreaterThan | GreaterEq | Is deriving (Eq, Show)

data Attribute = Technique | Form | Level | Range | Duration | Target | Tag | Source deriving (Eq, Show)

ifThen :: String -> a -> Parsec String () a
ifThen s out = do
  _ <- string' s
  return out

parseFilter :: Parsec String () (Spell -> Bool)
parseFilter = spaceWrapped $ do
  attributeName <- many1 letter
  attribute <- case attributeName of -- need to find better way to do this, it's horrible
        "te" -> return Technique
        "Te" -> return Technique
        "Technique" -> return Technique
        "technique" -> return Technique
        "fo" -> return Form
        "Fo" -> return Form
        "form" -> return Form
        "Form" -> return Form
        "l" -> return Level
        "L" -> return Level
        "level" -> return Level
        "Level" -> return Level
        "R" -> return Range
        "r" -> return Range
        "range" -> return Range
        "Range" -> return Range
        "d" -> return Duration
        "D" -> return Duration
        "Duration" -> return Duration
        "duration" -> return Duration
        "t" -> return Target
        "T" -> return Target
        "Target" -> return Target
        "target" -> return Target
        "Tag" -> return Tag
        "tag" -> return Tag
        "s" -> return Source
        "S" -> return Source
        "Source" -> return Source
        "source" -> return Source
        invalid -> fail ("Invalid attribute: " ++ invalid)

  relation <- choice $ uncurry ifThen <$> [("<=", LessEq), ("<", LessThan), (">=", GreaterEq), (">", GreaterThan), ("=", Is), (":", Has)]
  operand <- many1 alphaNum
  case matchRelation relation attribute operand of
    Right predicate -> return predicate
    Left errorMsg -> fail errorMsg



matchRelation :: Relation -> Attribute -> String -> Either String (Spell -> Bool)
matchRelation Has Technique operand     = Right $ \spell -> ((== T.pack operand) . technique . typeSig $ spell) || T.pack operand `elem` (techReq . typeSig $ spell)
matchRelation Is Technique operand      = Right $ \spell -> (== T.pack operand).technique . typeSig $ spell
matchRelation _other Technique _operand = Left "invalid selector for technique"

matchRelation Has Form operand     = Right $ \spell -> ((== T.pack operand) . form . typeSig $ spell) || T.pack operand `elem` (formReq . typeSig $ spell)
matchRelation Is Form operand      = Right $ \spell ->  (== T.pack operand) . form . typeSig $ spell
matchRelation _other Form _operand = Left "invalid selector for form"



matchRelation Has         Level "gen" = Right $ \spell -> (level . typeSig $ spell) == Gen
matchRelation Is          Level "gen" = Right $ \spell -> (level . typeSig $ spell) == Gen
matchRelation Has         Level "Gen" = Right $ \spell -> (level . typeSig $ spell) == Gen
matchRelation Is          Level "Gen" = Right $ \spell -> (level . typeSig $ spell) == Gen
matchRelation _other      Level "gen" = Left "Invalid selector for general level"
matchRelation _other      Level "Gen" = Left "Invalid selector for general level"

matchRelation Has         Level operand = case readMaybe operand of
  Just val -> Right $ \spell -> (level . typeSig $ spell) == Var val
  Nothing  -> Left "Improper number"
matchRelation Is          Level operand = case readMaybe operand of
  Just val -> Right $ \spell -> (level . typeSig $ spell) == Var val
  Nothing  -> Left "Improper number"
matchRelation GreaterThan Level operand = case readMaybe operand of
  Just val -> Right $ \spell -> (level . typeSig $ spell) > Var val
  Nothing  -> Left "Improper number"
matchRelation GreaterEq   Level operand = case readMaybe operand of
  Just val -> Right $ \spell -> (level . typeSig $ spell) >= Var val
  Nothing  -> Left "Improper number"
matchRelation LessThan    Level operand = case readMaybe operand of
  Just val -> Right $ \spell -> (level . typeSig $ spell) < Var val
  Nothing  -> Left "Improper number"
matchRelation LessEq      Level operand = case readMaybe operand of
  Just val -> Right $ \spell -> (level . typeSig $ spell) <= Var val
  Nothing  -> Left "Improper number"

matchRelation Has Range operand     = Right $ \spell -> (== T.pack operand) . range $ spell
matchRelation Is Range operand      = Right $ \spell -> (== T.pack operand) . range $ spell
matchRelation _other Range _operand = Left "invalid selector for Range"

matchRelation Has Duration operand     = Right $ \spell -> (== T.pack operand) . duration $ spell
matchRelation Is Duration operand      = Right $ \spell -> (== T.pack operand) . duration $ spell
matchRelation _other Duration _operand = Left "invalid selector for Duration"

matchRelation Has Target operand     = Right $ \spell -> (== T.pack operand) . target $ spell
matchRelation Is Target operand      = Right $ \spell -> (== T.pack operand) . target $ spell
matchRelation _other Target _operand = Left "invalid selector for Target"

matchRelation Has Tag operand     = Right $ \spell -> T.pack operand `elem` tags spell
matchRelation Is  Tag operand     = Right $ \spell -> T.pack operand `elem` tags spell
matchRelation _other Tag _operand = Left "invalid selector for Tag"

matchRelation Has Source operand     = Right $ \spell -> ((== T.pack operand) . form . typeSig $ spell) || T.pack operand `elem` (formReq . typeSig $ spell)
matchRelation Is Source operand      = Right $ \spell ->  (== T.pack operand) . form . typeSig $ spell
matchRelation _other Source _operand = Left "invalid selector for Source"
