{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Definitions where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Aeson.TH
import Data.Aeson


data Level = Var !Int | Gen deriving (Show, Eq)
instance FromJSON Level where
  parseJSON (String t) = return $ if t == "Gen" then Gen else Var . read . T.unpack $ t
  parseJSON invalid =prependFailure "parsing Level failed, "
            (typeMismatch "Object" invalid)
instance ToJSON Level where
  toJSON = String . T.pack . show
  toEncoding = toEncoding . String . T.pack . show

instance Ord Level where
  compare Gen _ = EQ
  compare _ Gen = EQ
  compare (Var a) (Var b) = compare a b

data TypeSig = TypeSig {
  technique :: !T.Text,
  techReq   :: ![T.Text],
  form      :: !T.Text,
  formReq   :: ![T.Text],
  level     :: !Level
  } deriving (Show, Eq)

$(deriveJSON defaultOptions ''TypeSig)

data Spell = Spell {
    name::     !T.Text,
    typeSig::  !TypeSig,
    range::    !T.Text,
    duration:: !T.Text,
    target::   !T.Text,
    tags::     ![T.Text],
    text::     !T.Text,
    source::   !(T.Text,Int)
    } deriving (Show, Eq)
$(deriveJSON defaultOptions ''Spell)

data Merit = Merit {
    meritAllowMultiple :: !(T.Text),
    meritApplicableTo  :: !(T.Text),
    meritCategory      :: !(T.Text),
    meritDesc          :: !(T.Text),
    meritName          :: !(T.Text),
    meritRestriction   :: !(T.Text),
    -- meritExclude       :: !(T.Text),
    meritSource        :: !(T.Text),
    meritType          :: !(T.Text),
    meritValue         :: !(Integer)
    } deriving (Show, Eq)
$(deriveJSON defaultOptions ''Merit)

data Postable = PostableSpell !Spell | PostableMerit !Merit deriving Eq

class NicePrintable record where
  render :: record -> T.Text
  sign   :: record -> T.Text

instance NicePrintable Spell where
  render spell = "### " <> name spell <> "\n**"
                            <> sign spell <> "**\n"
                            <> "**R:** " <> range spell <>", **D:** "<> duration spell <>", **T:** " <> target spell <> ", " <> T.intercalate ", " (tags spell) <> "\n"
                            <> text spell <> "\n> "
                            <> (fst.source$ spell)<> " " <> (T.show . snd.source $ spell)

  sign spell = name spell <> " " <> ( renderSig . typeSig $ spell ) <> " " <> T.intercalate ", " (tags spell)

getMagnitude :: Integer -> T.Text
getMagnitude m = case m of
  1 -> "Minor"
  3 -> "Major"
  _ -> "Strange"
instance NicePrintable Merit where
  render m = "### " <> meritName m <> "\n**" <> (getMagnitude . meritValue) m <> " " <> meritCategory m <> " " <> meritType m <> "**\n" <> meritDesc m <> "\n> " <> meritSource m
  sign   = meritName

instance NicePrintable Postable where
  render (PostableSpell spell) = render spell
  render (PostableMerit merit) = render merit
  sign   (PostableSpell spell) = sign spell
  sign   (PostableMerit merit) = sign merit

renderSig :: TypeSig -> T.Text
renderSig sig = te <> fo <> " " <> renderLevel (level sig) <> " " where
  te = technique sig <> (if null . techReq $ sig then "" else "(" <> (T.concat . L.intersperse ", " $ techReq sig) <> ")")
  fo = form sig      <> (if null . formReq $ sig then "" else "(" <> (T.concat . L.intersperse ", " $ formReq sig) <> ")")

renderLevel :: Level -> T.Text
renderLevel  Gen    = "Gen"
renderLevel (Var n) = T.pack . show $ n

readJson :: FromJSON a => String -> IO [a]
readJson fileName = do
    file <- B.readFile fileName
    case eitherDecode file of
      Right grimoire -> return grimoire
      Left err -> do
        error err
