{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ArgInterface.ArgInterface
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module contains functions that parse userinput into ArgsParse data.

module ArgInterface.ArgInterface
  ( parseArgs
  , ArgsParse(..)
  )
where

import qualified ArgInterface.WeaponDetailsIdentifier
                                               as WI
import           ClassyPrelude
import           Data.Char                      ( isAlpha )
import           Options.Applicative

-- | ArgsParse is the type for optparse-applicative to parse
--   userinput into.
data ArgsParse = ArgsParse { weaponName   :: Maybe (Text, Text) -- Name of the weapon to build
                           , neededMods   :: Maybe [Text] -- Mods to fit on the weapon
                           , unneededMods :: Maybe [Text] -- Mods to ignore when autobuilding
                           , multiplier1  :: Maybe Float -- A generic multiplier. Can be zoom level or melee multiplier
                           , multiplier2  :: Maybe Float -- Another generic multiplier. Can be damage muliplier from previous multiplier
                           , comparator   :: Maybe Text -- A comparator function's name. Used for comparing builds when autobuilding.
                           } deriving (Show, Generic)

-- | Entry point for user input parser.
parseArgs :: IO ArgsParse
parseArgs = execParser opts
 where
  opts = info
    (argParser <**> helper)
    (fullDesc <> progDesc "Warframe Builder" <> header
      "A tool for simulating builds."
    )

-- | ArgsParse type applied on all parsers.
argParser :: Parser ArgsParse
argParser =
  ArgsParse
    <$> optional weaponNameParser
    <*> optional neededModsParser
    <*> optional unneededModsParser
    <*> optional multiplier1Parser
    <*> optional multiplier2Parser
    <*> optional comparatorParser

-- | Parses weapon's name.
weaponNameParser :: Parser (Text, Text)
weaponNameParser = WI.weaponDetailsIdentifier <$> strOption
  (long "weapon" <> short 'w' <> metavar "WEAPON" <> help "Weapon name")

-- | Parses required mods.
neededModsParser :: Parser [Text]
neededModsParser =
  splitWhen (== ',') . filter (\a -> isAlpha a || a == ',') <$> strOption
    (long "build" <> short 'b' <> metavar "MODS" <> help
      "List of mods to be applied"
    )

-- | Parses ignored mods.
unneededModsParser :: Parser [Text]
unneededModsParser =
  splitWhen (== ',') . filter (\a -> isAlpha a || a == ',') <$> strOption
    (long "ignore" <> short 'i' <> metavar "IGNORED MODS" <> help
      "List of mods NOT to be applied"
    )

-- | Parses basic multiplier.
multiplier1Parser :: Parser Float
multiplier1Parser = option
  auto
  (  long "multiplier1"
  <> short 'm'
  <> metavar "MULTIPLIER1"
  <> help
       "A multiplier to be applied to the build (Can be combo counter) Mod Scaler"
  )

-- | Parses generic multiplier.
multiplier2Parser :: Parser Float
multiplier2Parser = option
  auto
  (  long "multiplier2"
  <> short 'g'
  <> metavar "MULTIPLIER2"
  <> help
       "A multiplier to be applied to the build (Generic) Build Scaler. For Condition Overload, this multiplier should be (1.6 ^ number of status effects) * combo counter"
  )

-- | Parses comparator.
comparatorParser :: Parser Text
comparatorParser = strOption
  (long "comparator" <> short 'c' <> metavar "COMPARATOR" <> help
    "A comparator to be applied to the build"
  )
