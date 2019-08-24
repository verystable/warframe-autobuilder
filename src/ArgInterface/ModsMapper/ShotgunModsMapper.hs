{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : ArgInterface.ModsMapper.PistolModsMapper
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module contains functions that filter mods according
-- to the primary weapon given by the user.
-- So only the mods that can be applied to that perticular weapon
-- type will be returned.

module ArgInterface.ModsMapper.ShotgunModsMapper where

import           ClassyPrelude
import           Data.FuzzySet                 as F

import           Builder.Mods.ShotgunModsMetaList

import           Types.GenericWeapon

-- | A generic best-matcher.
--   Matches a given mod name to corrected mod's name.
bestMatch :: Text -> Maybe Text
bestMatch stuff = map snd $ headMay $ F.get modSet stuff
{-# INLINE bestMatch #-}

-- | A Shotgun mods name list.
modList :: [Text]
modList = toList $ fst <$> shotgunModsMetaList dummyGenericWeapon

-- | Shotgun Mods list.
modSet :: FuzzySet
modSet = F.fromList modList

-- | Maps mod name to mod index.
modNameMapper' :: Text -> Int
modNameMapper' modName = fromMaybe 0 matchingElement
 where
  precedenceList = zip modList [0 ..] :: [(Text, Int)]
  matchingElement =
    snd <$> find (\(name, _) -> bestMatch modName == Just name) precedenceList

-- | Maps list of mod names to their indices.
modNameMapper :: [Text] -> [Int]
modNameMapper = map modNameMapper'
