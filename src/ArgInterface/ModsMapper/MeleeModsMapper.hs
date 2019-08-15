{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : ArgInterface.ModsMapper.MeleeModsMapper
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module contains functions that filter mods according
-- to the melee weapon given by the user.
-- So only the mods that can be applied to that perticular weapon
-- type will be returned.

module ArgInterface.ModsMapper.MeleeModsMapper where

import           ClassyPrelude
import           Data.FuzzySet                 as F

import           Builder.Mods.MeleeModsMetaList

import           Types.GenericWeapon

bestMatch :: Text -> Maybe Text
bestMatch stuff = map snd $ headMay $ F.get modSet stuff
{-# INLINE bestMatch #-}

modList :: [Text]
modList = toList $ fst <$> meleeModsMetaList dummyGenericWeapon Nothing
{-# INLINE modList #-}

modSet :: FuzzySet
modSet = F.fromList modList
{-# INLINE modSet #-}

modNameMapper' :: Text -> Int
modNameMapper' modName = fromMaybe 0 matchingElement
 where
  precedenceList = zip modList [0 ..] :: [(Text, Int)]
  matchingElement =
    snd <$> find (\(name, _) -> bestMatch modName == Just name) precedenceList

modNameMapper :: [Text] -> [Int]
modNameMapper = map modNameMapper'
{-# INLINE modNameMapper #-}
