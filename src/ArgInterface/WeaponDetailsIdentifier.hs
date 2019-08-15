{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : ArgInterface.PistolWeapons
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module contains all the functions used
-- to determine if the weapon name given by the user
-- belongs to a rifle, or a shotgun
-- or a pistol or a melee weapon

module ArgInterface.WeaponDetailsIdentifier where

import           ClassyPrelude
import           Data.FuzzySet                 as F

import           ArgInterface.MeleeWeapons
import           ArgInterface.PistolWeapons
import           ArgInterface.RifleWeapons
import           ArgInterface.ShotgunWeapons

-- | Given a weapon name, returns its name and type.
weaponDetailsIdentifier :: Text -> (Text, Text)
weaponDetailsIdentifier wep = (name, weaponTypeIdentifier name)
  where name = weaponNameIdentifier wep

-- | Corrects the name of a weapon. Defaults to 'Amprex' if failed.
weaponNameIdentifier :: Text -> Text
weaponNameIdentifier wep =
  fromMaybe "Amprex"
    $   betterMatch' (bestMatch' wep rifleSet)
    <|> betterMatch' (bestMatch' wep pistolsSet)
    <|> betterMatch' (bestMatch' wep shotgunSet)
    <|> betterMatch' (bestMatch' wep meleeSet)

-- | This function checks if the name belongs to
--   any of the available weapon category.
--   There has to be an 85% match.
weaponCategoryIdentifier :: Text -> Text
weaponCategoryIdentifier wep
  | bestMatch wep rifleSet > Just 0.85   = "Primary_Weapons"
  | bestMatch wep shotgunSet > Just 0.85 = "Primary_Weapons"
  | bestMatch wep meleeSet > Just 0.85   = "Melee_Weapons"
  | bestMatch wep pistolsSet > Just 0.85 = "Secondary_Weapons"
  | otherwise                            = "Primary_Weapons"
{-# INLINE weaponCategoryIdentifier #-}

-- | Matches the weapon name with its type.
weaponTypeIdentifier :: Text -> Text
weaponTypeIdentifier wep | bestMatch wep rifleSet > Just 0.85   = "Rifle"
                         | bestMatch wep shotgunSet > Just 0.85 = "Shotgun"
                         | bestMatch wep meleeSet > Just 0.85   = "Melee"
                         | bestMatch wep pistolsSet > Just 0.85 = "Secondary"
                         | otherwise                            = "Primary"
{-# INLINE weaponTypeIdentifier #-}

-- | Given some text, matches it against a given fuzzyset and returns the best match found.
bestMatch :: Text -> FuzzySet -> Maybe Double
bestMatch stuff set = map fst $ headMay $ F.get set stuff
{-# INLINE bestMatch #-}

-- | Given some text, matches it against a given fuzzyset and returns match that has >80% 'closeness'.
betterMatch' :: (Ord a1, Fractional a1) => Maybe (a1, a2) -> Maybe a2
betterMatch' (Just (matchPoints, match)) =
  if matchPoints > 0.85 then Just match else Nothing
betterMatch' _ = Nothing

-- | Helper function for bestMatch
bestMatch' :: Text -> FuzzySet -> Maybe (Double, Text)
bestMatch' stuff set = headMay $ F.get set stuff
{-# INLINE bestMatch' #-}

-- | List of rifles converted into fuzzyset to check against.
rifleSet :: FuzzySet
rifleSet = F.fromList listOfAvailableRifles

-- | List of shotguns converted into fuzzyset to check against.
shotgunSet :: FuzzySet
shotgunSet = F.fromList listOfAvailableShotguns

-- | List of melee weapons converted into fuzzyset to check against.
meleeSet :: FuzzySet
meleeSet = F.fromList listOfAvailableMelees

-- | List of pistols converted into fuzzyset to check against.
pistolsSet :: FuzzySet
pistolsSet = F.fromList listOfAvailablePistols
