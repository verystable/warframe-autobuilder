{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Types.GeneralTypes
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module contains general type synonyms used in this project.

module Types.GeneralTypes where

import           ClassyPrelude
import           Types.GenericWeapon            ( GenericWeapon )

-- | Mod is a Text Type representing a Warframe mod.
type Mod = Text

-- | Mods is list of Mod.
type Mods = [Mod]

-- | A build is a combination of a weapon and all the mods applied on that weapon.
type Build = (GenericWeapon, Mods)
