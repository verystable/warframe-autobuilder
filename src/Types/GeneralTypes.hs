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

type Mods = [Text]
type Build = (GenericWeapon, Mods)
