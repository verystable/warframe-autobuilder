{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.PistolMods.AmmoMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify special features, applicable on secondary weapons.

module Builder.Mods.PistolMods.SpecialMods where

import           ClassyPrelude
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Augur Seeker [+30% Status Duration]
augurSeeker
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
augurSeeker _ (targetWeapon, mods) =
  (targetWeapon, "Augur Seeker [+30% Status Duration]" : mods)
{-# INLINE augurSeeker #-}

-- | Perpetual Agony [+30% Status Duration]
perpetualAgony
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
perpetualAgony _ (targetWeapon, mods) =
  (targetWeapon, "Perpetual Agony [+30% Status Duration]" : mods)
{-# INLINE perpetualAgony #-}

-- | Seeker [+2.1m Punchthrough]
seeker :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
seeker _ (targetWeapon, mods) =
  ( modifyGeneralProperty gwPunchthrough (Just 2.1) (Just 1) (+) targetWeapon
  , "Seeker [+2.1m Punchthrough]" : mods
  )
{-# INLINE seeker #-}
