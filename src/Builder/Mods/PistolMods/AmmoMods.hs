{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.PistolMods.AmmoMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify ammo, applicable on secondary weapons.

module Builder.Mods.PistolMods.AmmoMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Trick Mag [+90% Ammo Max]
trickMag :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
trickMag baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwAmmo
                          (baseWeapon ^. gwAmmo)
                          (Just 0.9)
                          (+)
                          targetWeapon
  , "Trick Mag [+90% Ammo Max]" : mods
  )
{-# INLINE trickMag #-}

