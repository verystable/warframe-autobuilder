{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.ShotgunMods.AmmoMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify ammo, applicable on primary (shotguns) weapons.

module Builder.Mods.ShotgunMods.AmmoMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Ammo Stock [+60% Ammo Max]
ammoStock :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
ammoStock baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwAmmo
                          (baseWeapon ^. gwAmmo)
                          (Just 0.6)
                          (+)
                          targetWeapon
  , "Ammo Stock [+60% Ammo Max]" : mods
  )
{-# INLINE ammoStock #-}

-- | Shell Compression [+30% Ammo Max]
shellCompression
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
shellCompression baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwAmmo
                          (baseWeapon ^. gwAmmo)
                          (Just 0.3)
                          (+)
                          targetWeapon
  , "Shell Compression [+30% Ammo Max]" : mods
  )
{-# INLINE shellCompression #-}

