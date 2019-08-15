{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.RifleMods.AmmoMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify ammo, applicable on primary (rifle) weapons.

module Builder.Mods.RifleMods.AmmoMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Ammo Drum [+30% Ammo Max]
ammoDrum :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
ammoDrum baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwAmmo
                          (baseWeapon ^. gwAmmo)
                          (Just 0.3)
                          (+)
                          targetWeapon
  , "Ammo Drum [+30% Ammo Max]" : mods
  )
{-# INLINE ammoDrum #-}
