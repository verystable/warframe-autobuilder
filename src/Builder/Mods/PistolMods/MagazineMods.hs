{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.PistolMods.AmmoMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify magazine size, applicable on secondary weapons.

module Builder.Mods.PistolMods.MagazineMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

taintedClip1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
taintedClip1 baseWeapon targetWeapon = modifyGeneralProperty
  gwMagazineSize
  (baseWeapon ^. gwMagazineSize)
  (Just 0.6)
  (+)
  targetWeapon

taintedClip2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
taintedClip2 baseWeapon targetWeapon = modifyGeneralProperty
  gwReloadTime
  (baseWeapon ^. gwReloadTime)
  (Just 0.3)
  (+)
  targetWeapon

-- | Tainted Clip [+90% Magazine, -30% Reload Speed (+30% Reload Time)]
taintedClip
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
taintedClip baseWeapon (targetWeapon, mods) =
  ( taintedClip1 baseWeapon $ taintedClip2 baseWeapon targetWeapon
  , "Tainted Clip [+90% Magazine, -30% Reload Speed (+30% Reload Time)]" : mods
  )
{-# INLINE taintedClip1 #-}

-- | Primed Slip Magazine [+55% Magazine Size]
primedSlipMagazine
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedSlipMagazine baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwMagazineSize
                          (baseWeapon ^. gwMagazineSize)
                          (Just 0.55)
                          (+)
                          targetWeapon
  , "Primed Slip Magazine [+55% Magazine Size]" : mods
  )
{-# INLINE primedSlipMagazine #-}

-- | Slip Magazine [+30% Magazine Size]
slipMagazine
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
slipMagazine baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwMagazineSize
                          (baseWeapon ^. gwMagazineSize)
                          (Just 0.3)
                          (+)
                          targetWeapon
  , "Slip Magazine [+30% Magazine Size]" : mods
  )
{-# INLINE slipMagazine #-}
