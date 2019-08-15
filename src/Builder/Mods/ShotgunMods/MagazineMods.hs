{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.ShotgunMods.MagazineMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify magazine size, applicable on primary (shotguns) weapons.

module Builder.Mods.ShotgunMods.MagazineMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

burdenedMagazine1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
burdenedMagazine1 baseWeapon targetWeapon = modifyGeneralProperty
  gwMagazineSize
  (baseWeapon ^. gwMagazineSize)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE burdenedMagazine1 #-}

burdenedMagazine2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
burdenedMagazine2 baseWeapon targetWeapon = modifyGeneralProperty
  gwReloadTime
  (baseWeapon ^. gwReloadTime)
  (Just 0.18)
  (+)
  targetWeapon
{-# INLINE burdenedMagazine2 #-}

-- | Burdened Magazine [+90% Magazine, -18% Reload Speed (+18% Reload Time)]
burdenedMagazine
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
burdenedMagazine baseWeapon (targetWeapon, mods) =
  ( burdenedMagazine1 baseWeapon $ burdenedMagazine2 baseWeapon targetWeapon
  , "Burdened Magazine [+90% Magazine, -18% Reload Speed (+18% Reload Time)]"
    : mods
  )
{-# INLINE burdenedMagazine #-}
