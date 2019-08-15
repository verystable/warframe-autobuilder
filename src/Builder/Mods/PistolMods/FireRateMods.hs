{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.PistolMods.AmmoMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify fire rate, applicable on secondary weapons.

module Builder.Mods.PistolMods.FireRateMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

anemicAgility1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
anemicAgility1 baseWeapon targetWeapon =
  modifyWithMultiplier (Just 0.15) (-) targetWeapon

anemicAgility2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
anemicAgility2 baseWeapon targetWeapon = modifyGeneralProperty
  gwFireRate
  (baseWeapon ^. gwFireRate)
  (Just 0.9)
  (+)
  targetWeapon

-- | Anemic Agility [+90% Fire Rate, -15% Damage]
anemicAgility
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
anemicAgility baseWeapon (targetWeapon, mods) =
  ( anemicAgility1 baseWeapon $ anemicAgility2 baseWeapon targetWeapon
  , "Anemic Agility [+90% Fire Rate, -15% Damage]" : mods
  )
{-# INLINE anemicAgility1 #-}

-- | Gunslinger [+72% Fire Rate]
gunslinger
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
gunslinger baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwFireRate
                          (baseWeapon ^. gwFireRate)
                          (Just 0.72)
                          (+)
                          targetWeapon
  , "Gunslinger [+72% Fire Rate]" : mods
  )
{-# INLINE gunslinger #-}

-- | Pressurized Magazine [+90% Fire Rate, on reload]
pressurizedMagazine
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
pressurizedMagazine baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwFireRate
                          (baseWeapon ^. gwFireRate)
                          (Just 0.9)
                          (+)
                          targetWeapon
  , "Pressurized Magazine [+90% Fire Rate, on reload]" : mods
  )
{-# INLINE pressurizedMagazine #-}
