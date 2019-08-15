{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.RifleMods.CriticalChanceMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify critical chance, applicable on primary (rifle) weapons.

module Builder.Mods.RifleMods.CriticalChanceMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | PointStrike [+150% Critical Chance]
pointStrike
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
pointStrike baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalChance
                          (baseWeapon ^. gwCriticalChance)
                          (Just 1.5)
                          (+)
                          targetWeapon
  , "Point Strike [+150% Critical Chance]" : mods
  )
{-# INLINE pointStrike #-}

-- | Argon Scope [+135% Critical Chance, on headshot]
argonScope
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
argonScope baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalChance
                          (baseWeapon ^. gwCriticalChance)
                          (Just 1.35)
                          (+)
                          targetWeapon
  , "Argon Scope [+135% Critical Chance, on headshot]" : mods
  )
{-# INLINE argonScope #-}

criticalDelay1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
criticalDelay1 baseWeapon targetWeapon = modifyGeneralProperty
  gwCriticalChance
  (baseWeapon ^. gwCriticalChance)
  (Just 0.48)
  (+)
  targetWeapon
{-# INLINE criticalDelay1 #-}

criticalDelay2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
criticalDelay2 baseWeapon targetWeapon = modifyGeneralProperty
  gwFireRate
  (baseWeapon ^. gwFireRate)
  (Just 0.36)
  (-)
  targetWeapon
{-# INLINE criticalDelay2 #-}

-- | Critical Delay [+48% Critical Chance, -36% Fire Rate]
criticalDelay
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
criticalDelay baseWeapon (targetWeapon, mods) =
  ( criticalDelay1 baseWeapon $ criticalDelay2 baseWeapon targetWeapon
  , "Critical Delay [+48% Critical Chance, -36% Fire Rate]" : mods
  )
{-# INLINE criticalDelay #-}
