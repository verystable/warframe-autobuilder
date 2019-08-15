{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Builder.Mods.MeleeMods.CriticalMultiplierMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify critical multiplier, applicable on melee weapons.

module Builder.Mods.MeleeMods.CriticalMultiplierMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Organ Shatter [+90% Critical Multiplier]
organShatter
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
organShatter baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalMultiplier
                          (baseWeapon ^. gwCriticalMultiplier)
                          (Just 0.9)
                          (+)
                          targetWeapon
  , "Organ Shatter [+90% Critical Multiplier]" : mods
  )
{-# INLINE organShatter #-}

-- | Amalgam Organ Shatter [+85% Critical Multiplier, +60% Charge Attack Speed]
amalgamOrganShatter
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
amalgamOrganShatter baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalMultiplier
                          (baseWeapon ^. gwCriticalMultiplier)
                          (Just 0.85)
                          (+)
                          targetWeapon
  , "Amalgam Organ Shatter [+85% Critical Multiplier, +60% Charge Attack Speed]"
    : mods
  )
{-# INLINE amalgamOrganShatter #-}

-- | Gladiator Might [+60% Critical Multiplier, 15% Critical Chance stack with Combo Multiplier]
gladiatorMight
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
gladiatorMight baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalMultiplier
                          (baseWeapon ^. gwCriticalMultiplier)
                          (Just 0.6)
                          (+)
                          targetWeapon
  , "Gladiator Might [+60% Critical Multiplier, 15% Critical Chance stack with Combo Multiplier]"
    : mods
  )
{-# INLINE gladiatorMight #-}
