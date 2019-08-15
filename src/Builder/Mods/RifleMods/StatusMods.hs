{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.RifleMods.StatusMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify status, applicable on primary (rifles) weapons.

module Builder.Mods.RifleMods.StatusMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Catalyzer Link [+60% Status Chance, on ability cast]
catalyzerLink
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
catalyzerLink baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwStatusChance
                          (baseWeapon ^. gwStatusChance)
                          (Just 0.6)
                          (+)
                          targetWeapon
  , "Catalyzer Link [+60% Status Chance, on ability cast]" : mods
  )
{-# INLINE catalyzerLink #-}

-- | Rifle Aptitude [+15% Status Chance]
rifleAptitude
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
rifleAptitude baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwStatusChance
                          (baseWeapon ^. gwStatusChance)
                          (Just 0.15)
                          (+)
                          targetWeapon
  , "Rifle Aptitude [+15% Status Chance]" : mods
  )
{-# INLINE rifleAptitude #-}
