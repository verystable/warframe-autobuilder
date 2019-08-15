{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Builder.Mods.MeleeMods.StatusMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify status, applicable on melee weapons.

module Builder.Mods.MeleeMods.StatusMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Drifting Contact [+10 secs Combo Duration, +40% Status Chance]
driftingContact
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
driftingContact baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwStatusChance
                          (baseWeapon ^. gwStatusChance)
                          (Just 0.4)
                          (+)
                          targetWeapon
  , "Drifting Contact [+10 secs Combo Duration, +40% Status Chance]" : mods
  )
{-# INLINE driftingContact #-}

-- | Melee Prowess [+15% Status Chance]
meleeProwess
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
meleeProwess baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwStatusChance
                          (baseWeapon ^. gwStatusChance)
                          (Just 0.15)
                          (+)
                          targetWeapon
  , "Melee Prowess [+15% Status Chance]" : mods
  )
{-# INLINE meleeProwess #-}

-- | Weeping Wounds [+45% Critical Chance, stack with combo counter]
weepingWounds
  :: Maybe Float
  -> GenericWeapon
  -> (GenericWeapon, [Text])
  -> (GenericWeapon, [Text])
weepingWounds multiplier baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwStatusChance
                          (baseWeapon ^. gwStatusChance)
                          ((*) <$> Just 0.45 <*> multiplier')
                          (+)
                          targetWeapon
  , "Weeping Wounds [+45% Critical Chance, stack with combo counter]" : mods
  )
 where
  multiplier' = case multiplier == Just 1 of
    True  -> Just 0
    False -> multiplier
{-# INLINE weepingWounds #-}

