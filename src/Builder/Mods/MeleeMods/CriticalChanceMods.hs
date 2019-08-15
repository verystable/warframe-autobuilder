{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Builder.Mods.MeleeMods.CriticalChanceMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify critical chance, applicable on melee weapons.

module Builder.Mods.MeleeMods.CriticalChanceMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | True Steel [+60% Critical Chance]
trueSteel :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
trueSteel baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalChance
                          (baseWeapon ^. gwCriticalChance)
                          (Just 0.6)
                          (+)
                          targetWeapon
  , "True Steel [+60% Critical Chance]" : mods
  )
{-# INLINE trueSteel #-}

-- | Blood Rush [+165% Critical Chance, stack with combo counter]
bloodRush
  :: Maybe Float
  -> GenericWeapon
  -> (GenericWeapon, [Text])
  -> (GenericWeapon, [Text])
bloodRush multiplier baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalChance
                          (baseWeapon ^. gwCriticalChance)
                          ((*) <$> multiplier' <*> Just 1.65)
                          (+)
                          targetWeapon
  , "Blood Rush [+165% Critical Chance, stack with combo counter]" : mods
  )
 where
  multiplier' = case (multiplier == Just 1) of
    True  -> Just 0
    False -> multiplier
{-# INLINE bloodRush #-}

truePunishment1 :: GenericWeapon -> GenericWeapon -> GenericWeapon
truePunishment1 baseWeapon targetWeapon = modifyGeneralProperty
  gwCriticalChance
  (baseWeapon ^. gwCriticalChance)
  (Just 0.4)
  (+)
  targetWeapon
{-# INLINE truePunishment1 #-}

truePunishment2 :: GenericWeapon -> GenericWeapon -> GenericWeapon
truePunishment2 baseWeapon targetWeapon = modifyGeneralProperty
  gwChannelingCost
  (baseWeapon ^. gwChannelingCost)
  (Just 0.6)
  (+)
  targetWeapon
{-# INLINE truePunishment2 #-}

-- | True Punishment [+60% Critical Chance]
truePunishment
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
truePunishment baseWeapon (targetWeapon, mods) =
  ( truePunishment1 baseWeapon $ truePunishment2 baseWeapon targetWeapon
  , "True Punishment [+40% Critical Chance, -60% Channeling Efficiency]" : mods
  )
{-# INLINE truePunishment #-}

-- | Maiming Strike [+90% Critical Chance on slide attack]
maimingStrike
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
maimingStrike baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalChance
                          (baseWeapon ^. gwCriticalChance)
                          (Just 0.9)
                          (+)
                          targetWeapon
  , "Maiming Strike [+90% Critical Chance on slide attack]" : mods
  )
{-# INLINE maimingStrike #-}

-- | Sacrificial Steel [+88% Critical Chance, +33% Damage to Sentients]
sacrificialSteel
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
sacrificialSteel baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwCriticalChance
                          (baseWeapon ^. gwCriticalChance)
                          (Just 0.88)
                          (+)
                          targetWeapon
  , "Sacrificial Steel [+88% Critical Chance, +33% Damage to Sentients]" : mods
  )
{-# INLINE sacrificialSteel #-}
