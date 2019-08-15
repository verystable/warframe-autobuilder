{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Mods.RifleMods.SpecialMods
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- Contains function that modify special features, applicable on primary (rifles) weapons.

module Builder.Mods.RifleMods.SpecialMods where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           GenericFunctions.GenericFunctions
import           Types.GenericWeapon

-- | Hunter Munitions [+30% Slash Proc Chance depending on Critical Chance]
hunterMunitions
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
hunterMunitions _ (targetWeapon, mods) =
  ( targetWeapon
  , "Hunter Munitions [+30% Slash Proc Chance depending on Critical Chance]"
    : mods
  )
{-# INLINE hunterMunitions #-}

-- | Hunter Track [+30% Status Duration]
hunterTrack
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
hunterTrack _ (targetWeapon, mods) =
  (targetWeapon, "Hunter Track [+30% Status Duration]" : mods)
{-# INLINE hunterTrack #-}

-- | Continuous Misery [+100% Status Duration]
continuousMisery
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
continuousMisery _ (targetWeapon, mods) =
  (targetWeapon, "Continuous Misery [+100% Status Duration]" : mods)
{-# INLINE continuousMisery #-}

-- | Metal Auger [+2.1m Punchthrough]
--   genericWeapon & propertyToSet %~ (\property -> operator <$> property <*> ((*) <$> multiplier <*> valueToBaseOn))
--   genericWeapon & gwPunchthrough %~ (\x -> (+) <$> x <*> ((*) <$> Just 1 <*> Just 2.1))
--   genericWeapon & gwPunchthrough %~ (\x -> (+) <$> x <*> Just 2.1)
--   because x can be (Just 0)
--   genericWeapon & gwPunchthrough %~ (\x -> (+) <$> Just 0 <*> Just 2.1)
metalAuger
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
metalAuger _ (targetWeapon, mods) =
  ( modifyGeneralProperty gwPunchthrough (Just 2.1) (Just 1) (+) targetWeapon
  , "Metal Auger [+2.1m Punchthrough]" : mods
  )
{-# INLINE metalAuger #-}

-- | Shred [+30% Fire Rate, +1.2m Punchthrough]
shred :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
shred baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwPunchthrough (Just 1.2) (Just 1) (+)
    $ modifyGeneralProperty gwFireRate
                            (baseWeapon ^. gwFireRate)
                            (Just 0.3)
                            (+)
                            targetWeapon
  , "Shred [+30% Fire Rate, +1.2m Punchthrough]" : mods
  )
{-# INLINE shred #-}

-- | Primed Shred [+55% Fire Rate, +2.2m Punchthrough]
primedShred
  :: GenericWeapon -> (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
primedShred baseWeapon (targetWeapon, mods) =
  ( modifyGeneralProperty gwPunchthrough (Just 2.2) (Just 1) (+)
    $ modifyGeneralProperty gwFireRate
                            (baseWeapon ^. gwFireRate)
                            (Just 0.55)
                            (+)
                            targetWeapon
  , "Primed Shred [+55% Fire Rate, +2.2m Punchthrough]" : mods
  )
{-# INLINE primedShred #-}
