{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : Ranker.Comparators
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module exports functionality required to
-- build comparators.

module Ranker.Comparators where

import           ClassyPrelude
import           Control.Lens

import           Types.GenericWeapon

import           GenericFunctions.GenericFunctions

import           Types.ComprehensiveWeapon

-- | Comparator compare two builds by given a Lens
genericComparator :: Ord a => Getting a s a -> s -> s -> Ordering
genericComparator optimizer cw1 cw2 | cw1 ^. optimizer < cw2 ^. optimizer = LT
                                    | cw1 ^. optimizer > cw2 ^. optimizer = GT
                                    | otherwise                           = EQ

compareGenericWeaponGeneral
  :: Lens' GenericWeapon (Maybe Float)
  -> ComprehensiveWeapon
  -> ComprehensiveWeapon
  -> Ordering
compareGenericWeaponGeneral surfaceFn =
  genericComparator (build . _1 . surfaceFn)

compareGenericWeaponDamage
  :: Lens' GenericDamage (Maybe Float)
  -> ComprehensiveWeapon
  -> ComprehensiveWeapon
  -> Ordering
compareGenericWeaponDamage optimizer cw1 cw2
  | getGenericDamageProperty (cw1 ^. (build . _1)) optimizer
    < getGenericDamageProperty (cw2 ^. (build . _1)) optimizer
  = LT
  | getGenericDamageProperty (cw1 ^. (build . _1)) optimizer
    > getGenericDamageProperty (cw2 ^. (build . _1)) optimizer
  = GT
  | otherwise
  = EQ

compareSustainedDPS :: ComprehensiveWeapon -> ComprehensiveWeapon -> Ordering
compareSustainedDPS = genericComparator sustainedDPS

compareBurstDPS :: ComprehensiveWeapon -> ComprehensiveWeapon -> Ordering
compareBurstDPS = genericComparator burstDPS

compareTotalDamage :: ComprehensiveWeapon -> ComprehensiveWeapon -> Ordering
compareTotalDamage = genericComparator (build . _1 . gwTotalDamage)

compareAdjustedSustainedDPS
  :: ComprehensiveWeapon -> ComprehensiveWeapon -> Ordering
compareAdjustedSustainedDPS = genericComparator adjustedSustainedDPS

compareAdjustedBurstDPS
  :: ComprehensiveWeapon -> ComprehensiveWeapon -> Ordering
compareAdjustedBurstDPS = genericComparator adjustedBurstDPS

compareAdjustedTotalDamage
  :: ComprehensiveWeapon -> ComprehensiveWeapon -> Ordering
compareAdjustedTotalDamage = genericComparator adjustedTotalDamage

compareProcChances
  :: Ord a
  => ((a -> Const a a) -> GenericProcChances -> Const a GenericProcChances)
  -> ComprehensiveWeapon
  -> ComprehensiveWeapon
  -> Ordering
compareProcChances optimizer = genericComparator (procChances . optimizer)

compareProcDPSs
  :: Ord a
  => ((a -> Const a a) -> GenericProcDPSs -> Const a GenericProcDPSs)
  -> ComprehensiveWeapon
  -> ComprehensiveWeapon
  -> Ordering
compareProcDPSs optimizer = genericComparator (procDPSs . optimizer)
