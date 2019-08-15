{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : GenericFunctions.GenericFunctions
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module is the brain of this whole project.
-- It contains 'generic' functions (which are in fact mods in essence).
-- There are 3 distinct types of generic functions.
-- One that can modify a general property like
--     * MagazineSize
--     * ReloadTime
--     * TotalDamage
--     * BaseDamage
--     * Multishot
-- where a 'general property' is a property available to all types of weapons
-- like for example any weapon will have 'base damage' or 'total damage'
-- and while melee weapons won't have 'magazine size' or 'reload time'
-- those will be present on any sort of 'gun'.
-- Other one that can modify damage properties like
--     * Physical Damage
--     * Elemental Damage
-- and lastly a generic function that can apply a generic multiplier.

module GenericFunctions.GenericFunctions where

import           ClassyPrelude
import           Control.Lens
import           Data.Aeson                     ( eitherDecodeFileStrict )
import           Types.GenericWeapon

-- | Type constrained synonym for 'eitherDecodeFileStrict'
readWeapon :: FilePath -> IO (Either String GenericWeapon)
readWeapon = eitherDecodeFileStrict

-- | Sets a property of a GenericWeapon instance
--   Should be used for general stats like
--     * MagazineSize
--     * ReloadTime
--     * TotalDamage
--     * BaseDamage
--     * Multishot
--     * Punchthrough
--     * Accuracy
--     * CriticalChance
--     * CriticalMultiplier
--     * StatusChance
--     * FireRate
--    Qualified record will have a 'gw' prefix.
modifyGeneralProperty
  :: Lens' GenericWeapon (Maybe Float) -- Property to set
  -> Maybe Float -- Value to base on
  -> Maybe Float -- Multiplier
  -> (Float -> Float -> Float) -- Operator to apply
  -> GenericWeapon -- Weapon to set
  -> GenericWeapon -- New weapon
modifyGeneralProperty propertyToSet valueToBaseOn multiplier operator genericWeapon
  = genericWeapon
    &  propertyToSet
    %~ (\property ->
         operator <$> property <*> ((*) <$> multiplier <*> valueToBaseOn)
       )
{-# INLINE modifyGeneralProperty #-}

-- | Sets a property of a GenericDamage instance
--   Modifying with this function will also modify the total damage of the GenericWeapon instance
--   Should be used for damage stats like
--     * Physical Damage
--     * Elemental Damage
--    Qualified record will have a 'gd' prefix.
modifyDamageProperty
  :: (Lens' GenericDamage (Maybe Float)) -- Property to set
  -> Maybe Float -- Value to base on
  -> Maybe Float -- Multiplier
  -> (Float -> Float -> Float) -- Operator to apply
  -> GenericWeapon -- Weapon to set
  -> GenericWeapon -- New weapon
modifyDamageProperty propertyToSet valueToBaseOn multiplier operator genericWeapon
  = genericWeapon
    &  (gwDamageTypes . _Just . propertyToSet)
    %~ (\property ->
         operator <$> property <*> ((*) <$> multiplier <*> valueToBaseOn)
       )
    &  gwTotalDamage
    %~ (\property ->
         operator <$> property <*> ((*) <$> multiplier <*> valueToBaseOn)
       )
    &  gwSpinAttack
    %~ (\property ->
         operator <$> property <*> ((*) <$> multiplier <*> valueToBaseOn)
       )
    &  gwLeapAttack
    %~ (\property ->
         operator <$> property <*> ((*) <$> multiplier <*> valueToBaseOn)
       )
    &  gwWallAttack
    %~ (\property ->
         operator <$> property <*> ((*) <$> multiplier <*> valueToBaseOn)
       )
    &  gwChargeAttack
    %~ (\property ->
         operator <$> property <*> ((*) <$> multiplier <*> valueToBaseOn)
       )
{-# INLINE modifyDamageProperty #-}

-- | Applies a damage multiplier
--   Should be used for damage mods like
--     * Serration
--     * Split Chamber
--     * Crit Modification
applyMultiplier
  :: Maybe Float -- Multiplier
  -> GenericWeapon -- Weapon to set
  -> GenericWeapon -- New weapon
applyMultiplier multiplier@(Just _) genericWeapon =
  genericWeapon
    &  (gwDamageTypes . _Just)
    %~ omap (\property -> (*) <$> property <*> multiplier)
    &  gwTotalDamage
    %~ (\property -> (*) <$> property <*> multiplier)
    &  gwSpinAttack
    %~ (\property -> (*) <$> property <*> multiplier)
    &  gwLeapAttack
    %~ (\property -> (*) <$> property <*> multiplier)
    &  gwWallAttack
    %~ (\property -> (*) <$> property <*> multiplier)
    &  gwChargeAttack
    %~ (\property -> (*) <$> property <*> multiplier)
applyMultiplier Nothing genericWeapon =
  genericWeapon
    &  (gwDamageTypes . _Just)
    %~ omap (\property -> (*) <$> property <*> Just 1)
    &  gwTotalDamage
    %~ (\property -> (*) <$> property <*> Just 1)
    &  gwSpinAttack
    %~ (\property -> (*) <$> property <*> Just 1)
    &  gwLeapAttack
    %~ (\property -> (*) <$> property <*> Just 1)
    &  gwWallAttack
    %~ (\property -> (*) <$> property <*> Just 1)
    &  gwChargeAttack
    %~ (\property -> (*) <$> property <*> Just 1)
{-# INLINE applyMultiplier #-}

-- | Modifies damage with a multiplier
--   Should be used for damage mods like
--     * Vile Acceleration
modifyWithMultiplier
  :: Maybe Float -- Multiplier
  -> (Float -> Float -> Float) -- Operator
  -> GenericWeapon -- Weapon to set
  -> GenericWeapon -- New weapon
modifyWithMultiplier multiplier operator genericWeapon =
  genericWeapon
    &  (gwDamageTypes . _Just)
    %~ omap
         (\property ->
           operator <$> property <*> ((*) <$> property <*> multiplier)
         )
    &  gwTotalDamage
    %~ (\property -> operator <$> property <*> ((*) <$> property <*> multiplier)
       )
    &  gwSpinAttack
    %~ (\property -> operator <$> property <*> ((*) <$> property <*> multiplier)
       )
    &  gwLeapAttack
    %~ (\property -> operator <$> property <*> ((*) <$> property <*> multiplier)
       )
    &  gwWallAttack
    %~ (\property -> operator <$> property <*> ((*) <$> property <*> multiplier)
       )
    &  gwChargeAttack
    %~ (\property -> operator <$> property <*> ((*) <$> property <*> multiplier)
       )
{-# INLINE modifyWithMultiplier #-}

-- | Retrives GenericDamage value out of nested Maybe types
getGenericDamageProperty
  :: Num a
  => GenericWeapon
  -> Getting (Maybe a) GenericDamage (Maybe a)
  -> Maybe a
getGenericDamageProperty wep property =
  fromMaybe (Just 0) $ wep ^. gwDamageTypes & _Just %~ view property
{-# INLINE getGenericDamageProperty #-}
