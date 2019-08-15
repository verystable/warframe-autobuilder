{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Builder.Applicator.PostBuildSetup
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module contains all the mechanism that has to be applied after
-- building the weapon. For eg. modifying damage based on the critical modifier,
-- checking if the combo counter affects certain aspects of the weapon, etc.

module Builder.Applicator.PostBuildSetup where

import           ClassyPrelude
import           Control.Lens

import           Types.GenericWeapon

import           GenericFunctions.GenericFunctions

-- | List of postbuild (Build -> Build) functions.
--   Can be applied on any type of Build.
genericPostBuildSetup :: [(GenericWeapon, [Text]) -> (GenericWeapon, [Text])]
genericPostBuildSetup =
  [ setGenericDamageCombos
  , setGenericWeaponCritModified
  , setGenericWeaponStatusMultishotModified
  , setGenericWeaponDamageMultishotModified
  ]

-- | Sets damage critical modifier -modified.
setGenericWeaponCritModified
  :: (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
setGenericWeaponCritModified (gw, mods) =
  (applyMultiplier ecm (gw & gwBaseDamage %~ (\b -> (*) <$> b <*> ecm)), mods)
 where
  cc = gw ^. gwCriticalChance
  cd = gw ^. gwCriticalMultiplier
  ecm = -- effective critical multiplier
    (+) <$> ((*) <$> cc <*> cd) <*> ((-) <$> Just 1 <*> cc)

-- | Sets all the damage combos. Eg. Electricity + Cold - Magnetic
setGenericDamageCombos :: (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
setGenericDamageCombos = damageCombos' . damageCombos'
{-# INLINE setGenericDamageCombos #-}

-- | Helper function for setGenericDamageCombos
damageCombos' :: (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
damageCombos' (gw, mods)
  | fromMaybe (Just 0) (gw ^. gwDamageTypes & _Just %~ view gdCold)
    /= Just 0
    && fromMaybe (Just 0) (gw ^. gwDamageTypes & _Just %~ view gdToxin)
    /= Just 0
  = ( gw
      &  (gwDamageTypes . _Just . gdViral)
      %~ (\a ->
           (+)
             <$> a
             <*> (   (+)
                 <$> fromMaybe (Just 0)
                               (gw ^. gwDamageTypes & _Just %~ view gdCold)

                 <*> fromMaybe (Just 0)
                               (gw ^. gwDamageTypes & _Just %~ view gdToxin)
                 )
         )
      &  ((gwDamageTypes . _Just . gdCold) ?~ 0)
      &  ((gwDamageTypes . _Just . gdToxin) ?~ 0)
    , mods
    )
  | fromMaybe (Just 0) (gw ^. gwDamageTypes & _Just %~ view gdHeat)
    /= Just 0
    && fromMaybe (Just 0) (gw ^. gwDamageTypes & _Just %~ view gdCold)
    /= Just 0
  = ( gw
      &  (gwDamageTypes . _Just . gdBlast)
      %~ (\a ->
           (+)
             <$> a
             <*> (   (+)
                 <$> fromMaybe (Just 0)
                               (gw ^. gwDamageTypes & _Just %~ view gdHeat)

                 <*> fromMaybe (Just 0)
                               (gw ^. gwDamageTypes & _Just %~ view gdCold)
                 )
         )
      &  ((gwDamageTypes . _Just . gdHeat) ?~ 0)
      &  ((gwDamageTypes . _Just . gdCold) ?~ 0)
    , mods
    )
  | fromMaybe (Just 0) (gw ^. gwDamageTypes & _Just %~ view gdHeat)
    /= Just 0
    && fromMaybe (Just 0) (gw ^. gwDamageTypes & _Just %~ view gdToxin)
    /= Just 0
  = ( gw
      &  (gwDamageTypes . _Just . gdGas)
      %~ (\a ->
           (+)
             <$> a
             <*> (   (+)
                 <$> fromMaybe (Just 0)
                               (gw ^. gwDamageTypes & _Just %~ view gdHeat)

                 <*> fromMaybe (Just 0)
                               (gw ^. gwDamageTypes & _Just %~ view gdToxin)
                 )
         )
      &  ((gwDamageTypes . _Just . gdHeat) ?~ 0)
      &  ((gwDamageTypes . _Just . gdToxin) ?~ 0)
    , mods
    )
  | fromMaybe (Just 0) (gw ^. gwDamageTypes & _Just %~ view gdHeat)
    /= Just 0
    && fromMaybe (Just 0) (gw ^. gwDamageTypes & _Just %~ view gdElectricity)
    /= Just 0
  = ( gw
      &  (gwDamageTypes . _Just . gdRadiation)
      %~ (\a ->
           (+)
             <$> a
             <*> (   (+)
                 <$> fromMaybe (Just 0)
                               (gw ^. gwDamageTypes & _Just %~ view gdHeat)

                 <*> fromMaybe
                       (Just 0)
                       (gw ^. gwDamageTypes & _Just %~ view gdElectricity)
                 )
         )
      &  ((gwDamageTypes . _Just . gdHeat) ?~ 0)
      &  ((gwDamageTypes . _Just . gdElectricity) ?~ 0)
    , mods
    )
  | fromMaybe (Just 0) (gw ^. gwDamageTypes & _Just %~ view gdCold)
    /= Just 0
    && fromMaybe (Just 0) (gw ^. gwDamageTypes & _Just %~ view gdElectricity)
    /= Just 0
  = ( gw
      &  (gwDamageTypes . _Just . gdMagnetic)
      %~ (\a ->
           (+)
             <$> a
             <*> (   (+)
                 <$> fromMaybe (Just 0)
                               (gw ^. gwDamageTypes & _Just %~ view gdCold)

                 <*> fromMaybe
                       (Just 0)
                       (gw ^. gwDamageTypes & _Just %~ view gdElectricity)
                 )
         )
      &  ((gwDamageTypes . _Just . gdCold) ?~ 0)
      &  ((gwDamageTypes . _Just . gdElectricity) ?~ 0)
    , mods
    )
  | fromMaybe (Just 0) (gw ^. gwDamageTypes & _Just %~ view gdElectricity)
    /= Just 0
    && fromMaybe (Just 0) (gw ^. gwDamageTypes & _Just %~ view gdToxin)
    /= Just 0
  = ( gw
      &  (gwDamageTypes . _Just . gdCorrosive)
      %~ (\a ->
           (+)
             <$> a
             <*> (   (+)
                 <$> fromMaybe
                       (Just 0)
                       (gw ^. gwDamageTypes & _Just %~ view gdElectricity)

                 <*> fromMaybe (Just 0)
                               (gw ^. gwDamageTypes & _Just %~ view gdToxin)
                 )
         )
      &  ((gwDamageTypes . _Just . gdElectricity) ?~ 0)
      &  ((gwDamageTypes . _Just . gdToxin) ?~ 0)
    , mods
    )
  | otherwise
  = (gw, mods)

-- | Applies a post build multiplier.
setGenericWeaponDamageMultishotModified
  :: (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
setGenericWeaponDamageMultishotModified (gw, mods) =
  ( applyMultiplier
    (gw ^. gwMultishot)
    (gw & gwBaseDamage %~ (\b -> (*) <$> b <*> (gw ^. gwMultishot)))
  , mods
  )

-- | Weapons excluded from building due to some discrepancies.
weaponExceptionList :: [Maybe Text]
weaponExceptionList = [Just "Glaxion", Just "Glaxion Vandal"]

-- | Sets the status chance of a weapon after applying multishot.
setGenericWeaponStatusMultishotModified
  :: (GenericWeapon, [Text]) -> (GenericWeapon, [Text])
setGenericWeaponStatusMultishotModified (gw, mods) =
  ( modifyGeneralProperty gwStatusChance
                          (Just $ newStatusChance - oldStatusChance')
                          (Just 1)
                          (+)
                          gw
  , mods
  )
 where
  oldStatusChance' = fromMaybe 0 $ gw ^. gwStatusChance
  oldStatusChance  = if oldStatusChance' >= 1 then 1 else oldStatusChance'

  newStatusChance  = 1 - ((1 - oldStatusChance) ** multishotModifier)

  multishotModifier =
    fromMaybe 1 $ liftA2 (/) (gw ^. gwMultishot) (gw ^. gwBaseMultishot)
