{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Types.GenericWeapon
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module contains 'GenericWeapon' type.

module Types.GenericWeapon where

import           ClassyPrelude
import           Control.Lens                   ( makeLenses )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , defaultOptions
                                                , eitherDecodeFileStrict
                                                , genericToEncoding
                                                , parseJSON
                                                , toEncoding
                                                , withObject
                                                , (.:?)
                                                )

-- | Generic Damage Record
--   if the weapon doesn't have one of these fields, it's set to 'null' in the 'json' file
data GenericDamage = GenericDamage { _gdImpact      :: Maybe Float
                                   , _gdPuncture    :: Maybe Float
                                   , _gdSlash       :: Maybe Float
                                   , _gdHeat        :: Maybe Float
                                   , _gdCold        :: Maybe Float
                                   , _gdToxin       :: Maybe Float
                                   , _gdElectricity :: Maybe Float
                                   , _gdBlast       :: Maybe Float
                                   , _gdGas         :: Maybe Float
                                   , _gdRadiation   :: Maybe Float
                                   , _gdViral       :: Maybe Float
                                   , _gdCorrosive   :: Maybe Float
                                   , _gdMagnetic    :: Maybe Float
                                   } deriving (Show, Generic, Eq, Ord)

makeLenses ''GenericDamage

instance ToJSON GenericDamage where
      -- No need to provide a toJSON implementation.

      -- Writing a simple toEncoding implementation
      toEncoding = genericToEncoding defaultOptions

instance FromJSON GenericDamage where
  parseJSON = withObject "GenericDamage" $ \o -> GenericDamage
    <$> o .:? "_gdImpact"
    <*> o .:? "_gdPuncture"
    <*> o .:? "_gdSlash"
    <*> o .:? "_gdHeat"
    <*> o .:? "_gdCold"
    <*> o .:? "_gdToxin"
    <*> o .:? "_gdElectricity"
    <*> o .:? "_gdBlast"
    <*> o .:? "_gdGas"
    <*> o .:? "_gdRadiation"
    <*> o .:? "_gdViral"
    <*> o .:? "_gdCorrosive"
    <*> o .:? "_gdMagnetic"

type instance Element GenericDamage = Maybe Float

instance MonoFunctor GenericDamage where
  omap f
    (GenericDamage
     bvImpact
     bvPuncture
     bvSlash
     bvHeat
     bvCold
     bvToxin
     bvElectricity
     bvBlast
     bvGas
     bvRadiation
     bvViral
     bvCorrosive
     bvMagnetic)
    =
    GenericDamage
     (f bvImpact)
     (f bvPuncture)
     (f bvSlash)
     (f bvHeat)
     (f bvCold)
     (f bvToxin)
     (f bvElectricity)
     (f bvBlast)
     (f bvGas)
     (f bvRadiation)
     (f bvViral)
     (f bvCorrosive)
     (f bvMagnetic)

-- | Generic Weapon Record
data GenericWeapon = GenericWeapon { _gwName                 :: Maybe Text
                                   , _gwMagazineSize         :: Maybe Float
                                   , _gwReloadTime           :: Maybe Float
                                   , _gwTotalDamage          :: Maybe Float
                                   , _gwBaseDamage           :: Maybe Float
                                   , _gwBaseMultishot        :: Maybe Float
                                   , _gwMultishot            :: Maybe Float
                                   , _gwPunchthrough         :: Maybe Float
                                   , _gwAccuracy             :: Maybe Float
                                   , _gwCriticalChance       :: Maybe Float
                                   , _gwCriticalMultiplier   :: Maybe Float
                                   , _gwStatusChance         :: Maybe Float
                                   , _gwFireRate             :: Maybe Float
                                   , _gwChargeAttack         :: Maybe Float
                                   , _gwSpinAttack           :: Maybe Float
                                   , _gwLeapAttack           :: Maybe Float
                                   , _gwWallAttack           :: Maybe Float
                                   , _gwChannelingMultiplier :: Maybe Float
                                   , _gwChannelingCost       :: Maybe Float
                                   , _gwOmegaAttenuation     :: Maybe Float
                                   , _gwType                 :: Maybe Text
                                   , _gwCategory             :: Maybe Text
                                   , _gwAmmo                 :: Maybe Float
                                   , _gwDamageTypes          :: Maybe GenericDamage
                                   , _gwDisposition          :: Maybe Float
                                   } deriving (Show, Generic, Eq, Ord)

makeLenses ''GenericWeapon

instance ToJSON GenericWeapon where
      -- No need to provide a toJSON implementation.

      -- Writing a simple toEncoding implementation
      toEncoding = genericToEncoding defaultOptions

instance FromJSON GenericWeapon where
  parseJSON = withObject "GenericWeapon" $ \o -> GenericWeapon
    <$> o .:? "_gwName"
    <*> o .:? "_gwMagazineSize"
    <*> o .:? "_gwReloadTime"
    <*> o .:? "_gwTotalDamage"
    <*> o .:? "_gwBaseDamage"
    <*> o .:? "_gwBaseMultishot"
    <*> o .:? "_gwMultishot"
    <*> o .:? "_gwPunchthrough"
    <*> o .:? "_gwAccuracy"
    <*> o .:? "_gwCriticalChance"
    <*> o .:? "_gwCriticalMultiplier"
    <*> o .:? "_gwStatusChance"
    <*> o .:? "_gwFireRate"
    <*> o .:? "_gwChargeAttack"
    <*> o .:? "_gwSpinAttack"
    <*> o .:? "_gwLeapAttack"
    <*> o .:? "_gwWallAttack"
    <*> o .:? "_gwChannelingMultiplier"
    <*> o .:? "_gwChannelingCost"
    <*> o .:? "_gwOmegaAttenuation"
    <*> o .:? "_gwType"
    <*> o .:? "_gwCategory"
    <*> o .:? "_gwAmmo"
    <*> o .:? "_gwDamageTypes"
    <*> o .:? "_gwDisposition"

dummyGenericWeapon :: GenericWeapon
dummyGenericWeapon = GenericWeapon Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
                                   Nothing
