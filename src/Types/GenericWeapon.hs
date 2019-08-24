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
                                                , genericToEncoding
                                                , parseJSON
                                                , toEncoding
                                                , withObject
                                                , (.:?)
                                                )

-- | Generic Damage Record
--   if the weapon doesn't have one of these fields, it's set to 'null' in the 'json' file
data GenericDamage = GenericDamage
  {
    -- | Represents Impact damage of a weapon
    _gdImpact      :: Maybe Float
    -- | Represents Puncture damage of a weapon    
  , _gdPuncture    :: Maybe Float
    -- | Represents Slash damage of a weapon
  , _gdSlash       :: Maybe Float
    -- | Represents Heat damage of a weapon
  , _gdHeat        :: Maybe Float
    -- | Represents Cold damage of a weapon
  , _gdCold        :: Maybe Float
    -- | Represents Toxin damage of a weapon
  , _gdToxin       :: Maybe Float
    -- | Represents Electricity damage of a weapon
  , _gdElectricity :: Maybe Float
    -- | Represents Blast damage of a weapon
  , _gdBlast       :: Maybe Float
    -- | Represents Gas damage of a weapon
  , _gdGas         :: Maybe Float
    -- | Represents Radiation damage of a weapon
  , _gdRadiation   :: Maybe Float
    -- | Represents Viral damage of a weapon
  , _gdViral       :: Maybe Float
    -- | Represents Corrosive damage of a weapon
  , _gdCorrosive   :: Maybe Float
    -- | Represents Magnetic damage of a weapon
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
data GenericWeapon = GenericWeapon
  { -- | Represents Name of a weapon
    _gwName                 :: Maybe Text
    -- | Represents MagazineSize of a weapon
  , _gwMagazineSize         :: Maybe Float
    -- | Represents ReloadTime of a weapon
  , _gwReloadTime           :: Maybe Float
    -- | Represents TotalDamage of a weapon
  , _gwTotalDamage          :: Maybe Float
    -- | Represents BaseDamage of a weapon
  , _gwBaseDamage           :: Maybe Float
    -- | Represents BaseMultishot of a weapon
  , _gwBaseMultishot        :: Maybe Float
    -- | Represents Multishot of a weapon
  , _gwMultishot            :: Maybe Float
    -- | Represents Punchthrough of a weapon
  , _gwPunchthrough         :: Maybe Float
    -- | Represents Accuracy of a weapon
  , _gwAccuracy             :: Maybe Float
    -- | Represents CriticalChance of a weapon
  , _gwCriticalChance       :: Maybe Float
    -- | Represents CriticalMultiplier of a weapon
  , _gwCriticalMultiplier   :: Maybe Float
    -- | Represents StatusChance of a weapon
  , _gwStatusChance         :: Maybe Float
    -- | Represents FireRate of a weapon
  , _gwFireRate             :: Maybe Float
    -- | Represents ChargeAttack of a weapon
  , _gwChargeAttack         :: Maybe Float
    -- | Represents SpinAttack of a weapon
  , _gwSpinAttack           :: Maybe Float
    -- | Represents LeapAttack of a weapon
  , _gwLeapAttack           :: Maybe Float
    -- | Represents WallAttack of a weapon
  , _gwWallAttack           :: Maybe Float
    -- | Represents ChannelingMultiplier of a weapon
  , _gwChannelingMultiplier :: Maybe Float
    -- | Represents ChannelingCost of a weapon
  , _gwChannelingCost       :: Maybe Float
    -- | Represents OmegaAttenuation of a weapon
  , _gwOmegaAttenuation     :: Maybe Float
    -- | Represents Type of a weapon
  , _gwType                 :: Maybe Text
    -- | Represents Category of a weapon
  , _gwCategory             :: Maybe Text
    -- | Represents Ammo of a weapon
  , _gwAmmo                 :: Maybe Float
    -- | Represents all damage types of a weapon
  , _gwDamageTypes          :: Maybe GenericDamage
    -- | Represents Disposition of a weapon
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
