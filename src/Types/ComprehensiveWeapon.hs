{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Types.ComprehensiveWeapon
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module contains 'ComprehensiveWeapon' type.

module Types.ComprehensiveWeapon where

import           Types.GeneralTypes
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
import           Types.GenericWeapon

-- | Generic ProcProcChance Record
--   if the weapon doesn't have one of these fields, it's set to 'null' in the 'json' file
data GenericProcChances = GenericProcChances { _gpcImpact      :: Maybe Float
                                             , _gpcPuncture    :: Maybe Float
                                             , _gpcSlash       :: Maybe Float
                                             , _gpcHeat        :: Maybe Float
                                             , _gpcCold        :: Maybe Float
                                             , _gpcToxin       :: Maybe Float
                                             , _gpcElectricity :: Maybe Float
                                             , _gpcBlast       :: Maybe Float
                                             , _gpcGas         :: Maybe Float
                                             , _gpcRadiation   :: Maybe Float
                                             , _gpcViral       :: Maybe Float
                                             , _gpcCorrosive   :: Maybe Float
                                             , _gpcMagnetic    :: Maybe Float
                                             } deriving (Show, Generic, Eq, Ord)

makeLenses ''GenericProcChances

instance ToJSON GenericProcChances where
      -- No need to provide a toJSON implementation.

      -- Writing a simple toEncoding implementation
      toEncoding = genericToEncoding defaultOptions

instance FromJSON GenericProcChances where
  parseJSON = withObject "GenericProcChances" $ \o -> GenericProcChances
    <$> o .:? "_gpcImpact"
    <*> o .:? "_gpcPuncture"
    <*> o .:? "_gpcSlash"
    <*> o .:? "_gpcHeat"
    <*> o .:? "_gpcCold"
    <*> o .:? "_gpcToxin"
    <*> o .:? "_gpcElectricity"
    <*> o .:? "_gpcBlast"
    <*> o .:? "_gpcGas"
    <*> o .:? "_gpcRradiation"
    <*> o .:? "_gpcViral"
    <*> o .:? "_gpcCorrosive"
    <*> o .:? "_gpcMagnetic"

-- | Generic ProcDamage Record
--   if the weapon doesn't have one of these fields, it's set to 'null' in the 'json' file
data GenericProcDPSs = GenericProcDPSs { _gpdImpact      :: Maybe Float
                                       , _gpdPuncture    :: Maybe Float
                                       , _gpdSlash       :: Maybe Float
                                       , _gpdHeat        :: Maybe Float
                                       , _gpdCold        :: Maybe Float
                                       , _gpdToxin       :: Maybe Float
                                       , _gpdElectricity :: Maybe Float
                                       , _gpdBlast       :: Maybe Float
                                       , _gpdGas         :: Maybe Float
                                       , _gpdRadiation   :: Maybe Float
                                       , _gpdViral       :: Maybe Float
                                       , _gpdCorrosive   :: Maybe Float
                                       , _gpdMagnetic    :: Maybe Float
                                       } deriving (Show, Generic, Eq, Ord)

makeLenses ''GenericProcDPSs

instance ToJSON GenericProcDPSs where
      -- No need to provide a toJSON implementation.

      -- Writing a simple toEncoding implementation
      toEncoding = genericToEncoding defaultOptions

instance FromJSON GenericProcDPSs where
  parseJSON = withObject "GenericProcDPSs" $ \o -> GenericProcDPSs
    <$> o .:? "_gpcImpact"
    <*> o .:? "_gpcPuncture"
    <*> o .:? "_gpcSlash"
    <*> o .:? "_gpcHeat"
    <*> o .:? "_gpcCold"
    <*> o .:? "_gpcToxin"
    <*> o .:? "_gpcElectricity"
    <*> o .:? "_gpcBlast"
    <*> o .:? "_gpcGas"
    <*> o .:? "_gpcRadiation"
    <*> o .:? "_gpcViral"
    <*> o .:? "_gpcCorrosive"
    <*> o .:? "_gpcMagnetic"

data ComprehensiveWeapon = ComprehensiveWeapon { _build                :: Build
                                               , _burstDPS             :: Maybe Float
                                               , _comboCounter         :: Maybe Float
                                               , _sustainedDPS         :: Maybe Float
                                               , _procChances          :: GenericProcChances
                                               , _procDPSs             :: GenericProcDPSs
                                               , _adjustedTotalDamage  :: Maybe Float
                                               , _adjustedBurstDPS     :: Maybe Float
                                               , _adjustedSustainedDPS :: Maybe Float
                                               } deriving (Show, Generic, Eq, Ord)

makeLenses ''ComprehensiveWeapon
