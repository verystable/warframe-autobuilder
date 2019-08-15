{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      :  ComprehensiveWeapon.ComprehensiveFunctions
-- Maintainer  :  verystable <verystable@protonmail.com>
-- Stability   :  stable
--
-- This module contains the function 'makeComprehensive' which takes a build type
-- i.e. (GenericWeapon, Mods) and turns that into a 'ComprehensiveWeapon' type.
-- ComprehensiveWeapon contains in addition to whatever the preceding '(GenericWeapon, Mods)'
-- burstDPS
-- sustainedDPS
-- adjustedTotalDamage
-- adjustedBurstDPS
-- adjustedSustainedDPS
-- comboCounter (e.g. melee combo counter, sniper rifle combo counter, etc.)
-- procChances of each of the damage types
-- procDPSs of each of the damage types


module ComprehensiveWeapon.ComprehensiveFunctions where

import           ClassyPrelude
import           ComprehensiveWeapon.ProcChances
import           ComprehensiveWeapon.ProcDamages
import           ComprehensiveWeapon.SpecialAddativeFunctions
import           Control.Lens
import           Data.List                      ( nub )
import           GenericFunctions.GenericFunctions
import           Types.ComprehensiveWeapon
import           Types.GenericWeapon

makeComprehensive :: Maybe Float -> (GenericWeapon, Mods) -> ComprehensiveWeapon
makeComprehensive multiplier (gw, mods) = ComprehensiveWeapon
  { _build                = build'
  , _burstDPS             = burstDPS'
  , _comboCounter         = multiplier
  , _sustainedDPS         = sustainedDPS'
  , _procChances          = GenericProcChances
    { _gpcImpact      = procChanceImpact
    , _gpcPuncture    = procChancePuncture
    , _gpcSlash       = procChanceSlash
    , _gpcHeat        = procChanceHeat
    , _gpcCold        = procChanceCold
    , _gpcToxin       = procChanceToxin
    , _gpcElectricity = procChanceElectricity
    , _gpcBlast       = procChanceBlast
    , _gpcGas         = procChanceGas
    , _gpcRadiation   = procChanceRadiation
    , _gpcViral       = procChanceViral
    , _gpcCorrosive   = procChanceCorrosive
    , _gpcMagnetic    = procChanceMagnetic
    }
  , _procDPSs             = GenericProcDPSs
    { _gpdImpact      = procDPSImpact
    , _gpdPuncture    = procDPSPuncture
    , _gpdSlash       = procDPSSlash
    , _gpdHeat        = procDPSHeat
    , _gpdCold        = procDPSCold
    , _gpdToxin       = procDPSToxin
    , _gpdElectricity = procDPSElectricity
    , _gpdBlast       = procDPSBlast
    , _gpdGas         = procDPSGas
    , _gpdRadiation   = procDPSRadiation
    , _gpdViral       = procDPSViral
    , _gpdCorrosive   = procDPSCorrosive
    , _gpdMagnetic    = procDPSMagnetic
    }
  , _adjustedTotalDamage  = adjustedTotalDamage'
  , _adjustedBurstDPS     = adjustedBurstDPS'
  , _adjustedSustainedDPS = adjustedSustainedDPS'
  }
 where
  gw' = applyMultiplier
    multiplier
    (gw & gwBaseDamage %~ (\a -> (*) <$> a <*> multiplier))
  build'    = (gw', nub mods)

  burstDPS' = (*) <$> gw' ^. gwFireRate <*> gw' ^. gwTotalDamage

  -- sustainedDPS = (totalDamage * magazineSize) / (reloadTime + (magazineSize / fireRate))
  sustainedDPS' =
    sustainedDPSAlter
      $   (/)
      <$> ((*) <$> (gw' ^. gwTotalDamage) <*> (gw' ^. gwMagazineSize))
      <*> (   (+)
          <$> (gw' ^. gwReloadTime)
          <*> ((/) <$> gw' ^. gwMagazineSize <*> gw' ^. gwFireRate)
          )

  adjustedTotalDamage' =
    (+) <$> (gw' ^. gwTotalDamage) <*> totalDamageFromProcs
  adjustedBurstDPS' = (*) <$> gw' ^. gwFireRate <*> adjustedTotalDamage'
  adjustedSustainedDPS' =
    sustainedDPSAlter
      $   (/)
      <$> ((*) <$> adjustedTotalDamage' <*> (gw' ^. gwMagazineSize))
      <*> (   (+)
          <$> (gw' ^. gwReloadTime)
          <*> ((/) <$> gw' ^. gwMagazineSize <*> gw' ^. gwFireRate)
          )

  procChanceImpact =
    (+) <$> saImpact (gw', mods) <*> getProcChancePhysical gdImpact gw'
  procChancePuncture =
    (+) <$> saPuncture (gw', mods) <*> getProcChancePhysical gdPuncture gw'
  procChanceSlash =
    (+) <$> saSlash (gw', mods) <*> getProcChancePhysical gdSlash gw'
  procChanceHeat =
    (+) <$> saHeat (gw', mods) <*> getProcChanceElemental gdHeat gw'
  procChanceCold =
    (+) <$> saCold (gw', mods) <*> getProcChanceElemental gdCold gw'
  procChanceToxin =
    (+) <$> saToxin (gw', mods) <*> getProcChanceElemental gdToxin gw'
  procChanceElectricity =
    (+)
      <$> saElectricity (gw', mods)
      <*> getProcChanceElemental gdElectricity gw'
  procChanceBlast =
    (+) <$> saBlast (gw', mods) <*> getProcChanceElemental gdBlast gw'
  procChanceGas =
    (+) <$> saGas (gw', mods) <*> getProcChanceElemental gdGas gw'
  procChanceRadiation =
    (+) <$> saRadiation (gw', mods) <*> getProcChanceElemental gdRadiation gw'
  procChanceViral =
    (+) <$> saViral (gw', mods) <*> getProcChanceElemental gdViral gw'
  procChanceCorrosive =
    (+) <$> saCorrosive (gw', mods) <*> getProcChanceElemental gdCorrosive gw'
  procChanceMagnetic =
    (+) <$> saMagnetic (gw', mods) <*> getProcChanceElemental gdMagnetic gw'

  averageProcDamageImpact =
    (*) <$> procChanceImpact <*> (getImpactProcDamage (gw', mods))
  averageProcDamagePuncture =
    (*) <$> procChancePuncture <*> (getPunctureProcDamage (gw', mods))
  averageProcDamageSlash =
    (*) <$> procChanceSlash <*> (getSlashProcDamage (gw', mods))
  averageProcDamageHeat =
    (*) <$> procChanceHeat <*> (getHeatProcDamage (gw', mods))
  averageProcDamageCold =
    (*) <$> procChanceCold <*> (getColdProcDamage (gw', mods))
  averageProcDamageToxin =
    (*) <$> procChanceToxin <*> (getToxinProcDamage (gw', mods))
  averageProcDamageElectricity =
    (*) <$> procChanceElectricity <*> (getElectricityProcDamage (gw', mods))
  averageProcDamageBlast =
    (*) <$> procChanceBlast <*> (getBlastProcDamage (gw', mods))
  averageProcDamageGas =
    (*) <$> procChanceGas <*> (getGasProcDamage (gw', mods))
  averageProcDamageRadiation =
    (*) <$> procChanceRadiation <*> (getRadiationProcDamage (gw', mods))
  averageProcDamageViral =
    (*) <$> procChanceViral <*> (getViralProcDamage (gw', mods))
  averageProcDamageCorrosive =
    (*) <$> procChanceCorrosive <*> (getCorrosiveProcDamage (gw', mods))
  averageProcDamageMagnetic =
    (*) <$> procChanceMagnetic <*> (getMagneticProcDamage (gw', mods))

  procDPSImpact   = (*) <$> (gw' ^. gwFireRate) <*> averageProcDamageImpact
  procDPSPuncture = (*) <$> (gw' ^. gwFireRate) <*> averageProcDamagePuncture
  procDPSSlash    = (*) <$> (gw' ^. gwFireRate) <*> averageProcDamageSlash
  procDPSHeat     = (*) <$> (gw' ^. gwFireRate) <*> averageProcDamageHeat
  procDPSCold     = (*) <$> (gw' ^. gwFireRate) <*> averageProcDamageCold
  procDPSToxin    = (*) <$> (gw' ^. gwFireRate) <*> averageProcDamageToxin
  procDPSElectricity =
    (*) <$> (gw' ^. gwFireRate) <*> averageProcDamageElectricity
  procDPSBlast = (*) <$> (gw' ^. gwFireRate) <*> averageProcDamageBlast
  procDPSGas = (*) <$> (gw' ^. gwFireRate) <*> averageProcDamageGas
  procDPSRadiation = (*) <$> (gw' ^. gwFireRate) <*> averageProcDamageRadiation
  procDPSViral = (*) <$> (gw' ^. gwFireRate) <*> averageProcDamageViral
  procDPSCorrosive = (*) <$> (gw' ^. gwFireRate) <*> averageProcDamageCorrosive
  procDPSMagnetic = (*) <$> (gw' ^. gwFireRate) <*> averageProcDamageMagnetic

  totalDamageFromProcs = Just $ sum $ catMaybes
    [ averageProcDamageImpact
    , averageProcDamagePuncture
    , averageProcDamageSlash
    , averageProcDamageHeat
    , averageProcDamageCold
    , averageProcDamageToxin
    , averageProcDamageElectricity
    , averageProcDamageBlast
    , averageProcDamageGas
    , averageProcDamageRadiation
    , averageProcDamageViral
    , averageProcDamageCorrosive
    , averageProcDamageMagnetic
    ]

  sustainedDPSAlter sdps = case (isNaN <$> sdps) of
    (Just True) -> burstDPS'
    _           -> sdps

