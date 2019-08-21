{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Builder.Mods.RifleModsMetaList
-- Maintainer  :  verystable <verystable@protonmail.com>
-- Stability   :  stable
--
-- The "Builder.Mods.RifleModsMetaList" module contains a list of tuples
-- each containing name of a mod and the mod itself (Build -> Build).
-- This list works as a precedence setter where damage mods have
-- the highest precedence and elemental mods the lowest.
-- The applicaton of these mods is done by each weapon type's 'applicator' functions.
-- Each 'applicator' functions can be found in "Builder.Applicator".
-- All applicators apply mods via right fold or foldr
-- So lowest precedence mod is applied first
-- and highest precedence mod is applied last.

module Builder.Mods.RifleModsMetaList where

import           ClassyPrelude

import qualified Data.Vector                   as V


import           Types.GenericWeapon

import           Builder.Mods.RifleMods.AmmoMods
import           Builder.Mods.RifleMods.CriticalChanceMods
import           Builder.Mods.RifleMods.CriticalMultiplierMods
import           Builder.Mods.RifleMods.DamageMods
import           Builder.Mods.RifleMods.ElementalDamageMods
import           Builder.Mods.RifleMods.FireRateMods
import           Builder.Mods.RifleMods.MagazineMods
import           Builder.Mods.RifleMods.MultishotMods
import           Builder.Mods.RifleMods.PhysicalDamageMods
import           Builder.Mods.RifleMods.RivenMods
import           Builder.Mods.RifleMods.SpecialMods
import           Builder.Mods.RifleMods.StatusMods

type Build = (GenericWeapon, [Text])

rifleModsMetaList :: GenericWeapon -> V.Vector (Text, Build -> Build)
rifleModsMetaList wep = V.fromList
  [
  -- Damage Mods
    ("amalgamSerration", amalgamSerration wep)
  , ("serration"       , serration wep)
  , ( "vileAcceleration"
    , vileAcceleration wep
    )

  -- Multishot Mods
  , ("splitChamber", splitChamber wep)
  , ( "vigilanteArmaments"
    , vigilanteArmaments wep
    )

  -- Critical Chance Mods
  , ("pointStrike"  , pointStrike wep)
  , ("argonScope"   , argonScope wep)
  , ("criticalDelay", criticalDelay wep)
  , ( "rifleExampleRiven"
    , rifleExampleRiven wep
    )

  -- Critical Multiplier Mods
  , ("vitalSense"  , vitalSense wep)
  , ("bladedRounds", bladedRounds wep)
  , ( "hammerShot"
    , hammerShot wep
    )

  -- Special Mods
  , ("metalAuger"     , metalAuger wep)
  , ("shred"          , shred wep)
  , ("catalyzerLink"  , catalyzerLink wep)
  , ("rifleAptitude"  , rifleAptitude wep)
  , ("primedShred"    , primedShred wep)
  , ("hunterMunitions", hunterMunitions wep)
  , ("hunterTrack"    , hunterTrack wep)
  , ( "continuousMisery"
    , continuousMisery wep
    )

  -- Fire Rate Mods
  , ("springLoadedChamber", springLoadedChamber wep)
  , ( "speedTrigger"
    , speedTrigger wep
    )

  -- Magazine Mods
  , ( "taintedMag"
    , taintedMag wep
    )

  -- General Info Mods
  , ( "ammoDrum"
    , ammoDrum wep
    )

  -- Physical Damage Mods
  , ("piercingCaliber", piercingCaliber wep)
  , ("piercingHit"    , piercingHit wep)
  , ("fangedFusillade", fangedFusillade wep)
  , ("sawtoothClip"   , sawtoothClip wep)
  , ("crashCourse"    , crashCourse wep)
  , ( "rupture"
    , rupture wep
    )

  -- Elemental Damage Mods
  , ("cryoRounds"        , cryoRounds wep)
  , ("primedCryoRounds"  , primedCryoRounds wep)
  , ("hellfire"          , hellfire wep)
  , ("primedHellfire"    , primedHellfire wep)
  , ("infectedClip"      , infectedClip wep)
  , ("primedInfectedClip", primedInfectedClip wep)
  , ("stormbringer"      , stormbringer wep)
  , ("primedStormbringer", primedStormbringer wep)
  , ("rimeRounds"        , rimeRounds wep)
  , ("thermiteRounds"    , thermiteRounds wep)
  , ("malignantForce"    , malignantForce wep)
  , ("highVoltage"       , highVoltage wep)
  , ("wildfire"          , wildfire wep)
  ]
