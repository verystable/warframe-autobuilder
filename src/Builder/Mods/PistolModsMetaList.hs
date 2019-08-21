{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Builder.Mods.PistolModsMetaList
-- Maintainer  :  verystable <verystable@protonmail.com>
-- Stability   :  stable
--
-- The "Builder.Mods.PistolModsMetaList" module contains a list of tuples
-- each containing name of a mod and the mod itself (Build -> Build).
-- This list works as a precedence setter where damage mods have
-- the highest precedence and elemental mods the lowest.
-- The applicaton of these mods is done by each weapon type's 'applicator' functions.
-- Each 'applicator' functions can be found in "Builder.Applicator".
-- All applicators apply mods via right fold or foldr
-- So lowest precedence mod is applied first
-- and highest precedence mod is applied last.

module Builder.Mods.PistolModsMetaList where

import           ClassyPrelude
import qualified Data.Vector                   as V

import           Types.GenericWeapon

import           Builder.Mods.PistolMods.AmmoMods
import           Builder.Mods.PistolMods.CriticalChanceMods
import           Builder.Mods.PistolMods.CriticalMultiplierMods
import           Builder.Mods.PistolMods.DamageMods
import           Builder.Mods.PistolMods.ElementalDamageMods
import           Builder.Mods.PistolMods.FireRateMods
import           Builder.Mods.PistolMods.MagazineMods
import           Builder.Mods.PistolMods.MultishotMods
import           Builder.Mods.PistolMods.RivenMods
import           Builder.Mods.PistolMods.PhysicalDamageMods
import           Builder.Mods.PistolMods.SpecialMods
import           Builder.Mods.PistolMods.StatusMods

type Build = (GenericWeapon, [Text])

pistolModsMetaList :: GenericWeapon -> V.Vector (Text, Build -> Build)
pistolModsMetaList wep = V.fromList
  [
    -- Damage Mods
    ("hollowPoint" , hollowPoint wep)
  , ("hornetStrike", hornetStrike wep)
  , ( "augurPact"
    , augurPact wep
    )

  -- Multishot Mods
  , ("barrelDiffusion", barrelDiffusion wep)
  , ("lethalTorrent"  , lethalTorrent wep)
  , ( "amalgamBarrelDiffusion"
    , amalgamBarrelDiffusion wep
    )

  -- Ammo Mods
  , ( "trickMag"
    , trickMag wep
    )

  -- Critical Chance Mods
  , ("pistolGambit"       , pistolGambit wep)
  , ("primedPistolGambit" , primedPistolGambit wep)
  , ("hydraulicCrosshairs", hydraulicCrosshairs wep)
  , ( "creepingBullseye"
    , creepingBullseye wep
    )

  -- Critical Multiplier Mods
  , ("primedTargetCracker", primedTargetCracker wep)
  , ("targetCracker"      , targetCracker wep)
  , ("sharpenedBullets"   , sharpenedBullets wep)
  , ( "pistolExampleRiven"
    , pistolExampleRiven wep
    )

  -- Fire Rate Mods
  , ("anemicAgility", anemicAgility wep)
  , ( "gunslinger"
    , gunslinger wep
    )

  -- Magazine Size Mods
  , ("pressurizedMagazine", pressurizedMagazine wep)
  , ("taintedClip"        , taintedClip wep)
  , ("primedSlipMagazine" , primedSlipMagazine wep)
  , ( "slipMagazine"
    , slipMagazine wep
    )

  -- Status Mods
  , ("embeddedCatalyzer", embeddedCatalyzer wep)
  , ("sureShot"         , sureShot wep)
  , ( "stunningSpeed"
    , stunningSpeed wep
    )

  -- Special Mods
  , ("augurSeeker"   , augurSeeker wep)
  , ("perpetualAgony", perpetualAgony wep)
  , ( "seeker"
    , seeker wep
    )

  -- Physical Damage Mods
  , ("bore"     , bore wep)
  , ("noReturn" , noReturn wep)
  , ("maim"     , maim wep)
  , ("razorShot", razorShot wep)
  , ("pummel"   , pummel wep)
  , ( "concussionRounds"
    , concussionRounds wep
    )

  -- Elemental Damage Mods
  , ("deepFreeze"          , deepFreeze wep)
  , ("primedDeepFreeze"    , primedDeepFreeze wep)
  , ("heatedCharge"        , heatedCharge wep)
  , ("primedHeatedCharge"  , primedHeatedCharge wep)
  , ("pathogenRounds"      , pathogenRounds wep)
  , ("primedPathogenRounds", primedPathogenRounds wep)
  , ("convulsion"          , convulsion wep)
  , ("primedConvulsion"    , primedConvulsion wep)
  , ("frostbite"           , frostbite wep)
  , ("scorch"              , scorch wep)
  , ("pistolPestilence"    , pistolPestilence wep)
  , ("jolt"                , jolt wep)
  , ("iceStorm"            , iceStorm wep)
  ]
