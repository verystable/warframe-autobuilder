{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Builder.Mods.ShotgunModsMetaList
-- Maintainer  :  verystable <verystable@protonmail.com>
-- Stability   :  stable
--
-- The "Builder.Mods.ShotgunModsMetaList" module contains a list of tuples
-- each containing name of a mod and the mod itself (Build -> Build).
-- This list works as a precedence setter where damage mods have
-- the highest precedence and elemental mods the lowest.
-- The applicaton of these mods is done by each weapon type's 'applicator' functions.
-- Each 'applicator' functions can be found in "Builder.Applicator".
-- All applicators apply mods via right fold or foldr
-- So lowest precedence mod is applied first
-- and highest precedence mod is applied last.

module Builder.Mods.ShotgunModsMetaList where

import           ClassyPrelude

import qualified Data.Vector                   as V

import           Types.GenericWeapon

import           Builder.Mods.ShotgunMods.AmmoMods
import           Builder.Mods.ShotgunMods.CriticalChanceMods
import           Builder.Mods.ShotgunMods.CriticalMultiplierMods
import           Builder.Mods.ShotgunMods.DamageMods
import           Builder.Mods.ShotgunMods.ElementalDamageMods
import           Builder.Mods.ShotgunMods.FireRateMods
import           Builder.Mods.ShotgunMods.MagazineMods
import           Builder.Mods.ShotgunMods.MultishotMods
import           Builder.Mods.ShotgunMods.PhysicalDamageMods
import           Builder.Mods.ShotgunMods.SpecialMods
import           Builder.Mods.ShotgunMods.StatusMods

type Build = (GenericWeapon, [Text])

shotgunModsMetaList :: GenericWeapon -> V.Vector (Text, Build -> Build)
shotgunModsMetaList wep = V.fromList
  [
    -- Damage Mods
    ("pointBlank", pointBlank wep)
  , ( "primedPointBlank"
    , primedPointBlank wep
    )

    -- Multishot Mods
  , ( "hellsChamber"
    , hellsChamber wep
    )

    -- Ammo Mods
  , ("ammoStock", ammoStock wep)
  , ( "shellCompression"
    , shellCompression wep
    )

    -- Critical Chance Mods
  , ("blunderbuss", blunderbuss wep)
  , ("laserSight" , laserSight wep)
  , ( "criticalDeceleration"
    , criticalDeceleration wep
    )

    -- Critical Multiplier Mods
  , ("primedRavage", primedRavage wep)
  , ("ravage"      , ravage wep)
  , ( "sharpnelShot"
    , sharpnelShot wep
    )

    -- Fire Rate Mods
  , ("repeaterClip", repeaterClip wep)
  , ( "shotgunSpazz"
    , shotgunSpazz wep
    )

    -- Magazine Size Mods
  , ( "burdenedMagazine"
    , burdenedMagazine wep
    )

    -- Speical Mods
  , ("hunterMunitions" , hunterMunitions wep)
  , ("hunterTrack"     , hunterTrack wep)
  , ("lingeringTorment", lingeringTorment wep)
  , ("seekingForce"    , seekingForce wep)
  , ( "seekingFury"
    , seekingFury wep
    )

    -- Status Mods
  , ("nanoApplicator", nanoApplicator wep)
  , ( "shotgunSavvy"
    , shotgunSavvy wep
    )

    -- Physical Damage Mods
  , ("acceleratedBlast" , acceleratedBlast wep)
  , ("breachLoader"     , breachLoader wep)
  , ("flechette"        , flechette wep)
  , ("sweepingSerration", sweepingSerration wep)
  , ("shredder"         , shredder wep)
  , ("fullContact"      , fullContact wep)
  , ( "disruptor"
    , disruptor wep
    )

    -- Elemental Damage Mods
  , ("chillingGrasp"         , chillingGrasp wep)
  , ("primedChillingGrasp"   , primedChillingGrasp wep)
  , ("incendiaryCoat"        , incendiaryCoat wep)
  , ("primedIncendiaryCoat"  , primedIncendiaryCoat wep)
  , ("contagiousSpread"      , contagiousSpread wep)
  , ("primedContagiousSpread", primedContagiousSpread wep)
  , ("chargedShell"          , chargedShell wep)
  , ("primedChargedShell"    , primedChargedShell wep)
  , ("frigidBlast"           , frigidBlast wep)
  , ("scatteringInferno"     , scatteringInferno wep)
  , ("toxicBarrage"          , toxicBarrage wep)
  , ("shellShock"            , shellShock wep)
  , ("blaze"                 , blaze wep)
  , ("chillingReload"        , chillingReload wep)
  ]
