{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Builder.Mods.MeleeModsMetaList
-- Maintainer  :  verystable <verystable@protonmail.com>
-- Stability   :  stable
--
-- The "Builder.Mods.MeleeModsMetaList" module contains a list of tuples
-- each containing name of a mod and the mod itself (Build -> Build).
-- This list works as a precedence setter where damage mods have
-- the highest precedence and elemental mods the lowest.
-- The applicaton of these mods is done by each weapon type's 'applicator' functions.
-- Each 'applicator' functions can be found in "Builder.Applicator".
-- All applicators apply mods via right fold or foldr
-- So lowest precedence mod is applied first
-- and highest precedence mod is applied last.

module Builder.Mods.MeleeModsMetaList where

import           ClassyPrelude

import qualified Data.Vector                   as V

import           Types.GenericWeapon

import           Builder.Mods.MeleeMods.AmmoMods
import           Builder.Mods.MeleeMods.AttackSpeedMods
import           Builder.Mods.MeleeMods.CriticalChanceMods
import           Builder.Mods.MeleeMods.CriticalMultiplierMods
import           Builder.Mods.MeleeMods.DamageMods
import           Builder.Mods.MeleeMods.ElementalDamageMods
import           Builder.Mods.MeleeMods.MagazineMods
import           Builder.Mods.MeleeMods.MultishotMods
import           Builder.Mods.MeleeMods.PhysicalDamageMods
import           Builder.Mods.MeleeMods.RivenMods
import           Builder.Mods.MeleeMods.SpecialMods
import           Builder.Mods.MeleeMods.StatusMods
import           Types.GeneralTypes

meleeModsMetaList
  :: GenericWeapon -> Maybe Float -> V.Vector (Text, Build -> Build)
meleeModsMetaList wep multiplier =
  V.fromList
    $ [
    -- Damage Mods
        ("pressurePoint"      , pressurePoint wep)
      , ("primedPressurePoint", primedPressurePoint wep)
      , ("sacrificialPressure", sacrificialPressure wep)
      , ("spoiledStrike"      , spoiledStrike wep)
      , ("conditionOverload"  , conditionOverload wep)
      , ( "meleeExampleRiven"
        , meleeExampleRiven wep
        )

    -- Attack Speed Mods
      , ("quickening", quickening wep)
      , ("berserker" , berserker wep)
      , ("fury"      , fury wep)
      , ("primedFury", primedFury wep)
      , ( "gladiatorVice"
        , gladiatorVice wep
        )

    -- Critical Chance Mods
      , ("trueSteel"     , trueSteel wep)
      , ("bloodRush"     , bloodRush multiplier wep)
      , ("truePunishment", truePunishment wep)
      , ("maimingStrike" , maimingStrike wep)
      , ( "sacrificialSteel"
        , sacrificialSteel wep
        )

    -- Critical Multiplier Mods
      , ("organShatter"       , organShatter wep)
      , ("amalgamOrganShatter", amalgamOrganShatter wep)
      , ( "gladiatorMight"
        , gladiatorMight wep
        )

    -- Status Chance Mods
      , ("driftingContact", driftingContact wep)
      , ("meleeProwess"   , meleeProwess wep)
      , ( "weepingWounds"
        , weepingWounds multiplier wep
        )

    -- Physical Damage Mods
      , ("collisionForce"   , collisionForce wep)
      , ("heavyTrauma"      , heavyTrauma wep)
      , ("primedHeavyTrauma", primedHeavyTrauma wep)
      , ("sunderingStrike"  , sunderingStrike wep)
      , ("augerStrike"      , augerStrike wep)
      , ("primedAugerStrike", primedAugerStrike wep)
      , ("buzzKill"         , buzzKill wep)
      , ("primedBuzzKill"   , primedBuzzKill wep)
      , ("jaggedEdge"       , jaggedEdge wep)
      , ( "rendingStrike"
        , rendingStrike wep
        )

    -- Elemental Damage Mods
      , ("northWind"          , northWind wep)
      , ("primedNorthWind"    , primedNorthWind wep)
      , ("moltenImpact"       , moltenImpact wep)
      , ("primedMoltenImpact" , primedMoltenImpact wep)
      , ("feverStrike"        , feverStrike wep)
      , ("primedFeverStrike"  , primedFeverStrike wep)
      , ("shockingTouch"      , shockingTouch wep)
      , ("primedShockingTouch", primedShockingTouch wep)
      , ("viciousFrost"       , viciousFrost wep)
      , ("vocanicEdge"        , vocanicEdge wep)
      , ("virulentScourge"    , virulentScourge wep)
      , ("voltaicStrike"      , voltaicStrike wep)
      , ("focusEnergy"        , focusEnergy wep)
      ]
