{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Ranker.WeaponRankers
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module contains 4 different rankers for
-- 4 different types of 'ComprehensiveWeapon'.
-- Each [Weapon Type]ranker function locates the weapon data file
-- applies 'Ranker.Ranker.ranker' function and returns
-- either a successful build or a parse/read error message.

module Ranker.WeaponRankers where

import           ClassyPrelude

import           Types.ComprehensiveWeapon
import           Types.GenericWeapon

import           Ranker.Ranker

import           GenericFunctions.GenericFunctions

import qualified ArgInterface.ModsMapper.MeleeModsMapper
                                               as M
import qualified ArgInterface.ModsMapper.PistolModsMapper
                                               as P
import qualified ArgInterface.ModsMapper.RifleModsMapper
                                               as R
import qualified ArgInterface.ModsMapper.ShotgunModsMapper
                                               as S

import qualified Builder.Applicator.MeleeModApplicator
                                               as M
import qualified Builder.Applicator.PistolModApplicator
                                               as P
import qualified Builder.Applicator.RifleModApplicator
                                               as R
import qualified Builder.Applicator.ShotgunModApplicator
                                               as S

import           Printer.ComprehensiveWeaponPrinter

import           Text.PrettyPrint.Boxes
import           System.Directory


rifleRanker'
  :: Mods
  -> Mods
  -> Text
  -> Maybe Float
  -> (ComprehensiveWeapon -> ComprehensiveWeapon -> Ordering)
  -> IO ()
rifleRanker' neededMods' unneededMods' weaponName' multiplier' comparator' = do

  homeDir <- getHomeDirectory

  let dataFileDirectory = homeDir ++ "/.config/"

  -- Locates weapon file
  wep' <-
    readWeapon
    $  dataFileDirectory
    ++ "warframe-autobuilder-data/Primary_Weapons/"
    ++ unpack weaponName'

  -- Converts weapon file into a Build type
  let target = (, []) <$> wep' :: Either String (GenericWeapon, Mods)

  -- Lambda function to apply over functor of GenericWeapon
      finalBuild =
        (\target' -> ranker neededMods'
                            unneededMods'
                            target'
                            R.applicator -- applicator function
                            R.modNameMapper -- string to int converter
                            R.modList -- list of available mods
                            multiplier' -- generic multiplier
                            comparator' -- comparator
          )
          <$> target

  -- Either prints error occured during parsing or successful build
  either
    (\err -> putStrLn ("Error parsing " ++ weaponName' ++ ": " ++ pack err))
    printBox
    (printComprehensiveWeapon finalBuild)

shotgunRanker'
  :: Mods
  -> Mods
  -> Text
  -> Maybe Float
  -> (ComprehensiveWeapon -> ComprehensiveWeapon -> Ordering)
  -> IO ()
shotgunRanker' neededMods' unneededMods' weaponName' multiplier' comparator' =
  do

    homeDir <- getHomeDirectory

    let dataFileDirectory = homeDir ++ "/.config/"

  -- Locates weapon file
    wep' <-
      readWeapon
      $  dataFileDirectory
      ++ "warframe-autobuilder-data/Primary_Weapons/"
      ++ unpack weaponName'

    -- Converts weapon file into a Build type
    let target = (, []) <$> wep' :: Either String (GenericWeapon, Mods)

    -- Lambda function to apply over functor of GenericWeapon
        finalBuild =
          (\target' -> ranker neededMods'
                              unneededMods'
                              target'
                              S.applicator -- applicator function
                              S.modNameMapper -- string to int converter
                              S.modList -- list of available mods
                              multiplier' -- generic multiplier
                              comparator' -- comparator
            )
            <$> target

    -- Either prints error occured during parsing or successful build
    either
      (\err -> putStrLn ("Error parsing " ++ weaponName' ++ ": " ++ pack err))
      printBox
      (printComprehensiveWeapon finalBuild)

meleeRanker'
  :: Mods
  -> Mods
  -> Text
  -> Maybe Float
  -> Maybe Float
  -> (ComprehensiveWeapon -> ComprehensiveWeapon -> Ordering)
  -> IO ()
meleeRanker' neededMods' unneededMods' weaponName' multiplier1' multiplier2' comparator'
  = do

    homeDir <- getHomeDirectory

    let dataFileDirectory = homeDir ++ "/.config/"

  -- Locates weapon file
    wep' <-
      readWeapon
      $  dataFileDirectory
      ++ "warframe-autobuilder-data/Melee_Weapons/"
      ++ unpack weaponName'

    -- Converts weapon file into a Build type
    let target = (, []) <$> wep' :: Either String (GenericWeapon, Mods)

    -- Lambda function to apply over functor of GenericWeapon
        finalBuild =
          (\target' -> ranker neededMods'
                              unneededMods'
                              target'
                              (M.applicator multiplier1')  -- applicator function with combo counter
                              M.modNameMapper -- string to int converter
                              M.modList -- list of available mods
                              multiplier2' -- generic multiplier (combo counter, frame buff, etc.)
                              comparator' -- comparator
            )
            <$> target

    -- Either prints error occured during parsing or successful build
    either
      (\err -> putStrLn ("Error parsing " ++ weaponName' ++ ": " ++ pack err))
      printBox
      (printComprehensiveWeapon finalBuild)

pistolRanker'
  :: Mods
  -> Mods
  -> Text
  -> Maybe Float
  -> (ComprehensiveWeapon -> ComprehensiveWeapon -> Ordering)
  -> IO ()
pistolRanker' neededMods' unneededMods' weaponName' multiplier' comparator' =
  do

    homeDir <- getHomeDirectory

    let dataFileDirectory = homeDir ++ "/.config/"

  -- Locates weapon file
    wep' <-
      readWeapon
      $  dataFileDirectory
      ++ "warframe-autobuilder-data/Secondary_Weapons/"
      ++ unpack weaponName'

    -- Converts weapon file into a Build type
    let target = (, []) <$> wep' :: Either String (GenericWeapon, Mods)

    -- Lambda function to apply over functor of GenericWeapon
        finalBuild =
          (\target' -> ranker neededMods'
                              unneededMods'
                              target'
                              P.applicator -- applicator function
                              P.modNameMapper -- string to int converter
                              P.modList -- list of available mods
                              multiplier' -- generic multiplier
                              comparator' -- comparator
            )
            <$> target

    -- Either prints error occured during parsing or successful build
    either
      (\err -> putStrLn ("Error parsing " ++ weaponName' ++ ": " ++ pack err))
      printBox
      (printComprehensiveWeapon finalBuild)
