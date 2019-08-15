{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : AutoBuilder
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module is the main entry point for Warframe-Autobuilder via 'autoBuilder'

module AutoBuilder
  ( autoBuilder
  )
where

import           ClassyPrelude

import           Ranker.WeaponRankers

import           Ranker.ComparatorsGenerator

import qualified ArgInterface.ModsMapper.MeleeModsMapper
                                               as M
import qualified ArgInterface.ModsMapper.PistolModsMapper
                                               as P
import qualified ArgInterface.ModsMapper.RifleModsMapper
                                               as R
import qualified ArgInterface.ModsMapper.ShotgunModsMapper
                                               as S
import           ArgInterface.ArgInterface
import           ArgInterface.WeaponDataDirectoryTest


-- | autoBuilder is the main entry function that connects
--   user input parsed into ArgsParse to relavent functions.
--   This function sets some defaults and passes it to 'Ranker.WeaponRankers'
autoBuilder :: IO ()
autoBuilder = do

  checkIfDirectoryExists

  args <- parseArgs

  let (weaponName'', weaponType'') = fromMaybe ("N/A", "N/A") $ weaponName args

  case weaponType'' of

    -- Setting some default cases and passing 'safe' args to rankers
    "Rifle" -> rifleRanker' (fromMaybe [] $ neededMods args) -- sets needed mods to [] if parsing fails.
                            (fromMaybe R.modList $ unneededMods args) -- sets all available mods to ignore if parsing fails.
                            weaponName''
                            (Just $ fromMaybe 1 $ multiplier2 args) -- wraps basic multiplier with Maybe monad and sets 1 as a fallback value.
                            (comparatorGenerator (comparator args)) -- passes parsed comparator name to 'comparatorGenerator'

    "Shotgun" -> shotgunRanker' (fromMaybe [] $ neededMods args)
                                (fromMaybe S.modList $ unneededMods args)
                                weaponName''
                                (Just $ fromMaybe 1 $ multiplier2 args)
                                (comparatorGenerator (comparator args))

    "Melee" -> meleeRanker' (fromMaybe [] $ neededMods args)
                            (fromMaybe M.modList $ unneededMods args)
                            weaponName''
                            (Just $ fromMaybe 1 $ multiplier1 args)
                            (Just $ fromMaybe 1 $ multiplier2 args)
                            (comparatorGenerator (comparator args))

    "Secondary" -> pistolRanker' (fromMaybe [] $ neededMods args)
                                 (fromMaybe P.modList $ unneededMods args)
                                 weaponName''
                                 (Just $ fromMaybe 1 $ multiplier2 args)
                                 (comparatorGenerator (comparator args))

    "Unknown" -> do
      putStrLn "Could not derive type of weapon:"
      putStrLn $ pack $ show $ weaponName args

    wepType -> do
      putStrLn "Could not find the weapon in database."
      putStrLn $ "Derived type: " ++ wepType
