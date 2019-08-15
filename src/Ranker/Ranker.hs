{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : Ranker.Ranker
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module applies comparators on relevant
-- weapon types.

module Ranker.Ranker where

import           ClassyPrelude
import           Data.List                      ( (\\) )

import           Types.GenericWeapon

import           Types.ComprehensiveWeapon

import           ComprehensiveWeapon.ComprehensiveFunctions

-- | Non-determinitic version of 'take'
ndetake :: Int -> [Int] -> [[Int]]
ndetake n xs = go (length xs) n xs
 where
  go spare i _ | i > spare   = [[]]
  go spare i ys | i == spare = [ys]
  go _ 0 _                   = [[]]
  go _ _ []                  = [[]]
  go spare i (x : ys) =
    map (x :) (go (spare - 1) (i - 1) ys) ++ go (spare - 1) i ys

-- | Ranker takes a list of Mods and a function to optimize for
--   and returns an optimized build
ranker
  :: Mods
  -> Mods
  -> (GenericWeapon, Mods)
  -> ((GenericWeapon, Mods) -> [Int] -> (GenericWeapon, Mods))
  -> ([Text] -> [Int])
  -> [Text]
  -> Maybe Float
  -> (  ComprehensiveWeapon
     -> ComprehensiveWeapon
     -> Ordering
     )
  -> ComprehensiveWeapon
ranker theseMods notTheseMods (gw, mods) modApplicator modMapper modList multiplier comparator
  = maximumByEx
    comparator
  -- List of Comprehensive Builds
    (   makeComprehensive multiplier
    .   modApplicator (gw, mods)
    <$> allPossibleIteration
    )
 where
    -- List of mods names mapped over by modMapper from Mod Mappers
  theseModsIndices    = modMapper theseMods :: [Int]
  -- List of mods names (to be ignored) mapped over by modMapper from Mod Mappers
  notTheseModsIndices = modMapper notTheseMods :: [Int]

  -- Substracts needed and ignored mods from the list of all mods
  restOfMods =
    ([0 .. length modList - 1] -- list of all mods
                               Data.List.\\ theseModsIndices) -- list of needed mods
      Data.List.\\ notTheseModsIndices -- list of un-needed mods

  -- Generates combination of restOfMods and adds needed mods to each sublist
  restPossibleIteration = ndetake (8 - length theseModsIndices) restOfMods

  -- Final list of all possible mod combinations
  allPossibleIteration  = sort . (++ theseModsIndices) <$> restPossibleIteration

-- | List of ComprehensiveWeapon
listOfBuilds
  :: Mods
  -> Mods
  -> (GenericWeapon, Mods)
  -> ((GenericWeapon, Mods) -> [Int] -> (GenericWeapon, Mods))
  -> ([Text] -> [Int])
  -> [Text]
  -> Maybe Float
  -> [ComprehensiveWeapon]
listOfBuilds theseMods notTheseMods (gw, mods) modApplicator modMapper modList multiplier
  = makeComprehensive multiplier
    .   modApplicator (gw, mods)
    <$> allPossibleIteration
 where
    -- List of mods names mapped over by modMapper from Mod Mappers
  theseModsIndices    = modMapper theseMods :: [Int]
  -- List of mods names (to be ignored) mapped over by modMapper from Mod Mappers
  notTheseModsIndices = modMapper notTheseMods :: [Int]

  -- Substracts needed and ignored mods from the list of all mods
  restOfMods =
    ([0 .. length modList - 1] -- list of all mods
                               Data.List.\\ theseModsIndices) -- list of needed mods
      Data.List.\\ notTheseModsIndices -- list of un-needed mods

  -- Generates combination of restOfMods and adds needed mods to each sublist
  restPossibleIteration = ndetake (8 - length theseModsIndices) restOfMods

  -- Final list of all possible mod combinations
  allPossibleIteration  = sort . (++ theseModsIndices) <$> restPossibleIteration
