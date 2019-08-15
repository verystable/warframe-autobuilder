{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : Builder.Applicator.MeleeModApplicator
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- An applicator is a function that given a weapon and a list of mods,
-- applies the list on the weapon and returns a Build type.

module Builder.Applicator.MeleeModApplicator where

import           ClassyPrelude
import qualified Data.Vector                   as V

import           Types.GenericWeapon

import           Builder.Mods.MeleeModsMetaList

import           Builder.Applicator.PostBuildSetup

-- | Returns mod (Build -> Build) functions user chose.
getChosenMods :: GenericWeapon -> [Int] -> Maybe Float -> [Build -> Build]
getChosenMods wep choices multiplier =
  [ listOfMods wep multiplier V.! c | c <- choices ]

-- | Given an initial (GenericWeapon, []) state,
--   generates list of function from given indeces
--   and folds over the function list
--   Basically applies [Build -> Build] to the given weapon.
applicator :: Maybe Float -> Build -> [Int] -> Build
applicator multiplier' target'@(wep', _) choices' =
  foldr ($) target'
    $  genericPostBuildSetup
    ++ getChosenMods wep' choices' multiplier'

-- | List of relavent (Build -> Build) functions.
listOfMods :: GenericWeapon -> Maybe Float -> V.Vector (Build -> Build)
listOfMods wep multiplier = snd <$> meleeModsMetaList wep multiplier
