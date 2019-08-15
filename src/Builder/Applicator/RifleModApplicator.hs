{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

-- |
-- Module      : Builder.Applicator.RifleModApplicator
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- An applicator is a function that given a weapon and a list of mods,
-- applies the list on the weapon and returns a Build type.

module Builder.Applicator.RifleModApplicator where

import           ClassyPrelude
import qualified Data.Vector                   as V

import           Types.GenericWeapon

import           Builder.Mods.RifleModsMetaList

import           Builder.Applicator.PostBuildSetup

-- | Returns mod (Build -> Build) functions user chose.
getChosenMods :: GenericWeapon -> [Int] -> [Build -> Build]
getChosenMods wep choices = [ (listOfMods wep) V.! c | c <- choices ]

-- | Given an initial (GenericWeapon, []) state,
--   generates list of function from given indeces
--   and folds over the function list
--   Basically applies [Build -> Build] to the given weapon.
applicator :: Build -> [Int] -> Build
applicator target'@(wep', _) choices' =
  foldr ($) target' $ genericPostBuildSetup ++ getChosenMods wep' choices'

-- | List of relavent (Build -> Build) functions.
listOfMods :: GenericWeapon -> V.Vector (Build -> Build)
listOfMods wep = snd <$> rifleModsMetaList wep
