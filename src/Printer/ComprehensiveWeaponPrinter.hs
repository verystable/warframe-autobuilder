{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Printer.ComprehensiveWeaponPrinter
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module exports 'printComprehensiveWeapon' function which
-- given a 'ComprehensiveWeapon' returns a 'Box' type.
-- Both wrapped in 'Either' monad.


module Printer.ComprehensiveWeaponPrinter where

import           ClassyPrelude
import           Control.Lens                   ( (^.) )
import           Printer.MeleePrinter           ( printComprehensiveMelee )
import           Printer.PistolPrinter          ( printComprehensivePistol )
import           Printer.RiflePrinter           ( printComprehensiveRifle )
import           Printer.ShotgunPrinter         ( printComprehensiveShotgun )
import           Text.PrettyPrint.Boxes         ( Box
                                                , printBox
                                                )
import           Types.ComprehensiveWeapon      ( ComprehensiveWeapon
                                                , build
                                                )
import           Types.GenericWeapon            ( gwCategory
                                                , gwType
                                                )

printComprehensiveWeapon
  :: Either String ComprehensiveWeapon -> Either String Box
printComprehensiveWeapon ewep = printComprehensiveWeapon' <$> ewep
{-# INLINE printComprehensiveWeapon #-}

printComprehensiveWeapon' :: ComprehensiveWeapon -> Box
printComprehensiveWeapon' cw
  | gw ^. gwType == Just "Rifle"     = printComprehensiveRifle cw
  | gw ^. gwType == Just "Shotgun"   = printComprehensiveShotgun cw
  | gw ^. gwType == Just "Pistol"    = printComprehensivePistol cw
  | gw ^. gwCategory == Just "Melee" = printComprehensiveMelee cw
  | otherwise                        = printComprehensiveRifle cw
  where (gw, _) = cw ^. build
