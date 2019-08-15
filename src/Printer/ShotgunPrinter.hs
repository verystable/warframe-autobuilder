{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Printer.ShotgunPrinter
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module exports functionality required to pretty print
-- a primary (shotgun) weapon with 'ComprehensiveWeapon' type

module Printer.ShotgunPrinter where

import           Printer.RiflePrinter
import           Text.PrettyPrint.Boxes
import           Types.ComprehensiveWeapon

printComprehensiveShotgun :: ComprehensiveWeapon -> Box
printComprehensiveShotgun = printComprehensiveRifle
