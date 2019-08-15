{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Printer.PistolPrinter
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module exports functionality required to pretty print
-- a secondary (pistol) weapon with 'ComprehensiveWeapon' type

module Printer.PistolPrinter where

import           Printer.RiflePrinter
import           Text.PrettyPrint.Boxes
import           Types.ComprehensiveWeapon

printComprehensivePistol :: ComprehensiveWeapon -> Box
printComprehensivePistol = printComprehensiveRifle
