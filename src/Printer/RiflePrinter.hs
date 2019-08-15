{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Printer.RiflePrinter
-- Maintainer  : verystable@proton.com
-- Stability   : experimental
--
-- This module exports functionality required to pretty print
-- a primary (rifle) weapon with 'ComprehensiveWeapon' type

module Printer.RiflePrinter where

import           ClassyPrelude
import           Control.Lens
import           Numeric
import           Text.PrettyPrint.Boxes
import           Types.ComprehensiveWeapon
import           Types.GenericWeapon

formatFloatN :: RealFloat a => a -> String
formatFloatN floatNum = showFFloat (Just 2) floatNum ""

showProperty
  :: RealFloat a
  => GenericWeapon
  -> Getting (Maybe a) GenericDamage (Maybe a)
  -> Box
showProperty gw property =
  text
    $  formatFloatN
    $  fromMaybe 0
    $  fromMaybe (Just 0)
    $  gw
    ^. gwDamageTypes
    &  _Just
    %~ view property

printComprehensiveRifle :: ComprehensiveWeapon -> Box
printComprehensiveRifle cw = vcat
  left
  [ hcat
    left
    [ vcat left [sGeneralInformation, emptyBox 2 2]
    , emptyBox 3 3
    , vcat left [sDamageTypes]
    , emptyBox 3 3
    , hcat left [sProcChances, emptyBox 3 3, sProcDamages]
    ]
  , sDamagesOverall
  , emptyBox 1 1
  , sModsApplied
  , emptyBox 1 1
  ]
 where
  (gw, mods)          = cw ^. build

  sGeneralInformation = vcat
    left
    [ text "General Info"
    , emptyBox 1 1
    , text "Name               "
      <+> text (unpack $ fromMaybe "n/a" $ gw ^. gwName)
    , text "Type               "
      <+> text (unpack $ fromMaybe "n/a" $ gw ^. gwType)
    , text "Category           "
      <+> text (unpack $ fromMaybe "n/a" $ gw ^. gwCategory)
    , text "Accuracy           "
      <+> text (formatFloatN $ fromMaybe 0.000 $ gw ^. gwAccuracy)
    , text "Magazine Size      "
      <+> text (formatFloatN $ fromMaybe 0.000 $ gw ^. gwMagazineSize)
    , text "Ammo               "
      <+> text (formatFloatN $ fromMaybe 0.000 $ gw ^. gwAmmo)
    , text "Reload Time        "
      <+> text (formatFloatN $ fromMaybe 0.000 $ gw ^. gwReloadTime)
    , text "Fire Rate          "
      <+> text (formatFloatN $ fromMaybe 0.000 $ gw ^. gwFireRate)
    , text "Multishot          "
      <+> text (formatFloatN $ fromMaybe 0.000 $ gw ^. gwMultishot)
    , text "Punchthrough       "
      <+> text (formatFloatN $ fromMaybe 0.000 $ gw ^. gwPunchthrough)
    , text "Critical Chance    "
      <+> text (formatFloatN $ fromMaybe 0.000 $ gw ^. gwCriticalChance)
    , text "Critical Multiplier"
      <+> text (formatFloatN $ fromMaybe 0.000 $ gw ^. gwCriticalMultiplier)
    , text "Status Chance      "
      <+> text (formatFloatN $ fromMaybe 0.000 $ gw ^. gwStatusChance)
    , text "Omega Attenuation  "
      <+> text (formatFloatN $ fromMaybe 0.000 $ gw ^. gwOmegaAttenuation)
    ]


  sModsApplied =
    vcat left $ text "Mods Applied" : emptyBox 1 1 : map (text . unpack) mods

  sDamagesOverall = hcat
    left
    [ vcat
      left
      [ text "Total Damage       "
        <+> text (formatFloatN $ fromMaybe 0.000 $ gw ^. gwTotalDamage)
      , text "Burst DPS          "
        <+> text (formatFloatN $ fromMaybe 0.000 $ cw ^. burstDPS)
      , text "Sustained DPS      "
        <+> text (formatFloatN $ fromMaybe 0.000 $ cw ^. sustainedDPS)
      , text "Base Damage        "
        <+> text (formatFloatN $ fromMaybe 0.000 $ gw ^. gwBaseDamage)
      ]
    , emptyBox 1 5
    , vcat
      left
      [ text "Adj. Total Damage       "
        <+> text (formatFloatN $ fromMaybe 0.000 $ cw ^. adjustedTotalDamage)
      , text "Adj. Burst DPS          "
        <+> text (formatFloatN $ fromMaybe 0.000 $ cw ^. adjustedBurstDPS)
      , text "Adj. Sustained DPS      "
        <+> text (formatFloatN $ fromMaybe 0.000 $ cw ^. adjustedSustainedDPS)
      ]
    ]

  sDamageTypes = vcat
    left

    [ text "Damage Types"
    , emptyBox 1 1
    , text "Impact      " <+> showProperty gw gdImpact
    , text "Puncture    " <+> showProperty gw gdPuncture
    , text "Slash       " <+> showProperty gw gdSlash
    , text "Heat        " <+> showProperty gw gdHeat
    , text "Cold        " <+> showProperty gw gdCold
    , text "Toxin       " <+> showProperty gw gdToxin
    , text "Electricity " <+> showProperty gw gdElectricity
    , text "Blast       " <+> showProperty gw gdBlast
    , text "Gas         " <+> showProperty gw gdGas
    , text "Radiation   " <+> showProperty gw gdRadiation
    , text "Viral       " <+> showProperty gw gdViral
    , text "Corrosive   " <+> showProperty gw gdCorrosive
    , text "Magnetic    " <+> showProperty gw gdMagnetic
    ]

  sProcChances = vcat
    left

    [ text "Proc Probabilities"
    , emptyBox 1 1
    , text "Impact      "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpcImpact (cw ^. procChances))
    , text "Puncture    "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpcPuncture (cw ^. procChances))
    , text "Slash       "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpcSlash (cw ^. procChances))
    , text "Heat        "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpcHeat (cw ^. procChances))
    , text "Cold        "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpcCold (cw ^. procChances))
    , text "Toxin       "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpcToxin (cw ^. procChances))
    , text "Electricity " <+> text
      (formatFloatN $ fromMaybe 0 $ _gpcElectricity (cw ^. procChances))
    , text "Blast       "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpcBlast (cw ^. procChances))
    , text "Gas         "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpcGas (cw ^. procChances))
    , text "Radiation   "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpcRadiation (cw ^. procChances))
    , text "Viral       "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpcViral (cw ^. procChances))
    , text "Corrosive   "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpcCorrosive (cw ^. procChances))
    , text "Magnetic    "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpcMagnetic (cw ^. procChances))
    ]

  sProcDamages = vcat
    left

    [ text "Proc DPSs (Average)"
    , emptyBox 1 1
    , text "Impact      "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpdImpact (cw ^. procDPSs))
    , text "Puncture    "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpdPuncture (cw ^. procDPSs))
    , text "Slash       "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpdSlash (cw ^. procDPSs))
    , text "Heat        "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpdHeat (cw ^. procDPSs))
    , text "Cold        "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpdCold (cw ^. procDPSs))
    , text "Toxin       "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpdToxin (cw ^. procDPSs))
    , text "Electricity "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpdElectricity (cw ^. procDPSs))
    , text "Blast       "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpdBlast (cw ^. procDPSs))
    , text "Gas         "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpdGas (cw ^. procDPSs))
    , text "Radiation   "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpdRadiation (cw ^. procDPSs))
    , text "Viral       "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpdViral (cw ^. procDPSs))
    , text "Corrosive   "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpdCorrosive (cw ^. procDPSs))
    , text "Magnetic    "
      <+> text (formatFloatN $ fromMaybe 0 $ _gpdMagnetic (cw ^. procDPSs))
    ]
