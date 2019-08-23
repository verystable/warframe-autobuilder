{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module TestComprehensiveWeapon.TestComprehensiveFunctions where

import           ComprehensiveWeapon.ComprehensiveFunctions
import           Types.ComprehensiveWeapon
import           Types.GenericWeapon
import           GenericFunctions.GenericFunctions
                                                ( readWeapon )
import           Test.Hspec

amprex = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Amprex"
                                , _gwMagazineSize         = Just 100.0
                                , _gwReloadTime           = Just 2.6
                                , _gwTotalDamage          = Just 22.0
                                , _gwBaseDamage           = Just 22.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 12.5
                                , _gwCriticalChance       = Just 0.32
                                , _gwCriticalMultiplier   = Just 2.2
                                , _gwStatusChance         = Just 0.22000003
                                , _gwFireRate             = Just 12.000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.7
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 700.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 22.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 1.0
                                }
                              , []
                              )
    , _burstDPS             = Just 264.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 201.21953
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.22000003
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 29.040007
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 24.42
    , _adjustedBurstDPS     = Just 293.04004
    , _adjustedSustainedDPS = Just 223.35368
    }
  )

arca_plasmor = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Arca Plasmor"
                                , _gwMagazineSize         = Just 10.0
                                , _gwReloadTime           = Just 2.8
                                , _gwTotalDamage          = Just 600.0
                                , _gwBaseDamage           = Just 600.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 3.0
                                , _gwAccuracy             = Just 9.090909
                                , _gwCriticalChance       = Just 0.22
                                , _gwCriticalMultiplier   = Just 1.6
                                , _gwStatusChance         = Just 0.28
                                , _gwFireRate             = Just 1.1
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.6
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 48.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 600.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 1.0
                                }
                              , []
                              )
    , _burstDPS             = Just 660.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 504.58716
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.28
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 600.0
    , _adjustedBurstDPS     = Just 660.0
    , _adjustedSustainedDPS = Just 504.58716
    }
  )

astilla = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Astilla"
                                , _gwMagazineSize         = Just 16.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 190.0
                                , _gwBaseDamage           = Just 190.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 25.0
                                , _gwCriticalChance       = Just 0.17
                                , _gwCriticalMultiplier   = Just 1.9
                                , _gwStatusChance         = Just 0.33000004
                                , _gwFireRate             = Just 4.3333335
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.1
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 112.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 25.9
                                    , _gdPuncture    = Just 15.4
                                    , _gdSlash       = Just 28.7
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 823.3334
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 534.0541
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.12210002
      , _gpcPuncture    = Just 7.260001e-2
      , _gpcSlash       = Just 0.13530001
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 272.92267
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 252.98215
    , _adjustedBurstDPS     = Just 1096.256
    , _adjustedSustainedDPS = Just 711.08496
    }
  )

listOfWeaponNames :: [[Char]]
listOfWeaponNames =
  [ "Attica"
  , "Battacor"
  , "Baza"
  , "Boar"
  , "Boar_Prime"
  , "Boltor"
  , "Boltor_Prime"
  , "Braton"
  , "Braton_Prime"
  , "Braton_Vandal"
  , "Burston"
  , "Burston_Prime"
  , "Buzlok"
  , "Cernos"
  , "Cernos_Prime"
  , "Convectrix"
  , "Corinth"
  , "Daikyu"
  , "Dera"
  , "Dera_Vandal"
  , "Dex_Sybaris"
  , "Drakgoon"
  , "Dread"
  , "Exergis"
  , "Ferrox"
  , "Flux_Rifle"
  , "Fulmin_Auto"
  , "Fulmin_Semi_Auto"
  , "Glaxion"
  , "Glaxion_Vandal"
  , "Gorgon"
  , "Gorgon_Wraith"
  , "Grakata"
  , "Grinlok"
  , "Harpak"
  , "Hek"
  , "Hema"
  , "Hind"
  , "Ignis"
  , "Ignis_Wraith"
  , "Javlok"
  , "Karak"
  , "Karak_Wraith"
  , "Kohm"
  , "Lanka"
  , "Latron"
  , "Latron_Prime"
  , "Latron_Wraith"
  , "Lenz"
  , "Miter"
  , "Mk1-Braton"
  , "Mk1-Paris"
  , "Mk1-Strun"
  , "Mutalist_Cernos"
  , "Mutalist_Quanta"
  , "Nagantaka"
  , "Ogris"
  , "Opticor"
  , "Opticor_Vandal"
  , "Panthera"
  , "Paracyst"
  , "Paris"
  , "Paris_Prime"
  , "Penta"
  , "Phage"
  , "Phantasma"
  , "Prisma_Gorgon"
  , "Prisma_Grakata"
  , "Prisma_Grinlok"
  , "Prisma_Tetra"
  , "Quanta"
  , "Quanta_Vandal"
  , "Quartakk"
  , "Rakta_Cernos"
  , "Rubico"
  , "Rubico_Prime"
  , "Sancti_Tigris"
  , "Scourge"
  , "Secura_Penta"
  , "Simulor"
  , "Snipetron"
  , "Snipetron_Vandal"
  , "Sobek"
  , "Soma"
  , "Soma_Prime"
  , "Stradavar"
  , "Stradavar_Prime"
  , "Strun"
  , "Strun_Wraith"
  , "Supra"
  , "Supra_Vandal"
  , "Sybaris"
  , "Sybaris_Prime"
  , "Synapse"
  , "Synoid_Simulor"
  , "Telos_Boltor"
  , "Tenora"
  , "Tetra"
  , "Tiberon"
  , "Tiberon_Prime"
  , "Tigris"
  , "Tigris_Prime"
  , "Tonkor"
  , "Torid"
  , "Vaykor_Hek"
  , "Vectis"
  , "Vectis_Prime"
  , "Veldt"
  , "Vulkar"
  , "Vulkar_Wraith"
  , "Zarr"
  , "Zenith"
  , "Zhuge"
  ]

iterTestCom :: Functor f => f String -> f (IO ())
iterTestCom weapons = testComprehensiveWeapon <$> weapons

testComprehensiveWeapon :: String -> IO ()
testComprehensiveWeapon weaponName = do

  parsedWeapon <- readWeapon
    ("warframe-autobuilder-data/Primary_Weapons/" ++ weaponName)

  print (makeComprehensive (Just 1) . (, []) <$> parsedWeapon)

attica = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Attica"
                                , _gwMagazineSize         = Just 20.0
                                , _gwReloadTime           = Just 2.83
                                , _gwTotalDamage          = Just 80.0
                                , _gwBaseDamage           = Just 80.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 40.0
                                , _gwCriticalChance       = Just 0.25
                                , _gwCriticalMultiplier   = Just 3.0
                                , _gwStatusChance         = Just 0.10000002
                                , _gwFireRate             = Just 3.6666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.415
                                , _gwType                 = Just "Crossbow"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 4.0
                                    , _gdPuncture    = Just 60.0
                                    , _gdSlash       = Just 16.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 293.33334
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 193.13068
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 5.000001e-3
      , _gpcPuncture    = Just 7.500001e-2
      , _gpcSlash       = Just 2.0000003e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 14.373336
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 83.92
    , _adjustedBurstDPS     = Just 307.70667
    , _adjustedSustainedDPS = Just 202.59407
    }
  )

battacor = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Battacor"
                                , _gwMagazineSize         = Just 60.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 66.0
                                , _gwBaseDamage           = Just 66.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 25.0
                                , _gwCriticalChance       = Just 0.32
                                , _gwCriticalMultiplier   = Just 2.4
                                , _gwStatusChance         = Just 0.18
                                , _gwFireRate             = Just 3.571429
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.0
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 720.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 24.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 42.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 235.71431
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 210.63832
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.1252174
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 5.478261e-2
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 66.0
    , _adjustedBurstDPS     = Just 235.71431
    , _adjustedSustainedDPS = Just 210.63832
    }
  )

baza = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Baza"
                                , _gwMagazineSize         = Just 40.0
                                , _gwReloadTime           = Just 1.4
                                , _gwTotalDamage          = Just 16.0
                                , _gwBaseDamage           = Just 16.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 80.0
                                , _gwCriticalChance       = Just 0.26
                                , _gwCriticalMultiplier   = Just 3.0
                                , _gwStatusChance         = Just 0.10000002
                                , _gwFireRate             = Just 16.666668
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.0
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 800.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 5.8
                                    , _gdPuncture    = Just 6.7
                                    , _gdSlash       = Just 3.5
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 266.6667
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 168.42107
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.6250006e-2
      , _gpcPuncture    = Just 4.1875005e-2
      , _gpcSlash       = Just 2.1875003e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 14.29167
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 16.8575
    , _adjustedBurstDPS     = Just 280.95834
    , _adjustedSustainedDPS = Just 177.44737
    }
  )

boar = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Boar"
                                , _gwMagazineSize         = Just 20.0
                                , _gwReloadTime           = Just 2.7
                                , _gwTotalDamage          = Just 176.0
                                , _gwBaseDamage           = Just 176.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 5.0
                                , _gwCriticalChance       = Just 0.1
                                , _gwCriticalMultiplier   = Just 1.5
                                , _gwStatusChance         = Just 0.2000001
                                , _gwFireRate             = Just 4.166667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.34
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 120.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 96.8
                                    , _gdPuncture    = Just 26.4
                                    , _gdSlash       = Just 52.8
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 733.3334
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 469.33334
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.11000006
      , _gpcPuncture    = Just 3.0000014e-2
      , _gpcSlash       = Just 6.000003e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 107.80006
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 201.87201
    , _adjustedBurstDPS     = Just 841.1334
    , _adjustedSustainedDPS = Just 538.3254
    }
  )

boarprime = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Boar Prime"
                                , _gwMagazineSize         = Just 20.0
                                , _gwReloadTime           = Just 2.75
                                , _gwTotalDamage          = Just 320.0
                                , _gwBaseDamage           = Just 320.0
                                , _gwBaseMultishot        = Just 8.0
                                , _gwMultishot            = Just 8.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 5.0
                                , _gwCriticalChance       = Just 0.15
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.3
                                , _gwFireRate             = Just 4.666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.34
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 120.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 208.0
                                    , _gdPuncture    = Just 48.0
                                    , _gdSlash       = Just 64.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 1493.3335
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 909.6447
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.19500001
      , _gpcPuncture    = Just 4.5e-2
      , _gpcSlash       = Just 6.0000002e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 219.52002
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 367.04
    , _adjustedBurstDPS     = Just 1712.8535
    , _adjustedSustainedDPS = Just 1043.3625
    }
  )

boltor = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Boltor"
                                , _gwMagazineSize         = Just 60.0
                                , _gwReloadTime           = Just 2.6
                                , _gwTotalDamage          = Just 25.0
                                , _gwBaseDamage           = Just 25.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 25.0
                                , _gwCriticalChance       = Just 0.1
                                , _gwCriticalMultiplier   = Just 1.8
                                , _gwStatusChance         = Just 0.13999999
                                , _gwFireRate             = Just 8.75
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.95
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 2.5
                                    , _gdPuncture    = Just 20.0
                                    , _gdSlash       = Just 2.5
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 218.75
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 158.61028
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 1.3999999e-2
      , _gpcPuncture    = Just 0.11199999
      , _gpcSlash       = Just 1.3999999e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 7.503124
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 25.8575
    , _adjustedBurstDPS     = Just 226.25313
    , _adjustedSustainedDPS = Just 164.0506
    }
  )

boltorprime = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Boltor Prime"
                                , _gwMagazineSize         = Just 60.0
                                , _gwReloadTime           = Just 2.4
                                , _gwTotalDamage          = Just 45.999996
                                , _gwBaseDamage           = Just 45.999996
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 50.0
                                , _gwCriticalChance       = Just 0.12
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.34000003
                                , _gwFireRate             = Just 10.000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.95
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 4.6
                                    , _gdPuncture    = Just 41.4
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 460.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 328.5714
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.4000006e-2
      , _gpcPuncture    = Just 0.30600005
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 45.999996
    , _adjustedBurstDPS     = Just 460.0
    , _adjustedSustainedDPS = Just 328.5714
    }
  )

braton = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Braton"
                                , _gwMagazineSize         = Just 45.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 24.0
                                , _gwBaseDamage           = Just 24.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 0.12
                                , _gwCriticalMultiplier   = Just 1.6
                                , _gwStatusChance         = Just 6.0000002e-2
                                , _gwFireRate             = Just 8.75
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.1
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 7.9
                                    , _gdPuncture    = Just 7.9
                                    , _gdSlash       = Just 8.2
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 210.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 151.2
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 1.975e-2
      , _gpcPuncture    = Just 1.975e-2
      , _gpcSlash       = Just 2.05e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 10.54725
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 25.2054
    , _adjustedBurstDPS     = Just 220.54726
    , _adjustedSustainedDPS = Just 158.79402
    }
  )

bratonprime = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Braton Prime"
                                , _gwMagazineSize         = Just 75.0
                                , _gwReloadTime           = Just 2.15
                                , _gwTotalDamage          = Just 35.0
                                , _gwBaseDamage           = Just 35.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 0.12
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.26
                                , _gwFireRate             = Just 9.583334
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.1
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 600.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 1.75
                                    , _gdPuncture    = Just 12.25
                                    , _gdSlash       = Just 21.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 335.4167
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 263.12924
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 1.2999999e-2
      , _gpcPuncture    = Just 9.1e-2
      , _gpcSlash       = Just 0.156
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 128.19626
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 48.377
    , _adjustedBurstDPS     = Just 463.61295
    , _adjustedSustainedDPS = Just 363.6972
    }
  )

bratonvandal = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Braton Vandal"
                                , _gwMagazineSize         = Just 50.0
                                , _gwReloadTime           = Just 1.75
                                , _gwTotalDamage          = Just 35.0
                                , _gwBaseDamage           = Just 35.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 33.333332
                                , _gwCriticalChance       = Just 0.16
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.15999997
                                , _gwFireRate             = Just 7.5000005
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.1
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 550.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 12.25
                                    , _gdPuncture    = Just 1.75
                                    , _gdSlash       = Just 21.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 262.50003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 207.9208
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 5.5999987e-2
      , _gpcPuncture    = Just 7.9999985e-3
      , _gpcSlash       = Just 9.5999986e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 61.739994
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 43.232
    , _adjustedBurstDPS     = Just 324.24002
    , _adjustedSustainedDPS = Just 256.82376
    }
  )

burston = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Burston"
                                , _gwMagazineSize         = Just 45.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 30.0
                                , _gwBaseDamage           = Just 30.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 25.0
                                , _gwCriticalChance       = Just 6.0e-2
                                , _gwCriticalMultiplier   = Just 1.6
                                , _gwStatusChance         = Just 0.18
                                , _gwFireRate             = Just 7.8328986
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.3
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 10.0
                                    , _gdPuncture    = Just 10.0
                                    , _gdSlash       = Just 10.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 234.98695
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 174.306
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 6.0000002e-2
      , _gpcPuncture    = Just 6.0000002e-2
      , _gpcSlash       = Just 6.0000002e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 34.543087
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 34.41
    , _adjustedBurstDPS     = Just 269.53003
    , _adjustedSustainedDPS = Just 199.92899
    }
  )

burstonprime = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Burston Prime"
                                , _gwMagazineSize         = Just 45.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 36.0
                                , _gwBaseDamage           = Just 36.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 25.0
                                , _gwCriticalChance       = Just 0.18
                                , _gwCriticalMultiplier   = Just 1.8
                                , _gwStatusChance         = Just 0.3
                                , _gwFireRate             = Just 13.636364
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.3
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 10.8
                                    , _gdPuncture    = Just 10.8
                                    , _gdSlash       = Just 14.4
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 490.90912
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 305.66037
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 9.0e-2
      , _gpcPuncture    = Just 9.0e-2
      , _gpcSlash       = Just 0.12
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 144.32727
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 46.584
    , _adjustedBurstDPS     = Just 635.2364
    , _adjustedSustainedDPS = Just 395.5245
    }
  )

buzlok = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Buzlok"
                                , _gwMagazineSize         = Just 50.0
                                , _gwReloadTime           = Just 3.0
                                , _gwTotalDamage          = Just 60.0
                                , _gwBaseDamage           = Just 60.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 13.333333
                                , _gwCriticalChance       = Just 0.23
                                , _gwCriticalMultiplier   = Just 2.5
                                , _gwStatusChance         = Just 0.20999998
                                , _gwFireRate             = Just 6.2500005
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.45
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 30.0
                                    , _gdPuncture    = Just 24.0
                                    , _gdSlash       = Just 6.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 375.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 272.72726
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.10499999
      , _gpcPuncture    = Just 8.399999e-2
      , _gpcSlash       = Just 2.0999998e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 19.293749
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 63.086998
    , _adjustedBurstDPS     = Just 394.29376
    , _adjustedSustainedDPS = Just 286.75906
    }
  )

cernos = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Cernos"
                                , _gwMagazineSize         = Just 1.0
                                , _gwReloadTime           = Just 0.6
                                , _gwTotalDamage          = Just 220.0
                                , _gwBaseDamage           = Just 220.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 16.666666
                                , _gwCriticalChance       = Just 0.2
                                , _gwCriticalMultiplier   = Just 1.5
                                , _gwStatusChance         = Just 0.19999999
                                , _gwFireRate             = Just 1.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.1
                                , _gwType                 = Just "Bow"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 99.0
                                    , _gdPuncture    = Just 5.5
                                    , _gdSlash       = Just 5.5
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 220.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 137.5
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.17999998
      , _gpcPuncture    = Just 1.0e-2
      , _gpcSlash       = Just 1.0e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 5.39
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 225.39
    , _adjustedBurstDPS     = Just 225.39
    , _adjustedSustainedDPS = Just 140.86874
    }
  )

cernosprime = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Cernos Prime"
                                , _gwMagazineSize         = Just 1.0
                                , _gwReloadTime           = Just 0.65
                                , _gwTotalDamage          = Just 360.0
                                , _gwBaseDamage           = Just 360.0
                                , _gwBaseMultishot        = Just 3.0
                                , _gwMultishot            = Just 3.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 16.666666
                                , _gwCriticalChance       = Just 0.2
                                , _gwCriticalMultiplier   = Just 1.5
                                , _gwStatusChance         = Just 0.19999993
                                , _gwFireRate             = Just 1.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.1
                                , _gwType                 = Just "Bow"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 162.0
                                    , _gdPuncture    = Just 9.0
                                    , _gdSlash       = Just 9.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 360.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 218.18182
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.17999993
      , _gpcPuncture    = Just 9.999997e-3
      , _gpcSlash       = Just 9.999997e-3
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 8.819998
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 368.82
    , _adjustedBurstDPS     = Just 368.82
    , _adjustedSustainedDPS = Just 223.52728
    }
  )

convectrix = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Convectrix"
                                , _gwMagazineSize         = Just 70.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 24.0
                                , _gwBaseDamage           = Just 24.0
                                , _gwBaseMultishot        = Just 2.0
                                , _gwMultishot            = Just 2.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 50.0
                                , _gwCriticalChance       = Just 0.16
                                , _gwCriticalMultiplier   = Just 2.4
                                , _gwStatusChance         = Just 0.3
                                , _gwFireRate             = Just 12.000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.46
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 700.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 2.4
                                    , _gdPuncture    = Just 2.4
                                    , _gdSlash       = Just 19.2
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 288.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 214.4681
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.0000001e-2
      , _gpcPuncture    = Just 3.0000001e-2
      , _gpcSlash       = Just 0.24000001
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 169.34401
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 38.112
    , _adjustedBurstDPS     = Just 457.34402
    , _adjustedSustainedDPS = Just 340.57532
    }
  )

corinth = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Corinth"
                                , _gwMagazineSize         = Just 5.0
                                , _gwReloadTime           = Just 2.3000002
                                , _gwTotalDamage          = Just 540.0
                                , _gwBaseDamage           = Just 540.0
                                , _gwBaseMultishot        = Just 6.0
                                , _gwMultishot            = Just 6.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 9.090909
                                , _gwCriticalChance       = Just 0.3
                                , _gwCriticalMultiplier   = Just 2.8
                                , _gwStatusChance         = Just 0.11999995
                                , _gwFireRate             = Just 1.1666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.95
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 132.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 151.2
                                    , _gdPuncture    = Just 226.8
                                    , _gdSlash       = Just 162.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 630.00006
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 409.9783
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.3599988e-2
      , _gpcPuncture    = Just 5.039998e-2
      , _gpcSlash       = Just 3.5999987e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 55.565983
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 587.628
    , _adjustedBurstDPS     = Just 685.56604
    , _adjustedSustainedDPS = Just 446.13837
    }
  )

daikyu = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Daikyu"
                                , _gwMagazineSize         = Just 1.0
                                , _gwReloadTime           = Just 0.6
                                , _gwTotalDamage          = Just 460.0
                                , _gwBaseDamage           = Just 460.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 16.666666
                                , _gwCriticalChance       = Just 0.25
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.14999998
                                , _gwFireRate             = Just 1.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.3
                                , _gwType                 = Just "Bow"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 138.0
                                    , _gdPuncture    = Just 184.0
                                    , _gdSlash       = Just 138.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 460.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 287.5
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 4.4999994e-2
      , _gpcPuncture    = Just 5.999999e-2
      , _gpcSlash       = Just 4.4999994e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 50.714993
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 510.715
    , _adjustedBurstDPS     = Just 510.715
    , _adjustedSustainedDPS = Just 319.19687
    }
  )

dera = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Dera"
                                , _gwMagazineSize         = Just 45.0
                                , _gwReloadTime           = Just 1.8
                                , _gwTotalDamage          = Just 30.0
                                , _gwBaseDamage           = Just 30.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 8.0e-2
                                , _gwCriticalMultiplier   = Just 1.6
                                , _gwStatusChance         = Just 0.22000003
                                , _gwFireRate             = Just 11.250001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.3
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 6.0
                                    , _gdPuncture    = Just 22.5
                                    , _gdSlash       = Just 1.5
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 337.50003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 232.75864
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 4.4000007e-2
      , _gpcPuncture    = Just 0.16500002
      , _gpcSlash       = Just 1.1000002e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 9.095627
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 30.8085
    , _adjustedBurstDPS     = Just 346.59567
    , _adjustedSustainedDPS = Just 239.0315
    }
  )

deravandal = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Dera Vandal"
                                , _gwMagazineSize         = Just 60.0
                                , _gwReloadTime           = Just 1.8
                                , _gwTotalDamage          = Just 32.0
                                , _gwBaseDamage           = Just 32.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 8.0e-2
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.3
                                , _gwFireRate             = Just 11.250001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.3
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 6.4
                                    , _gdPuncture    = Just 24.0
                                    , _gdSlash       = Just 1.6
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 360.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 269.15887
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 6.0000002e-2
      , _gpcPuncture    = Just 0.22500001
      , _gpcSlash       = Just 1.5000001e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 13.230002
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 33.176
    , _adjustedBurstDPS     = Just 373.23
    , _adjustedSustainedDPS = Just 279.05048
    }
  )

dexsybaris = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Dex Sybaris"
                                , _gwMagazineSize         = Just 14.0
                                , _gwReloadTime           = Just 1.5
                                , _gwTotalDamage          = Just 75.0
                                , _gwBaseDamage           = Just 75.0
                                , _gwBaseMultishot        = Just 2.0
                                , _gwMultishot            = Just 2.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 0.35
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.10000002
                                , _gwFireRate             = Just 4.166667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.0
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 22.5
                                    , _gdPuncture    = Just 18.75
                                    , _gdSlash       = Just 33.75
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 312.50003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 216.0494
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.0000007e-2
      , _gpcPuncture    = Just 2.5000004e-2
      , _gpcSlash       = Just 4.5000006e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 34.453133
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 83.26875
    , _adjustedBurstDPS     = Just 346.95316
    , _adjustedSustainedDPS = Just 239.86887
    }
  )

drakgoon = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Drakgoon"
                                , _gwMagazineSize         = Just 7.0
                                , _gwReloadTime           = Just 2.3
                                , _gwTotalDamage          = Just 700.0
                                , _gwBaseDamage           = Just 700.0
                                , _gwBaseMultishot        = Just 10.0
                                , _gwMultishot            = Just 10.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 1.4285715
                                , _gwCriticalChance       = Just 5.0e-2
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.21000016
                                , _gwFireRate             = Just 3.3333335
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.48
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 0.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 40.0
                                    , _gdPuncture    = Just 40.0
                                    , _gdSlash       = Just 320.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 2333.3335
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 1113.6365
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 2.1000016e-2
      , _gpcPuncture    = Just 2.1000016e-2
      , _gpcSlash       = Just 0.16800013
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 960.4008
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 988.12024
    , _adjustedBurstDPS     = Just 3293.7344
    , _adjustedSustainedDPS = Just 1572.0096
    }
  )

dread = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Dread"
                                , _gwMagazineSize         = Just 1.0
                                , _gwReloadTime           = Just 0.7
                                , _gwTotalDamage          = Just 200.0
                                , _gwBaseDamage           = Just 200.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 16.666666
                                , _gwCriticalChance       = Just 0.25
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.14999998
                                , _gwFireRate             = Just 1.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.1
                                , _gwType                 = Just "Bow"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 6.5
                                    , _gdPuncture    = Just 6.5
                                    , _gdSlash       = Just 117.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 200.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 117.64706
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 7.499999e-3
      , _gpcPuncture    = Just 7.499999e-3
      , _gpcSlash       = Just 0.13499998
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 66.14999
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 266.15
    , _adjustedBurstDPS     = Just 266.15
    , _adjustedSustainedDPS = Just 156.55882
    }
  )

exergis = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Exergis"
                                , _gwMagazineSize         = Just 1.0
                                , _gwReloadTime           = Just 1.6
                                , _gwTotalDamage          = Just 1620.0
                                , _gwBaseDamage           = Just 1620.0
                                , _gwBaseMultishot        = Just 3.0
                                , _gwMultishot            = Just 3.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 15.384615
                                , _gwCriticalChance       = Just 8.0e-2
                                , _gwCriticalMultiplier   = Just 1.4
                                , _gwStatusChance         = Just 0.36000007
                                , _gwFireRate             = Just 3.3333335
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.0
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 47.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 60.0
                                    , _gdPuncture    = Just 360.0
                                    , _gdSlash       = Just 780.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 5400.0005
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 852.6316
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 1.8000005e-2
      , _gpcPuncture    = Just 0.108000025
      , _gpcSlash       = Just 0.23400004
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 3095.8206
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 2548.746
    , _adjustedBurstDPS     = Just 8495.82
    , _adjustedSustainedDPS = Just 1341.4453
    }
  )

ferrox = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Ferrox"
                                , _gwMagazineSize         = Just 10.0
                                , _gwReloadTime           = Just 1.8
                                , _gwTotalDamage          = Just 350.0
                                , _gwBaseDamage           = Just 350.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 16.666666
                                , _gwCriticalChance       = Just 0.32
                                , _gwCriticalMultiplier   = Just 2.8
                                , _gwStatusChance         = Just 0.10000002
                                , _gwFireRate             = Just 1.3333334
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.15
                                , _gwType                 = Just "Speargun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 35.0
                                    , _gdPuncture    = Just 245.0
                                    , _gdSlash       = Just 70.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 466.6667
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 376.3441
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 1.0000002e-2
      , _gpcPuncture    = Just 7.000001e-2
      , _gpcSlash       = Just 2.0000003e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 22.866673
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 367.15
    , _adjustedBurstDPS     = Just 489.53333
    , _adjustedSustainedDPS = Just 394.78494
    }
  )

flux rifle = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Flux Rifle"
                                , _gwMagazineSize         = Just 50.0
                                , _gwReloadTime           = Just 2.25
                                , _gwTotalDamage          = Just 22.0
                                , _gwBaseDamage           = Just 22.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.1
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.24000001
                                , _gwFireRate             = Just 12.000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.55
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 50.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 4.8
                                    , _gdSlash       = Just 17.2
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 264.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 171.42857
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 5.236364e-2
      , _gpcSlash       = Just 0.18763638
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 121.363205
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 32.1136
    , _adjustedBurstDPS     = Just 385.36325
    , _adjustedSustainedDPS = Just 250.23586
    }
  )

fulmin_auto = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Fulmin"
                                , _gwMagazineSize         = Just 60.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 33.0
                                , _gwBaseDamage           = Just 33.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 111.1
                                , _gwCriticalChance       = Just 0.28
                                , _gwCriticalMultiplier   = Just 2.4
                                , _gwStatusChance         = Just 0.1
                                , _gwFireRate             = Just 9.33
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.75
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 33.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 8.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 25.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 307.88998
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 234.85126
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 5.6140352e-2
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 4.385965e-2
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 6.7519736
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 33.723682
    , _adjustedBurstDPS     = Just 314.64197
    , _adjustedSustainedDPS = Just 240.00148
    }
  )

fulmin_semi_auto = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Fulmin"
                                , _gwMagazineSize         = Just 60.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 500.0
                                , _gwBaseDamage           = Just 500.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 3.0
                                , _gwAccuracy             = Just 9.1
                                , _gwCriticalChance       = Just 0.3
                                , _gwCriticalMultiplier   = Just 2.2
                                , _gwStatusChance         = Just 0.16
                                , _gwFireRate             = Just 2.17
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.75
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 100.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 400.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 1085.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 1011.81226
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 8.0e-2
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 8.0e-2
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 43.4
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 520.0
    , _adjustedBurstDPS     = Just 1128.4
    , _adjustedSustainedDPS = Just 1052.2848
    }
  )

glaxion = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Glaxion"
                                , _gwMagazineSize         = Just 80.0
                                , _gwReloadTime           = Just 2.2
                                , _gwTotalDamage          = Just 26.0
                                , _gwBaseDamage           = Just 26.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 8.0e-2
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.34000003
                                , _gwFireRate             = Just 12.000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.35
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 720.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 26.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 312.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 234.58649
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.34000003
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 26.0
    , _adjustedBurstDPS     = Just 312.00003
    , _adjustedSustainedDPS = Just 234.58649
    }
  )

glaxionvandal = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName = Just "Glaxion Vandal"
                                , _gwMagazineSize         = Just 100.0
                                , _gwReloadTime           = Just 1.8
                                , _gwTotalDamage          = Just 29.0
                                , _gwBaseDamage           = Just 29.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.14
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.38
                                , _gwFireRate             = Just 12.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.35
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 1000.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 29.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 348.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 286.1842
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.38
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 29.0
    , _adjustedBurstDPS     = Just 348.0
    , _adjustedSustainedDPS = Just 286.1842
    }
  )

gorgon = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Gorgon"
                                , _gwMagazineSize         = Just 90.0
                                , _gwReloadTime           = Just 4.2
                                , _gwTotalDamage          = Just 25.0
                                , _gwBaseDamage           = Just 25.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 12.5
                                , _gwCriticalChance       = Just 0.17
                                , _gwCriticalMultiplier   = Just 1.5
                                , _gwStatusChance         = Just 9.000003e-2
                                , _gwFireRate             = Just 12.500001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.4
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 18.75
                                    , _gdPuncture    = Just 3.75
                                    , _gdSlash       = Just 2.5
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 312.50003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 197.36842
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 6.7500025e-2
      , _gpcPuncture    = Just 1.3500006e-2
      , _gpcSlash       = Just 9.000003e-3
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 6.8906283
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 25.55125
    , _adjustedBurstDPS     = Just 319.39066
    , _adjustedSustainedDPS = Just 201.72041
    }
  )

gorgonwraith = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Gorgon Wraith"
                                , _gwMagazineSize         = Just 90.0
                                , _gwReloadTime           = Just 3.0
                                , _gwTotalDamage          = Just 27.0
                                , _gwBaseDamage           = Just 27.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 16.666666
                                , _gwCriticalChance       = Just 0.15
                                , _gwCriticalMultiplier   = Just 1.9
                                , _gwStatusChance         = Just 0.20999998
                                , _gwFireRate             = Just 13.333334
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.4
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 900.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 23.0
                                    , _gdPuncture    = Just 2.7
                                    , _gdSlash       = Just 1.3
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 360.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 249.23077
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.17888887
      , _gpcPuncture    = Just 2.0999998e-2
      , _gpcSlash       = Just 1.011111e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 8.917999
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 27.66885
    , _adjustedBurstDPS     = Just 368.91803
    , _adjustedSustainedDPS = Just 255.40477
    }
  )

grakata = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Grakata"
                                , _gwMagazineSize         = Just 60.0
                                , _gwReloadTime           = Just 2.4
                                , _gwTotalDamage          = Just 11.0
                                , _gwBaseDamage           = Just 11.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 0.25
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.19999999
                                , _gwFireRate             = Just 20.000002
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.2
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 750.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 4.4
                                    , _gdPuncture    = Just 3.7
                                    , _gdSlash       = Just 2.9
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 220.00002
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 122.22223
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 8.0e-2
      , _gpcPuncture    = Just 6.727272e-2
      , _gpcSlash       = Just 5.2727275e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 28.420002
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 12.421
    , _adjustedBurstDPS     = Just 248.42001
    , _adjustedSustainedDPS = Just 138.01111
    }
  )

grinlok = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Grinlok"
                                , _gwMagazineSize         = Just 9.0
                                , _gwReloadTime           = Just 1.7
                                , _gwTotalDamage          = Just 187.0
                                , _gwBaseDamage           = Just 187.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 44.444443
                                , _gwCriticalChance       = Just 0.15
                                , _gwCriticalMultiplier   = Just 2.5
                                , _gwStatusChance         = Just 0.35000002
                                , _gwFireRate             = Just 1.6666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.245
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 93.5
                                    , _gdPuncture    = Just 18.7
                                    , _gdSlash       = Just 74.8
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 311.6667
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 237.04227
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.17500001
      , _gpcPuncture    = Just 3.5000004e-2
      , _gpcSlash       = Just 0.14000002
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 106.90167
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 251.14099
    , _adjustedBurstDPS     = Just 418.56833
    , _adjustedSustainedDPS = Just 318.34778
    }
  )

harpak = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Harpak"
                                , _gwMagazineSize         = Just 45.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 50.0
                                , _gwBaseDamage           = Just 50.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 18.181818
                                , _gwCriticalChance       = Just 0.2
                                , _gwCriticalMultiplier   = Just 2.3
                                , _gwStatusChance         = Just 0.17000002
                                , _gwFireRate             = Just 6.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.55
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 5.0
                                    , _gdPuncture    = Just 37.5
                                    , _gdSlash       = Just 7.5
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 300.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 236.8421
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 1.7000003e-2
      , _gpcPuncture    = Just 0.12750001
      , _gpcSlash       = Just 2.5500003e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 18.742502
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 53.12375
    , _adjustedBurstDPS     = Just 318.7425
    , _adjustedSustainedDPS = Just 251.6388
    }
  )

hek = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Hek"
                                , _gwMagazineSize         = Just 4.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 525.0
                                , _gwBaseDamage           = Just 525.0
                                , _gwBaseMultishot        = Just 7.0
                                , _gwMultishot            = Just 7.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 9.090909
                                , _gwCriticalChance       = Just 0.1
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.24999988
                                , _gwFireRate             = Just 2.1666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.85
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 120.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 78.75
                                    , _gdPuncture    = Just 341.25
                                    , _gdSlash       = Just 105.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 2.0
                                }
                              , []
                              )
    , _burstDPS             = Just 1137.5
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 546.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.7499983e-2
      , _gpcPuncture    = Just 0.16249992
      , _gpcSlash       = Just 4.999998e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 139.34369
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 589.3125
    , _adjustedBurstDPS     = Just 1276.8438
    , _adjustedSustainedDPS = Just 612.885
    }
  )

hema = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Hema"
                                , _gwMagazineSize         = Just 60.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 47.0
                                , _gwBaseDamage           = Just 47.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 20.0
                                , _gwCriticalChance       = Just 0.11
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.25
                                , _gwFireRate             = Just 6.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.2
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 60.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 47.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 282.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 235.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.25
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 47.0
    , _adjustedBurstDPS     = Just 282.0
    , _adjustedSustainedDPS = Just 235.0
    }
  )

hind = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Hind"
                                , _gwMagazineSize         = Just 65.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 30.0
                                , _gwBaseDamage           = Just 30.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 33.333332
                                , _gwCriticalChance       = Just 7.0e-2
                                , _gwCriticalMultiplier   = Just 1.5
                                , _gwStatusChance         = Just 0.14999998
                                , _gwFireRate             = Just 6.2500005
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.42
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 7.5
                                    , _gdPuncture    = Just 7.5
                                    , _gdSlash       = Just 15.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 187.50002
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 157.25807
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.7499994e-2
      , _gpcPuncture    = Just 3.7499994e-2
      , _gpcSlash       = Just 7.499999e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 34.453125
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 35.5125
    , _adjustedBurstDPS     = Just 221.95314
    , _adjustedSustainedDPS = Just 186.15424
    }
  )

ignis = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Ignis"
                                , _gwMagazineSize         = Just 150.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 0.0
                                , _gwBaseDamage           = Just 0.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.11
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.26999998
                                , _gwFireRate             = Just 8.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.6
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 750.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 33.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 1.0
                                }
                              , []
                              )
    , _burstDPS             = Just 0.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 0.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.26999998
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 0.0
    , _adjustedBurstDPS     = Just 0.0
    , _adjustedSustainedDPS = Just 0.0
    }
  )

igniswraith = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Ignis Wraith"
                                , _gwMagazineSize         = Just 200.0
                                , _gwReloadTime           = Just 1.7
                                , _gwTotalDamage          = Just 0.0
                                , _gwBaseDamage           = Just 0.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.17
                                , _gwCriticalMultiplier   = Just 2.5
                                , _gwStatusChance         = Just 0.28999996
                                , _gwFireRate             = Just 8.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.6
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 800.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 35.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 1.0
                                }
                              , []
                              )
    , _burstDPS             = Just 0.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 0.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.28999996
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 0.0
    , _adjustedBurstDPS     = Just 0.0
    , _adjustedSustainedDPS = Just 0.0
    }
  )

javlok = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Javlok"
                                , _gwMagazineSize         = Just 6.0
                                , _gwReloadTime           = Just 1.9
                                , _gwTotalDamage          = Just 1.0
                                , _gwBaseDamage           = Just 1.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.2
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.25
                                , _gwFireRate             = Just 3.3333335
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.2
                                , _gwType                 = Just "Speargun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 300.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 230.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 3.3333335
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 1.6216217
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.25
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 2.9166667
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 1.875
    , _adjustedBurstDPS     = Just 6.2500005
    , _adjustedSustainedDPS = Just 3.0405407
    }
  )

karak = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Karak"
                                , _gwMagazineSize         = Just 30.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 29.0
                                , _gwBaseDamage           = Just 29.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 9.0e-2
                                , _gwCriticalMultiplier   = Just 1.5
                                , _gwStatusChance         = Just 0.14999998
                                , _gwFireRate             = Just 11.666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.28
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 13.0
                                    , _gdPuncture    = Just 8.7
                                    , _gdSlash       = Just 7.3
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 338.33334
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 190.31252
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 6.724137e-2
      , _gpcPuncture    = Just 4.499999e-2
      , _gpcSlash       = Just 3.775862e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 31.298748
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 31.68275
    , _adjustedBurstDPS     = Just 369.6321
    , _adjustedSustainedDPS = Just 207.91808
    }
  )

karakwraith = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Karak Wraith"
                                , _gwMagazineSize         = Just 60.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 31.0
                                , _gwBaseDamage           = Just 31.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 0.13
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.25
                                , _gwFireRate             = Just 11.666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.28
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 14.1
                                    , _gdPuncture    = Just 9.3
                                    , _gdSlash       = Just 7.8
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 361.6667
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 260.4
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.11298077
      , _gpcPuncture    = Just 7.451923e-2
      , _gpcSlash       = Just 6.25e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 55.380207
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 35.746876
    , _adjustedBurstDPS     = Just 417.0469
    , _adjustedSustainedDPS = Just 300.27374
    }
  )

kohm = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Kohm"
                                , _gwMagazineSize         = Just 245.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 30.0
                                , _gwBaseDamage           = Just 30.0
                                , _gwBaseMultishot        = Just 12.0
                                , _gwMultishot            = Just 12.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 8.0
                                , _gwCriticalChance       = Just 0.11
                                , _gwCriticalMultiplier   = Just 2.3
                                , _gwStatusChance         = Just 0.25
                                , _gwFireRate             = Just 3.6666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.4
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 960.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 6.0
                                    , _gdPuncture    = Just 6.0
                                    , _gdSlash       = Just 18.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 110.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 106.80317
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 5.0e-2
      , _gpcPuncture    = Just 5.0e-2
      , _gpcSlash       = Just 0.15
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 40.425003
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 41.025
    , _adjustedBurstDPS     = Just 150.425
    , _adjustedSustainedDPS = Just 146.05333
    }
  )

lanka = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Lanka"
                                , _gwMagazineSize         = Just 10.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 525.0
                                , _gwBaseDamage           = Just 525.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.2
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.25
                                , _gwFireRate             = Just 0.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.85
                                , _gwType                 = Just "Sniper Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 200.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 2.0
                                }
                              , []
                              )
    , _burstDPS             = Just 0.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 0.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.25
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 590.625
    , _adjustedBurstDPS     = Just 0.0
    , _adjustedSustainedDPS = Just 0.0
    }
  )

latron = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Latron"
                                , _gwMagazineSize         = Just 15.0
                                , _gwReloadTime           = Just 2.4
                                , _gwTotalDamage          = Just 55.0
                                , _gwBaseDamage           = Just 55.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 0.12
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.12
                                , _gwFireRate             = Just 4.166667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.25
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 8.25
                                    , _gdPuncture    = Just 38.5
                                    , _gdSlash       = Just 8.25
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 229.16669
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 137.5
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 1.8000001e-2
      , _gpcPuncture    = Just 8.4e-2
      , _gpcSlash       = Just 1.8000001e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 10.106252
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 57.4255
    , _adjustedBurstDPS     = Just 239.27293
    , _adjustedSustainedDPS = Just 143.56375
    }
  )

latronprime = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Latron Prime"
                                , _gwMagazineSize         = Just 15.0
                                , _gwReloadTime           = Just 2.4
                                , _gwTotalDamage          = Just 90.0
                                , _gwBaseDamage           = Just 90.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 0.22
                                , _gwCriticalMultiplier   = Just 2.8
                                , _gwStatusChance         = Just 0.26
                                , _gwFireRate             = Just 4.166667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.25
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 9.0
                                    , _gdPuncture    = Just 72.0
                                    , _gdSlash       = Just 9.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 375.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 225.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 2.5999999e-2
      , _gpcPuncture    = Just 0.20799999
      , _gpcSlash       = Just 2.5999999e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 23.8875
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 95.733
    , _adjustedBurstDPS     = Just 398.88754
    , _adjustedSustainedDPS = Just 239.3325
    }
  )

latronwraith = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Latron Wraith"
                                , _gwMagazineSize         = Just 15.0
                                , _gwReloadTime           = Just 2.4
                                , _gwTotalDamage          = Just 60.0
                                , _gwBaseDamage           = Just 60.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 0.26
                                , _gwCriticalMultiplier   = Just 2.8
                                , _gwStatusChance         = Just 0.13999999
                                , _gwFireRate             = Just 5.416667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.25
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 15.0
                                    , _gdPuncture    = Just 42.0
                                    , _gdSlash       = Just 3.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 325.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 174.10715
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.4999996e-2
      , _gpcPuncture    = Just 9.799999e-2
      , _gpcSlash       = Just 6.9999993e-3
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 5.57375
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 61.029
    , _adjustedBurstDPS     = Just 330.57376
    , _adjustedSustainedDPS = Just 177.0931
    }
  )

lenz = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Lenz"
                                , _gwMagazineSize         = Just 1.0
                                , _gwReloadTime           = Just 0.6
                                , _gwTotalDamage          = Just 60.0
                                , _gwBaseDamage           = Just 60.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 16.666666
                                , _gwCriticalChance       = Just 0.5
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 5.0000012e-2
                                , _gwFireRate             = Just 1.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.9
                                , _gwType                 = Just "Bow"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 6.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 50.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 60.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 37.5
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 5.0000012e-2
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 60.0
    , _adjustedBurstDPS     = Just 60.0
    , _adjustedSustainedDPS = Just 37.5
    }
  )

miter = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Miter"
                                , _gwMagazineSize         = Just 20.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 250.0
                                , _gwBaseDamage           = Just 250.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 5.0e-2
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.19999999
                                , _gwFireRate             = Just 2.5000002
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.55
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 5.0
                                    , _gdPuncture    = Just 5.0
                                    , _gdSlash       = Just 90.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 625.00006
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 500.00006
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 1.0e-2
      , _gpcPuncture    = Just 1.0e-2
      , _gpcSlash       = Just 0.17999998
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 275.625
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 360.25
    , _adjustedBurstDPS     = Just 900.62506
    , _adjustedSustainedDPS = Just 720.50006
    }
  )

mutalistcernos = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName = Just "Mutalist Cernos"
                                , _gwMagazineSize         = Just 1.0
                                , _gwReloadTime           = Just 0.6
                                , _gwTotalDamage          = Just 280.0
                                , _gwBaseDamage           = Just 280.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 16.666666
                                , _gwCriticalChance       = Just 0.1
                                , _gwCriticalMultiplier   = Just 1.5
                                , _gwStatusChance         = Just 0.3
                                , _gwFireRate             = Just 1.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.35
                                , _gwType                 = Just "Bow"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 108.0
                                    , _gdPuncture    = Just 6.0
                                    , _gdSlash       = Just 6.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 280.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 175.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.27
      , _gpcPuncture    = Just 1.5000001e-2
      , _gpcSlash       = Just 1.5000001e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 10.29
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 290.29
    , _adjustedBurstDPS     = Just 290.29
    , _adjustedSustainedDPS = Just 181.43126
    }
  )

mutalist quanta = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName = Just "Mutalist Quanta"
                                , _gwMagazineSize         = Just 60.0
                                , _gwReloadTime           = Just 3.0
                                , _gwTotalDamage          = Just 25.0
                                , _gwBaseDamage           = Just 25.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 2.5e-2
                                , _gwCriticalMultiplier   = Just 1.5
                                , _gwStatusChance         = Just 0.14999998
                                , _gwFireRate             = Just 10.000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.55
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 2.5
                                    , _gdPuncture    = Just 15.0
                                    , _gdSlash       = Just 7.5
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 250.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 166.66667
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 1.4999998e-2
      , _gpcPuncture    = Just 8.999999e-2
      , _gpcSlash       = Just 4.4999994e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 27.5625
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 27.75625
    , _adjustedBurstDPS     = Just 277.56253
    , _adjustedSustainedDPS = Just 185.04167
    }
  )

nagantaka = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Nagantaka"
                                , _gwMagazineSize         = Just 9.0
                                , _gwReloadTime           = Just 2.3
                                , _gwTotalDamage          = Just 158.99998
                                , _gwBaseDamage           = Just 158.99998
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 40.0
                                , _gwCriticalChance       = Just 0.15
                                , _gwCriticalMultiplier   = Just 2.3
                                , _gwStatusChance         = Just 0.39
                                , _gwFireRate             = Just 2.5000002
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.05
                                , _gwType                 = Just "Crossbow"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 1.6
                                    , _gdPuncture    = Just 14.3
                                    , _gdSlash       = Just 143.1
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 397.5
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 242.54237
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.924528e-3
      , _gpcPuncture    = Just 3.507547e-2
      , _gpcSlash       = Just 0.351
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 341.83014
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 295.73203
    , _adjustedBurstDPS     = Just 739.33014
    , _adjustedSustainedDPS = Just 451.11667
    }
  )

ogris = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Ogris"
                                , _gwMagazineSize         = Just 5.0
                                , _gwReloadTime           = Just 2.5
                                , _gwTotalDamage          = Just 0.0
                                , _gwBaseDamage           = Just 0.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 5.0e-2
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.35000002
                                , _gwFireRate             = Just 1.5000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.3
                                , _gwType                 = Just "Launcher"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 20.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 100.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 0.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 0.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.35000002
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 0.0
    , _adjustedBurstDPS     = Just 0.0
    , _adjustedSustainedDPS = Just 0.0
    }
  )

opticor = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Opticor"
                                , _gwMagazineSize         = Just 5.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 0.0
                                , _gwBaseDamage           = Just 0.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.2
                                , _gwCriticalMultiplier   = Just 2.5
                                , _gwStatusChance         = Just 0.19999999
                                , _gwFireRate             = Just 1.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.15
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 200.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 100.0
                                    , _gdPuncture    = Just 850.0
                                    , _gdSlash       = Just 50.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 0.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 0.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 2.0e-2
      , _gpcPuncture    = Just 0.17
      , _gpcSlash       = Just 1.0e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 0.0
    , _adjustedBurstDPS     = Just 0.0
    , _adjustedSustainedDPS = Just 0.0
    }
  )

opticorvandal = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName = Just "Opticor Vandal"
                                , _gwMagazineSize         = Just 8.0
                                , _gwReloadTime           = Just 1.4
                                , _gwTotalDamage          = Just 0.0
                                , _gwBaseDamage           = Just 0.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.24
                                , _gwCriticalMultiplier   = Just 2.6
                                , _gwStatusChance         = Just 0.3
                                , _gwFireRate             = Just 2.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.15
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 200.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 40.0
                                    , _gdPuncture    = Just 280.0
                                    , _gdSlash       = Just 80.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 0.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 0.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.0000001e-2
      , _gpcPuncture    = Just 0.21000001
      , _gpcSlash       = Just 6.0000002e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 0.0
    , _adjustedBurstDPS     = Just 0.0
    , _adjustedSustainedDPS = Just 0.0
    }
  )

panthera = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Panthera"
                                , _gwMagazineSize         = Just 30.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 100.0
                                , _gwBaseDamage           = Just 100.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.12
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.24000001
                                , _gwFireRate             = Just 3.0000002
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.4
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 0.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 20.0
                                    , _gdPuncture    = Just 10.0
                                    , _gdSlash       = Just 70.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 300.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 250.00002
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 4.8000004e-2
      , _gpcPuncture    = Just 2.4000002e-2
      , _gpcSlash       = Just 0.168
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 123.48001
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 141.16
    , _adjustedBurstDPS     = Just 423.48004
    , _adjustedSustainedDPS = Just 352.90005
    }
  )

paracyst = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Paracyst"
                                , _gwMagazineSize         = Just 60.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 33.0
                                , _gwBaseDamage           = Just 33.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 50.0
                                , _gwCriticalChance       = Just 0.1
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.3
                                , _gwFireRate             = Just 11.111112
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.315
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 0.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 33.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 366.6667
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 267.5676
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.3
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 495.00006
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 77.55
    , _adjustedBurstDPS     = Just 861.66675
    , _adjustedSustainedDPS = Just 628.7838
    }
  )

paris = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Paris"
                                , _gwMagazineSize         = Just 1.0
                                , _gwReloadTime           = Just 0.65
                                , _gwTotalDamage          = Just 180.0
                                , _gwBaseDamage           = Just 180.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 16.666666
                                , _gwCriticalChance       = Just 0.2
                                , _gwCriticalMultiplier   = Just 1.5
                                , _gwStatusChance         = Just 0.10000002
                                , _gwFireRate             = Just 1.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.2
                                , _gwType                 = Just "Bow"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 6.0
                                    , _gdPuncture    = Just 96.0
                                    , _gdSlash       = Just 18.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 180.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 109.09091
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 5.000001e-3
      , _gpcPuncture    = Just 8.000001e-2
      , _gpcSlash       = Just 1.5000003e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 6.6150017
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 186.615
    , _adjustedBurstDPS     = Just 186.615
    , _adjustedSustainedDPS = Just 113.100006
    }
  )

parisprime = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Paris Prime"
                                , _gwMagazineSize         = Just 1.0
                                , _gwReloadTime           = Just 0.7
                                , _gwTotalDamage          = Just 260.0
                                , _gwBaseDamage           = Just 260.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 16.666666
                                , _gwCriticalChance       = Just 0.25
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.14999998
                                , _gwFireRate             = Just 1.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.2
                                , _gwType                 = Just "Bow"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 3.25
                                    , _gdPuncture    = Just 104.0
                                    , _gdSlash       = Just 22.75
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 260.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 152.94118
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.7499995e-3
      , _gpcPuncture    = Just 0.11999998
      , _gpcSlash       = Just 2.6249995e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 16.721247
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 276.72125
    , _adjustedBurstDPS     = Just 276.72125
    , _adjustedSustainedDPS = Just 162.7772
    }
  )

penta = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Penta"
                                , _gwMagazineSize         = Just 5.0
                                , _gwReloadTime           = Just 2.5
                                , _gwTotalDamage          = Just 0.0
                                , _gwBaseDamage           = Just 0.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.1
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.10000002
                                , _gwFireRate             = Just 1.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.3
                                , _gwType                 = Just "Launcher"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 20.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 75.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 350.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 0.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 0.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 4.6153855e-2
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 5.3846166e-2
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 0.0
    , _adjustedBurstDPS     = Just 0.0
    , _adjustedSustainedDPS = Just 0.0
    }
  )

phage = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Phage"
                                , _gwMagazineSize         = Just 90.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 35.0
                                , _gwBaseDamage           = Just 35.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 50.0
                                , _gwCriticalChance       = Just 0.19
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.35137963
                                , _gwFireRate             = Just 12.000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.46
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 720.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 35.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 420.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 331.57895
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.35137963
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 35.0
    , _adjustedBurstDPS     = Just 420.00003
    , _adjustedSustainedDPS = Just 331.57895
    }
  )

phantasma = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Phantasma"
                                , _gwMagazineSize         = Just 11.0
                                , _gwReloadTime           = Just 0.5
                                , _gwTotalDamage          = Just 75.0
                                , _gwBaseDamage           = Just 75.0
                                , _gwBaseMultishot        = Just 5.0
                                , _gwMultishot            = Just 5.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 3.0e-2
                                , _gwCriticalMultiplier   = Just 1.5
                                , _gwStatusChance         = Just 0.36999995
                                , _gwFireRate             = Just 12.000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.0
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 275.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 25.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 900.00006
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 582.353
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.36999995
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 75.0
    , _adjustedBurstDPS     = Just 900.00006
    , _adjustedSustainedDPS = Just 582.353
    }
  )

prisma gorgon = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Prisma Gorgon"
                                , _gwMagazineSize         = Just 120.0
                                , _gwReloadTime           = Just 3.0
                                , _gwTotalDamage          = Just 23.000002
                                , _gwBaseDamage           = Just 23.000002
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 20.0
                                , _gwCriticalChance       = Just 0.3
                                , _gwCriticalMultiplier   = Just 2.3
                                , _gwStatusChance         = Just 0.14999998
                                , _gwFireRate             = Just 14.166667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.4
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 840.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 17.3
                                    , _gdPuncture    = Just 3.5
                                    , _gdSlash       = Just 2.3
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 325.83337
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 240.61542
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.11233765
      , _gpcPuncture    = Just 2.272727e-2
      , _gpcSlash       = Just 1.4935062e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 11.922537
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 23.841593
    , _adjustedBurstDPS     = Just 337.75592
    , _adjustedSustainedDPS = Just 249.41975
    }
  )

prismagrakata = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName = Just "Prisma Grakata"
                                , _gwMagazineSize         = Just 120.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 15.0
                                , _gwBaseDamage           = Just 15.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 0.25
                                , _gwCriticalMultiplier   = Just 2.5
                                , _gwStatusChance         = Just 0.20999998
                                , _gwFireRate             = Just 21.666668
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.2
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 1000.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 6.0
                                    , _gdPuncture    = Just 5.0
                                    , _gdSlash       = Just 4.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 325.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 238.77553
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 8.399999e-2
      , _gpcPuncture    = Just 6.999999e-2
      , _gpcSlash       = Just 5.5999998e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 44.59
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 17.058
    , _adjustedBurstDPS     = Just 369.59003
    , _adjustedSustainedDPS = Just 271.53552
    }
  )

prismagrinlok = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName = Just "Prisma Grinlok"
                                , _gwMagazineSize         = Just 21.0
                                , _gwReloadTime           = Just 1.7
                                , _gwTotalDamage          = Just 187.0
                                , _gwBaseDamage           = Just 187.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 32.0
                                , _gwCriticalChance       = Just 0.21
                                , _gwCriticalMultiplier   = Just 2.9
                                , _gwStatusChance         = Just 0.37
                                , _gwFireRate             = Just 1.6666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.245
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 74.8
                                    , _gdPuncture    = Just 18.7
                                    , _gdSlash       = Just 93.5
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 311.6667
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 274.6154
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.148
      , _gpcPuncture    = Just 3.7e-2
      , _gpcSlash       = Just 0.185
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 141.26291
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 271.75775
    , _adjustedBurstDPS     = Just 452.9296
    , _adjustedSustainedDPS = Just 399.0848
    }
  )

prismatetra = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Prisma Tetra"
                                , _gwMagazineSize         = Just 60.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 38.0
                                , _gwBaseDamage           = Just 38.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 18.181818
                                , _gwCriticalChance       = Just 0.1
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.24000001
                                , _gwFireRate             = Just 7.0833335
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.5
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 7.6
                                    , _gdPuncture    = Just 30.4
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 269.1667
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 217.75282
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 4.8000004e-2
      , _gpcPuncture    = Just 0.19200002
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 38.0
    , _adjustedBurstDPS     = Just 269.1667
    , _adjustedSustainedDPS = Just 217.75282
    }
  )

quanta = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Quanta"
                                , _gwMagazineSize         = Just 60.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 20.0
                                , _gwBaseDamage           = Just 20.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.16
                                , _gwCriticalMultiplier   = Just 2.2
                                , _gwStatusChance         = Just 0.15999997
                                , _gwFireRate             = Just 12.000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.1
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 20.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 240.00002
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 171.42859
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.15999997
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 19.199997
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 21.6
    , _adjustedBurstDPS     = Just 259.2
    , _adjustedSustainedDPS = Just 185.14287
    }
  )

quantavandal = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Quanta Vandal"
                                , _gwMagazineSize         = Just 80.0
                                , _gwReloadTime           = Just 1.8
                                , _gwTotalDamage          = Just 26.0
                                , _gwBaseDamage           = Just 26.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.22
                                , _gwCriticalMultiplier   = Just 2.4
                                , _gwStatusChance         = Just 0.3
                                , _gwFireRate             = Just 12.000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.1
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 560.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 26.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 312.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 245.66931
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.3
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 46.800003
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 29.9
    , _adjustedBurstDPS     = Just 358.80002
    , _adjustedSustainedDPS = Just 282.5197
    }
  )

quartakk = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Quartakk"
                                , _gwMagazineSize         = Just 84.0
                                , _gwReloadTime           = Just 1.9
                                , _gwTotalDamage          = Just 49.0
                                , _gwBaseDamage           = Just 49.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 90.90909
                                , _gwCriticalChance       = Just 0.19
                                , _gwCriticalMultiplier   = Just 2.3
                                , _gwStatusChance         = Just 0.26999998
                                , _gwFireRate             = Just 6.3333335
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.25
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 840.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 18.1
                                    , _gdPuncture    = Just 14.2
                                    , _gdSlash       = Just 16.7
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 310.33334
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 271.44742
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 9.9734694e-2
      , _gpcPuncture    = Just 7.824489e-2
      , _gpcSlash       = Just 9.202041e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 69.964645
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 60.04705
    , _adjustedBurstDPS     = Just 380.298
    , _adjustedSustainedDPS = Just 332.64523
    }
  )

raktacernos = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Rakta Cernos"
                                , _gwMagazineSize         = Just 1.0
                                , _gwReloadTime           = Just 0.6
                                , _gwTotalDamage          = Just 250.0
                                , _gwBaseDamage           = Just 250.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 16.666666
                                , _gwCriticalChance       = Just 0.2
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.19999999
                                , _gwFireRate             = Just 1.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.1
                                , _gwType                 = Just "Bow"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 135.0
                                    , _gdPuncture    = Just 7.5
                                    , _gdSlash       = Just 7.5
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 250.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 156.25
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.17999998
      , _gpcPuncture    = Just 1.0e-2
      , _gpcSlash       = Just 1.0e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 6.125
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 256.125
    , _adjustedBurstDPS     = Just 256.125
    , _adjustedSustainedDPS = Just 160.07813
    }
  )

rubico = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Rubico"
                                , _gwMagazineSize         = Just 5.0
                                , _gwReloadTime           = Just 2.4
                                , _gwTotalDamage          = Just 179.99998
                                , _gwBaseDamage           = Just 179.99998
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 13.333333
                                , _gwCriticalChance       = Just 0.3
                                , _gwCriticalMultiplier   = Just 3.0
                                , _gwStatusChance         = Just 0.12
                                , _gwFireRate             = Just 2.6666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.8
                                , _gwType                 = Just "Sniper Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 144.0
                                    , _gdPuncture    = Just 27.0
                                    , _gdSlash       = Just 9.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 2.0
                                }
                              , []
                              )
    , _burstDPS             = Just 479.99997
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 210.52629
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 9.6e-2
      , _gpcPuncture    = Just 1.8000001e-2
      , _gpcSlash       = Just 6.0e-3
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 7.0559993
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 182.64598
    , _adjustedBurstDPS     = Just 487.05597
    , _adjustedSustainedDPS = Just 213.62103
    }
  )

rubicoprime = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Rubico Prime"
                                , _gwMagazineSize         = Just 5.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 187.0
                                , _gwBaseDamage           = Just 187.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 13.333333
                                , _gwCriticalChance       = Just 0.38
                                , _gwCriticalMultiplier   = Just 3.0
                                , _gwStatusChance         = Just 0.15999997
                                , _gwFireRate             = Just 3.6666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.8
                                , _gwType                 = Just "Sniper Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 149.6
                                    , _gdPuncture    = Just 28.1
                                    , _gdSlash       = Just 9.3
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 2.0
                                }
                              , []
                              )
    , _burstDPS             = Just 685.6667
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 277.97296
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.12799996
      , _gpcPuncture    = Just 2.4042772e-2
      , _gpcSlash       = Just 7.957217e-3
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 13.367196
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 190.6456
    , _adjustedBurstDPS     = Just 699.0339
    , _adjustedSustainedDPS = Just 283.39212
    }
  )

sancti tigris = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Sancti Tigris"
                                , _gwMagazineSize         = Just 2.0
                                , _gwReloadTime           = Just 1.5
                                , _gwTotalDamage          = Just 1260.0
                                , _gwBaseDamage           = Just 1260.0
                                , _gwBaseMultishot        = Just 6.0
                                , _gwMultishot            = Just 6.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 6.451613
                                , _gwCriticalChance       = Just 0.15
                                , _gwCriticalMultiplier   = Just 1.5
                                , _gwStatusChance         = Just 0.2800001
                                , _gwFireRate             = Just 2.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.5
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 0.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 126.0
                                    , _gdPuncture    = Just 126.0
                                    , _gdSlash       = Just 1008.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 1.0
                                }
                              , []
                              )
    , _burstDPS             = Just 2520.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 1008.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 2.800001e-2
      , _gpcPuncture    = Just 2.800001e-2
      , _gpcSlash       = Just 0.22400008
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 1382.9766
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 1951.4883
    , _adjustedBurstDPS     = Just 3902.9766
    , _adjustedSustainedDPS = Just 1561.1907
    }
  )

scourge = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Scourge"
                                , _gwMagazineSize         = Just 20.0
                                , _gwReloadTime           = Just 2.5
                                , _gwTotalDamage          = Just 27.0
                                , _gwBaseDamage           = Just 27.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 2.0e-2
                                , _gwCriticalMultiplier   = Just 1.5
                                , _gwStatusChance         = Just 0.3
                                , _gwFireRate             = Just 2.6666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.15
                                , _gwType                 = Just "Speargun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 100.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 70.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 72.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 54.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.3
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 27.0
    , _adjustedBurstDPS     = Just 72.0
    , _adjustedSustainedDPS = Just 54.0
    }
  )

secura penta = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Secura Penta"
                                , _gwMagazineSize         = Just 7.0
                                , _gwReloadTime           = Just 2.5
                                , _gwTotalDamage          = Just 48.0
                                , _gwBaseDamage           = Just 48.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.26
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.26
                                , _gwFireRate             = Just 2.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.3
                                , _gwType                 = Just "Launcher"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 28.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 75.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 300.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 96.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 56.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.13
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.13
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 48.0
    , _adjustedBurstDPS     = Just 96.0
    , _adjustedSustainedDPS = Just 56.0
    }
  )

simulor = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Simulor"
                                , _gwMagazineSize         = Just 8.0
                                , _gwReloadTime           = Just 3.0
                                , _gwTotalDamage          = Just 75.0
                                , _gwBaseDamage           = Just 75.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 0.12
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.3
                                , _gwFireRate             = Just 3.0000002
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.85
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 50.0
                                    }
                                  )
                                , _gwDisposition          = Just 2.0
                                }
                              , []
                              )
    , _burstDPS             = Just 225.00002
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 105.882355
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.3
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 75.0
    , _adjustedBurstDPS     = Just 225.00002
    , _adjustedSustainedDPS = Just 105.882355
    }
  )

snipetron = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Snipetron"
                                , _gwMagazineSize         = Just 4.0
                                , _gwReloadTime           = Just 3.5
                                , _gwTotalDamage          = Just 180.0
                                , _gwBaseDamage           = Just 180.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 13.333333
                                , _gwCriticalChance       = Just 0.3
                                , _gwCriticalMultiplier   = Just 1.5
                                , _gwStatusChance         = Just 0.12
                                , _gwFireRate             = Just 2.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.25
                                , _gwType                 = Just "Sniper Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 18.0
                                    , _gdPuncture    = Just 144.0
                                    , _gdSlash       = Just 18.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 360.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 130.90909
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 1.2e-2
      , _gpcPuncture    = Just 9.6e-2
      , _gpcSlash       = Just 1.2e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 10.584
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 185.29199
    , _adjustedBurstDPS     = Just 370.58398
    , _adjustedSustainedDPS = Just 134.75781
    }
  )

snipetronvandal = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName = Just "Snipetron Vandal"
                                , _gwMagazineSize         = Just 6.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 200.0
                                , _gwBaseDamage           = Just 200.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 13.333333
                                , _gwCriticalChance       = Just 0.28
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.15999997
                                , _gwFireRate             = Just 2.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.25
                                , _gwType                 = Just "Sniper Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 10.0
                                    , _gdPuncture    = Just 180.0
                                    , _gdSlash       = Just 10.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 400.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 240.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 7.9999985e-3
      , _gpcPuncture    = Just 0.14399996
      , _gpcSlash       = Just 7.9999985e-3
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 7.8399987
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 203.92
    , _adjustedBurstDPS     = Just 407.84
    , _adjustedSustainedDPS = Just 244.70401
    }
  )

sobek = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Sobek"
                                , _gwMagazineSize         = Just 20.0
                                , _gwReloadTime           = Just 2.7
                                , _gwTotalDamage          = Just 350.0
                                , _gwBaseDamage           = Just 350.0
                                , _gwBaseMultishot        = Just 5.0
                                , _gwMultishot            = Just 5.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 9.090909
                                , _gwCriticalChance       = Just 0.11
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.26999998
                                , _gwFireRate             = Just 2.5000002
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.33
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 240.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 262.5
                                    , _gdPuncture    = Just 43.75
                                    , _gdSlash       = Just 43.75
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 875.00006
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 654.2057
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.20249999
      , _gpcPuncture    = Just 3.3749998e-2
      , _gpcSlash       = Just 3.3749998e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 72.35156
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 378.9406
    , _adjustedBurstDPS     = Just 947.3516
    , _adjustedSustainedDPS = Just 708.3003
    }
  )

soma = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Soma"
                                , _gwMagazineSize         = Just 100.0
                                , _gwReloadTime           = Just 3.0
                                , _gwTotalDamage          = Just 12.0
                                , _gwBaseDamage           = Just 12.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 0.3
                                , _gwCriticalMultiplier   = Just 3.0
                                , _gwStatusChance         = Just 6.999999e-2
                                , _gwFireRate             = Just 15.000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.75
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 1.2
                                    , _gdPuncture    = Just 4.8
                                    , _gdSlash       = Just 6.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 1.0
                                }
                              , []
                              )
    , _burstDPS             = Just 180.00002
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 124.13794
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 6.9999993e-3
      , _gpcPuncture    = Just 2.7999997e-2
      , _gpcSlash       = Just 3.4999996e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 15.4349985
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 13.028999
    , _adjustedBurstDPS     = Just 195.435
    , _adjustedSustainedDPS = Just 134.78276
    }
  )

somaprime = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Soma Prime"
                                , _gwMagazineSize         = Just 200.0
                                , _gwReloadTime           = Just 3.0
                                , _gwTotalDamage          = Just 12.0
                                , _gwBaseDamage           = Just 12.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 0.3
                                , _gwCriticalMultiplier   = Just 3.0
                                , _gwStatusChance         = Just 0.10000002
                                , _gwFireRate             = Just 15.000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.75
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 800.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 1.2
                                    , _gdPuncture    = Just 4.8
                                    , _gdSlash       = Just 6.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 1.0
                                }
                              , []
                              )
    , _burstDPS             = Just 180.00002
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 146.93878
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 1.0000002e-2
      , _gpcPuncture    = Just 4.0000007e-2
      , _gpcSlash       = Just 5.000001e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 22.050003
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 13.47
    , _adjustedBurstDPS     = Just 202.05002
    , _adjustedSustainedDPS = Just 164.93878
    }
  )

stradavar = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Stradavar"
                                , _gwMagazineSize         = Just 65.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 28.0
                                , _gwBaseDamage           = Just 28.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 14.285714
                                , _gwCriticalChance       = Just 0.24
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.12
                                , _gwFireRate             = Just 10.000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.15
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 9.8
                                    , _gdPuncture    = Just 9.8
                                    , _gdSlash       = Just 8.4
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 280.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 214.11765
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 4.2e-2
      , _gpcPuncture    = Just 4.2e-2
      , _gpcSlash       = Just 3.6e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 24.696
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 30.4696
    , _adjustedBurstDPS     = Just 304.69604
    , _adjustedSustainedDPS = Just 233.00282
    }
  )

stradavarprime = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName = Just "Stradavar Prime"
                                , _gwMagazineSize         = Just 90.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 30.0
                                , _gwBaseDamage           = Just 30.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 25.0
                                , _gwCriticalChance       = Just 0.24
                                , _gwCriticalMultiplier   = Just 2.6
                                , _gwStatusChance         = Just 0.12
                                , _gwFireRate             = Just 10.000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.15
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 10.5
                                    , _gdPuncture    = Just 10.5
                                    , _gdSlash       = Just 9.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 300.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 245.45456
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 4.2e-2
      , _gpcPuncture    = Just 4.2e-2
      , _gpcSlash       = Just 3.6000002e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 26.460005
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 32.646
    , _adjustedBurstDPS     = Just 326.46002
    , _adjustedSustainedDPS = Just 267.10364
    }
  )

strun = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Strun"
                                , _gwMagazineSize         = Just 6.0
                                , _gwReloadTime           = Just 3.75
                                , _gwTotalDamage          = Just 300.0
                                , _gwBaseDamage           = Just 300.0
                                , _gwBaseMultishot        = Just 10.0
                                , _gwMultishot            = Just 10.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 4.0
                                , _gwCriticalChance       = Just 7.5e-2
                                , _gwCriticalMultiplier   = Just 1.5
                                , _gwStatusChance         = Just 0.20000029
                                , _gwFireRate             = Just 2.5000002
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.35
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 120.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 165.0
                                    , _gdPuncture    = Just 45.0
                                    , _gdSlash       = Just 90.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 750.00006
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 292.68295
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.11000016
      , _gpcPuncture    = Just 3.0000044e-2
      , _gpcSlash       = Just 6.0000088e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 110.25017
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 344.10007
    , _adjustedBurstDPS     = Just 860.25024
    , _adjustedSustainedDPS = Just 335.7074
    }
  )

strunwraith = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Strun Wraith"
                                , _gwMagazineSize         = Just 10.0
                                , _gwReloadTime           = Just 5.0
                                , _gwTotalDamage          = Just 400.0
                                , _gwBaseDamage           = Just 400.0
                                , _gwBaseMultishot        = Just 10.0
                                , _gwMultishot            = Just 10.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 6.6666665
                                , _gwCriticalChance       = Just 0.18
                                , _gwCriticalMultiplier   = Just 2.2
                                , _gwStatusChance         = Just 0.4000001
                                , _gwFireRate             = Just 2.5000002
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.35
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 120.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 260.0
                                    , _gdPuncture    = Just 60.0
                                    , _gdSlash       = Just 80.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 1000.0001
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 444.44446
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.26000005
      , _gpcPuncture    = Just 6.0000017e-2
      , _gpcSlash       = Just 8.000002e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 196.00006
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 478.40002
    , _adjustedBurstDPS     = Just 1196.0001
    , _adjustedSustainedDPS = Just 531.55554
    }
  )

supra = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Supra"
                                , _gwMagazineSize         = Just 180.0
                                , _gwReloadTime           = Just 3.0
                                , _gwTotalDamage          = Just 40.0
                                , _gwBaseDamage           = Just 40.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 14.285714
                                , _gwCriticalChance       = Just 0.12
                                , _gwCriticalMultiplier   = Just 1.8
                                , _gwStatusChance         = Just 0.3
                                , _gwFireRate             = Just 12.500001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.85
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 1080.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 4.0
                                    , _gdPuncture    = Just 30.0
                                    , _gdSlash       = Just 6.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 2.0
                                }
                              , []
                              )
    , _burstDPS             = Just 500.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 413.79315
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.0000001e-2
      , _gpcPuncture    = Just 0.22500001
      , _gpcSlash       = Just 4.5e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 55.125008
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 44.41
    , _adjustedBurstDPS     = Just 555.12506
    , _adjustedSustainedDPS = Just 459.41385
    }
  )

supravandal = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Supra Vandal"
                                , _gwMagazineSize         = Just 300.0
                                , _gwReloadTime           = Just 3.0
                                , _gwTotalDamage          = Just 40.0
                                , _gwBaseDamage           = Just 40.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 0.16
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.3
                                , _gwFireRate             = Just 12.500001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.85
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 1600.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 4.0
                                    , _gdPuncture    = Just 30.0
                                    , _gdSlash       = Just 6.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 2.0
                                }
                              , []
                              )
    , _burstDPS             = Just 500.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 444.4445
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.0000001e-2
      , _gpcPuncture    = Just 0.22500001
      , _gpcSlash       = Just 4.5e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 55.125008
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 44.41
    , _adjustedBurstDPS     = Just 555.12506
    , _adjustedSustainedDPS = Just 493.4445
    }
  )

sybaris = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Sybaris"
                                , _gwMagazineSize         = Just 10.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 80.0
                                , _gwBaseDamage           = Just 80.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 0.25
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.10000002
                                , _gwFireRate             = Just 3.9840639
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.0
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 26.4
                                    , _gdPuncture    = Just 26.4
                                    , _gdSlash       = Just 27.2
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 318.7251
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 177.38358
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.3000004e-2
      , _gpcPuncture    = Just 3.3000004e-2
      , _gpcSlash       = Just 3.4000006e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 26.549805
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 86.664
    , _adjustedBurstDPS     = Just 345.2749
    , _adjustedSustainedDPS = Just 192.15964
    }
  )

sybarisprime = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Sybaris Prime"
                                , _gwMagazineSize         = Just 20.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 88.0
                                , _gwBaseDamage           = Just 88.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 25.0
                                , _gwCriticalChance       = Just 0.3
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.25
                                , _gwFireRate             = Just 4.7169814
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.0
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 29.0
                                    , _gdPuncture    = Just 29.0
                                    , _gdSlash       = Just 29.9
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 415.09436
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 282.0513
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 8.248009e-2
      , _gpcPuncture    = Just 8.248009e-2
      , _gpcSlash       = Just 8.503982e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 86.48389
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 106.33458
    , _adjustedBurstDPS     = Just 501.57825
    , _adjustedSustainedDPS = Just 340.81598
    }
  )

synapse = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Synapse"
                                , _gwMagazineSize         = Just 70.0
                                , _gwReloadTime           = Just 1.5
                                , _gwTotalDamage          = Just 20.0
                                , _gwBaseDamage           = Just 20.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.39
                                , _gwCriticalMultiplier   = Just 2.7
                                , _gwStatusChance         = Just 0.13
                                , _gwFireRate             = Just 12.000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.315
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 20.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 240.00002
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 190.9091
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.13
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 20.0
    , _adjustedBurstDPS     = Just 240.00002
    , _adjustedSustainedDPS = Just 190.9091
    }
  )

synoid simulor = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName = Just "Synoid Simulor"
                                , _gwMagazineSize         = Just 16.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 75.0
                                , _gwBaseDamage           = Just 75.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 28.571428
                                , _gwCriticalChance       = Just 0.14
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.35000002
                                , _gwFireRate             = Just 3.3333335
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.85
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 96.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 20.0
                                    }
                                  )
                                , _gwDisposition          = Just 2.0
                                }
                              , []
                              )
    , _burstDPS             = Just 250.00002
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 176.4706
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.35000002
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 75.0
    , _adjustedBurstDPS     = Just 250.00002
    , _adjustedSustainedDPS = Just 176.4706
    }
  )

telos boltor = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Telos Boltor"
                                , _gwMagazineSize         = Just 90.0
                                , _gwReloadTime           = Just 2.4
                                , _gwTotalDamage          = Just 30.0
                                , _gwBaseDamage           = Just 30.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 25.0
                                , _gwCriticalChance       = Just 0.3
                                , _gwCriticalMultiplier   = Just 2.4
                                , _gwStatusChance         = Just 0.15999997
                                , _gwFireRate             = Just 9.333334
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.95
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 3.0
                                    , _gdPuncture    = Just 27.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 280.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 224.19931
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 1.5999997e-2
      , _gpcPuncture    = Just 0.14399996
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 30.0
    , _adjustedBurstDPS     = Just 280.00003
    , _adjustedSustainedDPS = Just 224.19931
    }
  )

tenora = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Tenora"
                                , _gwMagazineSize         = Just 150.0
                                , _gwReloadTime           = Just 2.5
                                , _gwTotalDamage          = Just 24.0
                                , _gwBaseDamage           = Just 24.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 12.5
                                , _gwCriticalChance       = Just 0.28
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.15999997
                                , _gwFireRate             = Just 11.666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.05
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 900.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 7.2
                                    , _gdPuncture    = Just 9.6
                                    , _gdSlash       = Just 7.2
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 280.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 234.41861
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 4.7999986e-2
      , _gpcPuncture    = Just 6.399999e-2
      , _gpcSlash       = Just 4.7999986e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 32.92799
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 26.8224
    , _adjustedBurstDPS     = Just 312.928
    , _adjustedSustainedDPS = Just 261.98624
    }
  )

tetra = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Tetra"
                                , _gwMagazineSize         = Just 60.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 32.0
                                , _gwBaseDamage           = Just 32.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 18.181818
                                , _gwCriticalChance       = Just 4.0e-2
                                , _gwCriticalMultiplier   = Just 1.5
                                , _gwStatusChance         = Just 0.19999999
                                , _gwFireRate             = Just 6.666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.5
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 6.4
                                    , _gdPuncture    = Just 25.6
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 213.33334
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 174.54546
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 4.0e-2
      , _gpcPuncture    = Just 0.16
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 32.0
    , _adjustedBurstDPS     = Just 213.33334
    , _adjustedSustainedDPS = Just 174.54546
    }
  )

tiberon = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Tiberon"
                                , _gwMagazineSize         = Just 30.0
                                , _gwReloadTime           = Just 2.26
                                , _gwTotalDamage          = Just 44.0
                                , _gwBaseDamage           = Just 44.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 33.333332
                                , _gwCriticalChance       = Just 0.26
                                , _gwCriticalMultiplier   = Just 2.4
                                , _gwStatusChance         = Just 0.15999997
                                , _gwFireRate             = Just 9.09091
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.1
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 11.0
                                    , _gdPuncture    = Just 22.0
                                    , _gdSlash       = Just 11.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 400.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 237.4101
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.999999e-2
      , _gpcPuncture    = Just 7.999998e-2
      , _gpcSlash       = Just 3.999999e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 39.199993
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 48.312
    , _adjustedBurstDPS     = Just 439.20004
    , _adjustedSustainedDPS = Just 260.67627
    }
  )

tiberonprime = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Tiberon Prime"
                                , _gwMagazineSize         = Just 42.0
                                , _gwReloadTime           = Just 2.0
                                , _gwTotalDamage          = Just 46.0
                                , _gwBaseDamage           = Just 46.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 33.333332
                                , _gwCriticalChance       = Just 0.28
                                , _gwCriticalMultiplier   = Just 3.0
                                , _gwStatusChance         = Just 0.19999999
                                , _gwFireRate             = Just 7.37705
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.1
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 13.8
                                    , _gdPuncture    = Just 18.4
                                    , _gdSlash       = Just 13.8
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 339.3443
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 251.12654
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 6.0e-2
      , _gpcPuncture    = Just 8.0e-2
      , _gpcSlash       = Just 6.0e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 49.883614
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 52.762
    , _adjustedBurstDPS     = Just 389.2279
    , _adjustedSustainedDPS = Just 288.04214
    }
  )

tigris = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Tigris"
                                , _gwMagazineSize         = Just 2.0
                                , _gwReloadTime           = Just 1.8
                                , _gwTotalDamage          = Just 1050.0
                                , _gwBaseDamage           = Just 1050.0
                                , _gwBaseMultishot        = Just 5.0
                                , _gwMultishot            = Just 5.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 9.090909
                                , _gwCriticalChance       = Just 0.1
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.2800001
                                , _gwFireRate             = Just 2.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.5
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 120.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 105.0
                                    , _gdPuncture    = Just 105.0
                                    , _gdSlash       = Just 840.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 1.0
                                }
                              , []
                              )
    , _burstDPS             = Just 2100.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 750.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 2.800001e-2
      , _gpcPuncture    = Just 2.800001e-2
      , _gpcSlash       = Just 0.22400008
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 1152.4805
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 1626.2402
    , _adjustedBurstDPS     = Just 3252.4805
    , _adjustedSustainedDPS = Just 1161.6002
    }
  )

tigrisprime = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Tigris Prime"
                                , _gwMagazineSize         = Just 2.0
                                , _gwReloadTime           = Just 1.8
                                , _gwTotalDamage          = Just 1560.0
                                , _gwBaseDamage           = Just 1560.0
                                , _gwBaseMultishot        = Just 8.0
                                , _gwMultishot            = Just 8.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 9.090909
                                , _gwCriticalChance       = Just 0.1
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.29999995
                                , _gwFireRate             = Just 2.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.5
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 120.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 156.0
                                    , _gdPuncture    = Just 156.0
                                    , _gdSlash       = Just 1248.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 1.0
                                }
                              , []
                              )
    , _burstDPS             = Just 3120.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 1114.2858
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 2.9999996e-2
      , _gpcPuncture    = Just 2.9999996e-2
      , _gpcSlash       = Just 0.23999996
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 1834.5597
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 2477.2798
    , _adjustedBurstDPS     = Just 4954.5596
    , _adjustedSustainedDPS = Just 1769.4856
    }
  )

tonkor = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Tonkor"
                                , _gwMagazineSize         = Just 1.0
                                , _gwReloadTime           = Just 1.7
                                , _gwTotalDamage          = Just 14.0
                                , _gwBaseDamage           = Just 14.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.25
                                , _gwCriticalMultiplier   = Just 2.5
                                , _gwStatusChance         = Just 0.10000002
                                , _gwFireRate             = Just 3.1666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.95
                                , _gwType                 = Just "Launcher"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 30.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 75.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 44.333336
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 6.9451694
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.10000002
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 14.0
    , _adjustedBurstDPS     = Just 44.333336
    , _adjustedSustainedDPS = Just 6.9451694
    }
  )

torid = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Torid"
                                , _gwMagazineSize         = Just 5.0
                                , _gwReloadTime           = Just 1.7
                                , _gwTotalDamage          = Just 0.0
                                , _gwBaseDamage           = Just 0.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.15
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.23000002
                                , _gwFireRate             = Just 1.5000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.3
                                , _gwType                 = Just "Launcher"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 60.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 0.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 100.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 0.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 0.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.0
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.23000002
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 0.0
    , _adjustedBurstDPS     = Just 0.0
    , _adjustedSustainedDPS = Just 0.0
    }
  )

vaykor hek = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Vaykor Hek"
                                , _gwMagazineSize         = Just 8.0
                                , _gwReloadTime           = Just 2.25
                                , _gwTotalDamage          = Just 525.0
                                , _gwBaseDamage           = Just 525.0
                                , _gwBaseMultishot        = Just 7.0
                                , _gwMultishot            = Just 7.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 9.090909
                                , _gwCriticalChance       = Just 0.25
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.24999988
                                , _gwFireRate             = Just 3.0000002
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.85
                                , _gwType                 = Just "Shotgun"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 120.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 78.75
                                    , _gdPuncture    = Just 341.25
                                    , _gdSlash       = Just 105.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 2.0
                                }
                              , []
                              )
    , _burstDPS             = Just 1575.0001
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 854.2373
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 3.7499983e-2
      , _gpcPuncture    = Just 0.16249992
      , _gpcSlash       = Just 4.999998e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 192.93742
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 589.3125
    , _adjustedBurstDPS     = Just 1767.9376
    , _adjustedSustainedDPS = Just 958.8814
    }
  )

vectis = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Vectis"
                                , _gwMagazineSize         = Just 1.0
                                , _gwReloadTime           = Just 1.0
                                , _gwTotalDamage          = Just 225.0
                                , _gwBaseDamage           = Just 225.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 13.333333
                                , _gwCriticalChance       = Just 0.25
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.3
                                , _gwFireRate             = Just 1.5000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.85
                                , _gwType                 = Just "Sniper Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 90.0
                                    , _gdPuncture    = Just 78.75
                                    , _gdSlash       = Just 56.25
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 2.0
                                }
                              , []
                              )
    , _burstDPS             = Just 337.50003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 135.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.120000005
      , _gpcPuncture    = Just 0.105000004
      , _gpcSlash       = Just 7.5e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 62.01563
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 266.34375
    , _adjustedBurstDPS     = Just 399.51566
    , _adjustedSustainedDPS = Just 159.80626
    }
  )

vectisprime = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Vectis Prime"
                                , _gwMagazineSize         = Just 2.0
                                , _gwReloadTime           = Just 0.85
                                , _gwTotalDamage          = Just 350.0
                                , _gwBaseDamage           = Just 350.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 13.333333
                                , _gwCriticalChance       = Just 0.3
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.3
                                , _gwFireRate             = Just 2.6666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 0.85
                                , _gwType                 = Just "Sniper Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 140.0
                                    , _gdPuncture    = Just 157.5
                                    , _gdSlash       = Just 52.5
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 2.0
                                }
                              , []
                              )
    , _burstDPS             = Just 933.3334
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 437.5
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.120000005
      , _gpcPuncture    = Just 0.135
      , _gpcSlash       = Just 4.5e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 102.90001
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 388.5875
    , _adjustedBurstDPS     = Just 1036.2334
    , _adjustedSustainedDPS = Just 485.73438
    }
  )

veldt = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Veldt"
                                , _gwMagazineSize         = Just 26.0
                                , _gwReloadTime           = Just 1.8
                                , _gwTotalDamage          = Just 90.0
                                , _gwBaseDamage           = Just 90.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 32.0
                                , _gwCriticalChance       = Just 0.22
                                , _gwCriticalMultiplier   = Just 2.2
                                , _gwStatusChance         = Just 0.22000003
                                , _gwFireRate             = Just 3.6666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.2
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 528.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 23.4
                                    , _gdPuncture    = Just 23.4
                                    , _gdSlash       = Just 43.2
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 330.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 263.1902
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 5.7200007e-2
      , _gpcPuncture    = Just 5.7200007e-2
      , _gpcSlash       = Just 0.105600014
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 85.37762
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 113.284805
    , _adjustedBurstDPS     = Just 415.37762
    , _adjustedSustainedDPS = Just 331.28278
    }
  )

vulkar = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Vulkar"
                                , _gwMagazineSize         = Just 6.0
                                , _gwReloadTime           = Just 3.0
                                , _gwTotalDamage          = Just 224.99998
                                , _gwBaseDamage           = Just 224.99998
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 13.333333
                                , _gwCriticalChance       = Just 0.2
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.25
                                , _gwFireRate             = Just 1.5000001
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.4
                                , _gwType                 = Just "Sniper Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 180.0
                                    , _gdPuncture    = Just 33.8
                                    , _gdSlash       = Just 11.2
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 337.5
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 192.85713
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.2
      , _gpcPuncture    = Just 3.7555553e-2
      , _gpcSlash       = Just 1.2444444e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 10.29
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 231.85999
    , _adjustedBurstDPS     = Just 347.79
    , _adjustedSustainedDPS = Just 198.73714
    }
  )

vulkarwraith = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Vulkar Wraith"
                                , _gwMagazineSize         = Just 8.0
                                , _gwReloadTime           = Just 3.0
                                , _gwTotalDamage          = Just 273.0
                                , _gwBaseDamage           = Just 273.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 13.333333
                                , _gwCriticalChance       = Just 0.2
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.25
                                , _gwFireRate             = Just 2.0
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.4
                                , _gwType                 = Just "Sniper Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 72.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 245.7
                                    , _gdPuncture    = Just 27.3
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 5.0
                                }
                              , []
                              )
    , _burstDPS             = Just 546.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 312.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.225
      , _gpcPuncture    = Just 2.4999999e-2
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 273.0
    , _adjustedBurstDPS     = Just 546.0
    , _adjustedSustainedDPS = Just 312.0
    }
  )

zarr = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Zarr"
                                , _gwMagazineSize         = Just 3.0
                                , _gwReloadTime           = Just 2.25
                                , _gwTotalDamage          = Just 0.0
                                , _gwBaseDamage           = Just 0.0
                                , _gwBaseMultishot        = Just 7.0
                                , _gwMultishot            = Just 7.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 100.0
                                , _gwCriticalChance       = Just 0.17
                                , _gwCriticalMultiplier   = Just 2.5
                                , _gwStatusChance         = Just 0.28999996
                                , _gwFireRate             = Just 1.6666667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.15
                                , _gwType                 = Just "Launcher"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 84.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 25.0
                                    , _gdPuncture    = Just 0.0
                                    , _gdSlash       = Just 0.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 0.0
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 0.0
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 0.28999996
      , _gpcPuncture    = Just 0.0
      , _gpcSlash       = Just 0.0
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 0.0
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 0.0
    , _adjustedBurstDPS     = Just 0.0
    , _adjustedSustainedDPS = Just 0.0
    }
  )

zenith = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Zenith"
                                , _gwMagazineSize         = Just 90.0
                                , _gwReloadTime           = Just 1.6
                                , _gwTotalDamage          = Just 30.0
                                , _gwBaseDamage           = Just 30.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 33.333332
                                , _gwCriticalChance       = Just 0.1
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.34000003
                                , _gwFireRate             = Just 10.833334
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.1
                                , _gwType                 = Just "Rifle"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 4.5
                                    , _gdPuncture    = Just 6.0
                                    , _gdSlash       = Just 19.5
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 3.0
                                }
                              , []
                              )
    , _burstDPS             = Just 325.00003
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 272.51553
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 5.1000006e-2
      , _gpcPuncture    = Just 6.800001e-2
      , _gpcSlash       = Just 0.22100002
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 175.97128
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 46.2435
    , _adjustedBurstDPS     = Just 500.97128
    , _adjustedSustainedDPS = Just 420.0691
    }
  )

zhuge = Right
  (ComprehensiveWeapon
    { _build                = ( GenericWeapon
                                { _gwName                 = Just "Zhuge"
                                , _gwMagazineSize         = Just 20.0
                                , _gwReloadTime           = Just 2.5
                                , _gwTotalDamage          = Just 100.0
                                , _gwBaseDamage           = Just 100.0
                                , _gwBaseMultishot        = Just 1.0
                                , _gwMultishot            = Just 1.0
                                , _gwPunchthrough         = Just 0.0
                                , _gwAccuracy             = Just 40.0
                                , _gwCriticalChance       = Just 0.2
                                , _gwCriticalMultiplier   = Just 2.0
                                , _gwStatusChance         = Just 0.35000002
                                , _gwFireRate             = Just 4.166667
                                , _gwChargeAttack         = Just 0.0
                                , _gwSpinAttack           = Just 0.0
                                , _gwLeapAttack           = Just 0.0
                                , _gwWallAttack           = Just 0.0
                                , _gwChannelingMultiplier = Just 0.0
                                , _gwChannelingCost       = Just 0.0
                                , _gwOmegaAttenuation     = Just 1.25
                                , _gwType                 = Just "Crossbow"
                                , _gwCategory             = Just "Primary"
                                , _gwAmmo                 = Just 540.0
                                , _gwDamageTypes          = Just
                                  (GenericDamage
                                    { _gdImpact      = Just 5.0
                                    , _gdPuncture    = Just 75.0
                                    , _gdSlash       = Just 20.0
                                    , _gdHeat        = Just 0.0
                                    , _gdCold        = Just 0.0
                                    , _gdToxin       = Just 0.0
                                    , _gdElectricity = Just 0.0
                                    , _gdBlast       = Just 0.0
                                    , _gdGas         = Just 0.0
                                    , _gdRadiation   = Just 0.0
                                    , _gdViral       = Just 0.0
                                    , _gdCorrosive   = Just 0.0
                                    , _gdMagnetic    = Just 0.0
                                    }
                                  )
                                , _gwDisposition          = Just 4.0
                                }
                              , []
                              )
    , _burstDPS             = Just 416.6667
    , _comboCounter         = Just 1.0
    , _sustainedDPS         = Just 273.97263
    , _procChances          = GenericProcChances
      { _gpcImpact      = Just 1.7500002e-2
      , _gpcPuncture    = Just 0.26250002
      , _gpcSlash       = Just 7.000001e-2
      , _gpcHeat        = Just 0.0
      , _gpcCold        = Just 0.0
      , _gpcToxin       = Just 0.0
      , _gpcElectricity = Just 0.0
      , _gpcBlast       = Just 0.0
      , _gpcGas         = Just 0.0
      , _gpcRadiation   = Just 0.0
      , _gpcViral       = Just 0.0
      , _gpcCorrosive   = Just 0.0
      , _gpcMagnetic    = Just 0.0
      }
    , _procDPSs             = GenericProcDPSs
      { _gpdImpact      = Nothing
      , _gpdPuncture    = Nothing
      , _gpdSlash       = Just 71.45834
      , _gpdHeat        = Just 0.0
      , _gpdCold        = Nothing
      , _gpdToxin       = Just 0.0
      , _gpdElectricity = Just 0.0
      , _gpdBlast       = Nothing
      , _gpdGas         = Just 0.0
      , _gpdRadiation   = Nothing
      , _gpdViral       = Nothing
      , _gpdCorrosive   = Nothing
      , _gpdMagnetic    = Nothing
      }
    , _adjustedTotalDamage  = Just 117.15
    , _adjustedBurstDPS     = Just 488.12503
    , _adjustedSustainedDPS = Just 320.95892
    }
  )

testComprehensiveWeaponBuilder :: SpecWith ()
testComprehensiveWeaponBuilder =
  it "Checks if given build successfully can be made comprehensive" $ do
    parsedAmprex <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Amprex"
    makeComprehensive (Just 1) . (, []) <$> parsedAmprex `shouldBe` amprex
    parsedAstilla <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Astilla"
    makeComprehensive (Just 1) . (, []) <$> parsedAstilla `shouldBe` astilla
    parsedAttica <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Attica"
    makeComprehensive (Just 1) . (, []) <$> parsedAttica `shouldBe` attica
    parsedBattacor <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Battacor"
    makeComprehensive (Just 1) . (, []) <$> parsedBattacor `shouldBe` battacor
    parsedBaza <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Baza"
    makeComprehensive (Just 1) . (, []) <$> parsedBaza `shouldBe` baza
    parsedBoar <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Boar"
    makeComprehensive (Just 1) . (, []) <$> parsedBoar `shouldBe` boar
    parsedBoar_Prime <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Boar_Prime"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedBoar_Prime
      `shouldBe` boarprime
    parsedBoltor <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Boltor"
    makeComprehensive (Just 1) . (, []) <$> parsedBoltor `shouldBe` boltor
    parsedBoltor_Prime <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Boltor_Prime"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedBoltor_Prime
      `shouldBe` boltorprime
    parsedBraton <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Braton"
    makeComprehensive (Just 1) . (, []) <$> parsedBraton `shouldBe` braton
    parsedBraton_Prime <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Braton_Prime"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedBraton_Prime
      `shouldBe` bratonprime
    parsedBraton_Vandal <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Braton_Vandal"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedBraton_Vandal
      `shouldBe` bratonvandal
    parsedBurston <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Burston"
    makeComprehensive (Just 1) . (, []) <$> parsedBurston `shouldBe` burston
    parsedBurston_Prime <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Burston_Prime"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedBurston_Prime
      `shouldBe` burstonprime
    parsedBuzlok <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Buzlok"
    makeComprehensive (Just 1) . (, []) <$> parsedBuzlok `shouldBe` buzlok
    parsedCernos <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Cernos"
    makeComprehensive (Just 1) . (, []) <$> parsedCernos `shouldBe` cernos
    parsedCernos_Prime <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Cernos_Prime"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedCernos_Prime
      `shouldBe` cernosprime
    parsedConvectrix <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Convectrix"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedConvectrix
      `shouldBe` convectrix
    parsedCorinth <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Corinth"
    makeComprehensive (Just 1) . (, []) <$> parsedCorinth `shouldBe` corinth
    parsedDaikyu <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Daikyu"
    makeComprehensive (Just 1) . (, []) <$> parsedDaikyu `shouldBe` daikyu
    parsedDera <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Dera"
    makeComprehensive (Just 1) . (, []) <$> parsedDera `shouldBe` dera
    parsedDera_Vandal <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Dera_Vandal"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedDera_Vandal
      `shouldBe` deravandal
    parsedDex_Sybaris <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Dex_Sybaris"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedDex_Sybaris
      `shouldBe` dexsybaris
    parsedDrakgoon <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Drakgoon"
    makeComprehensive (Just 1) . (, []) <$> parsedDrakgoon `shouldBe` drakgoon
    parsedDread <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Dread"
    makeComprehensive (Just 1) . (, []) <$> parsedDread `shouldBe` dread
    parsedExergis <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Exergis"
    makeComprehensive (Just 1) . (, []) <$> parsedExergis `shouldBe` exergis
    parsedFerrox <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Ferrox"
    makeComprehensive (Just 1) . (, []) <$> parsedFerrox `shouldBe` ferrox
    parsedFlux_Rifle <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Flux_Rifle"
    parsedFulmin_Auto <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Fulmin_Auto"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedFulmin_Auto
      `shouldBe` fulmin_auto
    parsedFulmin_Semi_Auto <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Fulmin_Semi_Auto"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedFulmin_Semi_Auto
      `shouldBe` fulmin_semi_auto
    parsedGlaxion <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Glaxion"
    makeComprehensive (Just 1) . (, []) <$> parsedGlaxion `shouldBe` glaxion
    parsedGlaxion_Vandal <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Glaxion_Vandal"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedGlaxion_Vandal
      `shouldBe` glaxionvandal
    parsedGorgon <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Gorgon"
    makeComprehensive (Just 1) . (, []) <$> parsedGorgon `shouldBe` gorgon
    parsedGorgon_Wraith <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Gorgon_Wraith"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedGorgon_Wraith
      `shouldBe` gorgonwraith
    parsedGrakata <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Grakata"
    makeComprehensive (Just 1) . (, []) <$> parsedGrakata `shouldBe` grakata
    parsedGrinlok <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Grinlok"
    makeComprehensive (Just 1) . (, []) <$> parsedGrinlok `shouldBe` grinlok
    parsedHarpak <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Harpak"
    makeComprehensive (Just 1) . (, []) <$> parsedHarpak `shouldBe` harpak
    parsedHek <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Hek"
    makeComprehensive (Just 1) . (, []) <$> parsedHek `shouldBe` hek
    parsedHema <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Hema"
    makeComprehensive (Just 1) . (, []) <$> parsedHema `shouldBe` hema
    parsedHind <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Hind"
    makeComprehensive (Just 1) . (, []) <$> parsedHind `shouldBe` hind
    parsedIgnis <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Ignis"
    makeComprehensive (Just 1) . (, []) <$> parsedIgnis `shouldBe` ignis
    parsedIgnis_Wraith <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Ignis_Wraith"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedIgnis_Wraith
      `shouldBe` igniswraith
    parsedJavlok <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Javlok"
    makeComprehensive (Just 1) . (, []) <$> parsedJavlok `shouldBe` javlok
    parsedKarak <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Karak"
    makeComprehensive (Just 1) . (, []) <$> parsedKarak `shouldBe` karak
    parsedKarak_Wraith <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Karak_Wraith"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedKarak_Wraith
      `shouldBe` karakwraith
    parsedKohm <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Kohm"
    makeComprehensive (Just 1) . (, []) <$> parsedKohm `shouldBe` kohm
    parsedLanka <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Lanka"
    makeComprehensive (Just 1) . (, []) <$> parsedLanka `shouldBe` lanka
    parsedLatron <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Latron"
    makeComprehensive (Just 1) . (, []) <$> parsedLatron `shouldBe` latron
    parsedLatron_Prime <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Latron_Prime"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedLatron_Prime
      `shouldBe` latronprime
    parsedLatron_Wraith <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Latron_Wraith"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedLatron_Wraith
      `shouldBe` latronwraith
    parsedLenz <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Lenz"
    makeComprehensive (Just 1) . (, []) <$> parsedLenz `shouldBe` lenz
    parsedMiter <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Miter"
    makeComprehensive (Just 1) . (, []) <$> parsedMiter `shouldBe` miter
    parsedMutalist_Cernos <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Mutalist_Cernos"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedMutalist_Cernos
      `shouldBe` mutalistcernos
    parsedNagantaka <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Nagantaka"
    makeComprehensive (Just 1) . (, []) <$> parsedNagantaka `shouldBe` nagantaka
    parsedOgris <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Ogris"
    makeComprehensive (Just 1) . (, []) <$> parsedOgris `shouldBe` ogris
    parsedOpticor <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Opticor"
    makeComprehensive (Just 1) . (, []) <$> parsedOpticor `shouldBe` opticor
    parsedOpticor_Vandal <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Opticor_Vandal"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedOpticor_Vandal
      `shouldBe` opticorvandal
    parsedPanthera <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Panthera"
    makeComprehensive (Just 1) . (, []) <$> parsedPanthera `shouldBe` panthera
    parsedParacyst <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Paracyst"
    makeComprehensive (Just 1) . (, []) <$> parsedParacyst `shouldBe` paracyst
    parsedParis <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Paris"
    makeComprehensive (Just 1) . (, []) <$> parsedParis `shouldBe` paris
    parsedParis_Prime <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Paris_Prime"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedParis_Prime
      `shouldBe` parisprime
    parsedPenta <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Penta"
    makeComprehensive (Just 1) . (, []) <$> parsedPenta `shouldBe` penta
    parsedPhage <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Phage"
    makeComprehensive (Just 1) . (, []) <$> parsedPhage `shouldBe` phage
    parsedPhantasma <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Phantasma"
    makeComprehensive (Just 1) . (, []) <$> parsedPhantasma `shouldBe` phantasma
    parsedPrisma_Grinlok <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Prisma_Grinlok"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedPrisma_Grinlok
      `shouldBe` prismagrinlok
    parsedPrisma_Tetra <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Prisma_Tetra"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedPrisma_Tetra
      `shouldBe` prismatetra
    parsedQuanta <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Quanta"
    makeComprehensive (Just 1) . (, []) <$> parsedQuanta `shouldBe` quanta
    parsedQuanta_Vandal <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Quanta_Vandal"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedQuanta_Vandal
      `shouldBe` quantavandal
    parsedQuartakk <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Quartakk"
    makeComprehensive (Just 1) . (, []) <$> parsedQuartakk `shouldBe` quartakk
    parsedRakta_Cernos <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Rakta_Cernos"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedRakta_Cernos
      `shouldBe` raktacernos
    parsedRubico <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Rubico"
    makeComprehensive (Just 1) . (, []) <$> parsedRubico `shouldBe` rubico
    parsedRubico_Prime <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Rubico_Prime"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedRubico_Prime
      `shouldBe` rubicoprime
    parsedScourge <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Scourge"
    makeComprehensive (Just 1) . (, []) <$> parsedScourge `shouldBe` scourge
    parsedSimulor <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Simulor"
    makeComprehensive (Just 1) . (, []) <$> parsedSimulor `shouldBe` simulor
    parsedSnipetron <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Snipetron"
    makeComprehensive (Just 1) . (, []) <$> parsedSnipetron `shouldBe` snipetron
    parsedSnipetron_Vandal <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Snipetron_Vandal"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedSnipetron_Vandal
      `shouldBe` snipetronvandal
    parsedSobek <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Sobek"
    makeComprehensive (Just 1) . (, []) <$> parsedSobek `shouldBe` sobek
    parsedSoma <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Soma"
    makeComprehensive (Just 1) . (, []) <$> parsedSoma `shouldBe` soma
    parsedSoma_Prime <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Soma_Prime"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedSoma_Prime
      `shouldBe` somaprime
    parsedStradavar <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Stradavar"
    makeComprehensive (Just 1) . (, []) <$> parsedStradavar `shouldBe` stradavar
    parsedStradavar_Prime <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Stradavar_Prime"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedStradavar_Prime
      `shouldBe` stradavarprime
    parsedStrun <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Strun"
    makeComprehensive (Just 1) . (, []) <$> parsedStrun `shouldBe` strun
    parsedStrun_Wraith <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Strun_Wraith"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedStrun_Wraith
      `shouldBe` strunwraith
    parsedSupra <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Supra"
    makeComprehensive (Just 1) . (, []) <$> parsedSupra `shouldBe` supra
    parsedSupra_Vandal <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Supra_Vandal"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedSupra_Vandal
      `shouldBe` supravandal
    parsedSybaris <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Sybaris"
    makeComprehensive (Just 1) . (, []) <$> parsedSybaris `shouldBe` sybaris
    parsedSybaris_Prime <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Sybaris_Prime"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedSybaris_Prime
      `shouldBe` sybarisprime
    parsedSynapse <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Synapse"
    makeComprehensive (Just 1) . (, []) <$> parsedSynapse `shouldBe` synapse
    parsedTenora <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Tenora"
    makeComprehensive (Just 1) . (, []) <$> parsedTenora `shouldBe` tenora
    parsedTetra <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Tetra"
    makeComprehensive (Just 1) . (, []) <$> parsedTetra `shouldBe` tetra
    parsedTiberon <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Tiberon"
    makeComprehensive (Just 1) . (, []) <$> parsedTiberon `shouldBe` tiberon
    parsedTiberon_Prime <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Tiberon_Prime"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedTiberon_Prime
      `shouldBe` tiberonprime
    parsedTigris <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Tigris"
    makeComprehensive (Just 1) . (, []) <$> parsedTigris `shouldBe` tigris
    parsedTigris_Prime <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Tigris_Prime"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedTigris_Prime
      `shouldBe` tigrisprime
    parsedTonkor <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Tonkor"
    makeComprehensive (Just 1) . (, []) <$> parsedTonkor `shouldBe` tonkor
    parsedTorid <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Torid"
    makeComprehensive (Just 1) . (, []) <$> parsedTorid `shouldBe` torid
    parsedVectis <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Vectis"
    makeComprehensive (Just 1) . (, []) <$> parsedVectis `shouldBe` vectis
    parsedVectis_Prime <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Vectis_Prime"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedVectis_Prime
      `shouldBe` vectisprime
    parsedVeldt <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Veldt"
    makeComprehensive (Just 1) . (, []) <$> parsedVeldt `shouldBe` veldt
    parsedVulkar <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Vulkar"
    makeComprehensive (Just 1) . (, []) <$> parsedVulkar `shouldBe` vulkar
    parsedVulkar_Wraith <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Vulkar_Wraith"
    makeComprehensive (Just 1)
      .          (, [])
      <$>        parsedVulkar_Wraith
      `shouldBe` vulkarwraith
    parsedZarr <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Zarr"
    makeComprehensive (Just 1) . (, []) <$> parsedZarr `shouldBe` zarr
    parsedZenith <- readWeapon
      "warframe-autobuilder-data/Primary_Weapons/Zenith"
    makeComprehensive (Just 1) . (, []) <$> parsedZenith `shouldBe` zenith
    parsedZhuge <- readWeapon "warframe-autobuilder-data/Primary_Weapons/Zhuge"
    makeComprehensive (Just 1) . (, []) <$> parsedZhuge `shouldBe` zhuge
