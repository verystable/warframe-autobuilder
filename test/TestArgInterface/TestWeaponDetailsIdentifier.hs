{-# LANGUAGE OverloadedStrings #-}

module TestArgInterface.TestWeaponDetailsIdentifier where

import           ArgInterface.WeaponDetailsIdentifier
import           Test.Hspec

testWeaponDetailsIdentifier :: SpecWith ()
testWeaponDetailsIdentifier =
  it
      "Checks if given weapon name belongs to a rifle or shotgun or melee weapon or a pistol"
    $ do
        weaponDetailsIdentifier "Soma Prime" `shouldBe` ("Soma_Prime", "Rifle")
        weaponDetailsIdentifier "Amprex" `shouldBe` ("Amprex", "Rifle")
        weaponDetailsIdentifier "Argonak" `shouldBe` ("Argonak", "Rifle")
        weaponDetailsIdentifier "Attica" `shouldBe` ("Attica", "Rifle")
        weaponDetailsIdentifier "Battacor" `shouldBe` ("Battacor", "Rifle")
        weaponDetailsIdentifier "Baza" `shouldBe` ("Baza", "Rifle")
        weaponDetailsIdentifier "Boltor" `shouldBe` ("Boltor", "Rifle")
        weaponDetailsIdentifier "Boltor_Prime"
          `shouldBe` ("Boltor_Prime", "Rifle")
        weaponDetailsIdentifier "Braton" `shouldBe` ("Braton", "Rifle")
        weaponDetailsIdentifier "Braton_Prime"
          `shouldBe` ("Braton_Prime", "Rifle")
        weaponDetailsIdentifier "Braton_Vandal"
          `shouldBe` ("Braton_Vandal", "Rifle")
        weaponDetailsIdentifier "Burston" `shouldBe` ("Burston", "Rifle")
        weaponDetailsIdentifier "Burston_Prime"
          `shouldBe` ("Burston_Prime", "Rifle")
        weaponDetailsIdentifier "Buzlok" `shouldBe` ("Buzlok", "Rifle")
        weaponDetailsIdentifier "Cernos" `shouldBe` ("Cernos", "Rifle")
        weaponDetailsIdentifier "Cernos_Prime"
          `shouldBe` ("Cernos_Prime", "Rifle")
        weaponDetailsIdentifier "Daikyu" `shouldBe` ("Daikyu", "Rifle")
        weaponDetailsIdentifier "Dera" `shouldBe` ("Dera", "Rifle")
        weaponDetailsIdentifier "Dera_Vandal"
          `shouldBe` ("Dera_Vandal", "Rifle")
        weaponDetailsIdentifier "Dex_Sybaris"
          `shouldBe` ("Dex_Sybaris", "Rifle")
        weaponDetailsIdentifier "Dread" `shouldBe` ("Dread", "Rifle")
        weaponDetailsIdentifier "Ferrox" `shouldBe` ("Ferrox", "Rifle")
        weaponDetailsIdentifier "Flux_Rifle" `shouldBe` ("Flux_Rifle", "Rifle")
        weaponDetailsIdentifier "Fulmin_Auto"
          `shouldBe` ("Fulmin_Auto", "Rifle")
        weaponDetailsIdentifier "Fulmin_Semi_Auto"
          `shouldBe` ("Fulmin_Semi_Auto", "Rifle")
        weaponDetailsIdentifier "Glaxion" `shouldBe` ("Glaxion", "Rifle")
        weaponDetailsIdentifier "Glaxion_Vandal"
          `shouldBe` ("Glaxion_Vandal", "Rifle")
        weaponDetailsIdentifier "Gorgon" `shouldBe` ("Gorgon", "Rifle")
        weaponDetailsIdentifier "Gorgon_Wraith"
          `shouldBe` ("Gorgon_Wraith", "Rifle")
        weaponDetailsIdentifier "Grakata" `shouldBe` ("Grakata", "Rifle")
        weaponDetailsIdentifier "Grinlok" `shouldBe` ("Grinlok", "Rifle")
        weaponDetailsIdentifier "Harpak" `shouldBe` ("Harpak", "Rifle")
        weaponDetailsIdentifier "Hema" `shouldBe` ("Hema", "Rifle")
        weaponDetailsIdentifier "Hind" `shouldBe` ("Hind", "Rifle")
        weaponDetailsIdentifier "Ignis" `shouldBe` ("Ignis", "Rifle")
        weaponDetailsIdentifier "Ignis_Wraith"
          `shouldBe` ("Ignis_Wraith", "Rifle")
        weaponDetailsIdentifier "Javlok" `shouldBe` ("Javlok", "Rifle")
        weaponDetailsIdentifier "Karak" `shouldBe` ("Karak", "Rifle")
        weaponDetailsIdentifier "Karak_Wraith"
          `shouldBe` ("Karak_Wraith", "Rifle")
        weaponDetailsIdentifier "Lanka" `shouldBe` ("Lanka", "Rifle")
        weaponDetailsIdentifier "Latron" `shouldBe` ("Latron", "Rifle")
        weaponDetailsIdentifier "Latron_Prime"
          `shouldBe` ("Latron_Prime", "Rifle")
        weaponDetailsIdentifier "Latron_Wraith"
          `shouldBe` ("Latron_Wraith", "Rifle")
        weaponDetailsIdentifier "Lenz" `shouldBe` ("Lenz", "Rifle")
        weaponDetailsIdentifier "Miter" `shouldBe` ("Miter", "Rifle")
        weaponDetailsIdentifier "Mk1-Braton" `shouldBe` ("Mk1-Braton", "Rifle")
        weaponDetailsIdentifier "Mk1-Paris" `shouldBe` ("Mk1-Paris", "Rifle")
        weaponDetailsIdentifier "Mutalist_Cernos"
          `shouldBe` ("Mutalist_Cernos", "Rifle")
        weaponDetailsIdentifier "Mutalist_Quanta"
          `shouldBe` ("Mutalist_Quanta", "Rifle")
        weaponDetailsIdentifier "Nagantaka" `shouldBe` ("Nagantaka", "Rifle")
        weaponDetailsIdentifier "Ogris" `shouldBe` ("Ogris", "Rifle")
        weaponDetailsIdentifier "Opticor" `shouldBe` ("Opticor", "Rifle")
        weaponDetailsIdentifier "Opticor_Vandal"
          `shouldBe` ("Opticor_Vandal", "Rifle")
        weaponDetailsIdentifier "Panthera" `shouldBe` ("Panthera", "Rifle")
        weaponDetailsIdentifier "Paracyst" `shouldBe` ("Paracyst", "Rifle")
        weaponDetailsIdentifier "Paris" `shouldBe` ("Paris", "Rifle")
        weaponDetailsIdentifier "Paris_Prime"
          `shouldBe` ("Paris_Prime", "Rifle")
        weaponDetailsIdentifier "Penta" `shouldBe` ("Penta", "Rifle")
        weaponDetailsIdentifier "Prisma_Gorgon"
          `shouldBe` ("Prisma_Gorgon", "Rifle")
        weaponDetailsIdentifier "Prisma_Grakata"
          `shouldBe` ("Prisma_Grakata", "Rifle")
        weaponDetailsIdentifier "Prisma_Grinlok"
          `shouldBe` ("Prisma_Grinlok", "Rifle")
        weaponDetailsIdentifier "Prisma_Tetra"
          `shouldBe` ("Prisma_Tetra", "Rifle")
        weaponDetailsIdentifier "Quanta" `shouldBe` ("Quanta", "Rifle")
        weaponDetailsIdentifier "Quanta_Vandal"
          `shouldBe` ("Quanta_Vandal", "Rifle")
        weaponDetailsIdentifier "Quartakk" `shouldBe` ("Quartakk", "Rifle")
        weaponDetailsIdentifier "Rakta_Cernos"
          `shouldBe` ("Rakta_Cernos", "Rifle")
        weaponDetailsIdentifier "Rubico" `shouldBe` ("Rubico", "Rifle")
        weaponDetailsIdentifier "Rubico_Prime"
          `shouldBe` ("Rubico_Prime", "Rifle")
        weaponDetailsIdentifier "Scourge" `shouldBe` ("Scourge", "Rifle")
        weaponDetailsIdentifier "Secura_Penta"
          `shouldBe` ("Secura_Penta", "Rifle")
        weaponDetailsIdentifier "Simulor" `shouldBe` ("Simulor", "Rifle")
        weaponDetailsIdentifier "Snipetron" `shouldBe` ("Snipetron", "Rifle")
        weaponDetailsIdentifier "Snipetron_Vandal"
          `shouldBe` ("Snipetron_Vandal", "Rifle")
        weaponDetailsIdentifier "Soma" `shouldBe` ("Soma", "Rifle")
        weaponDetailsIdentifier "Soma_Prime" `shouldBe` ("Soma_Prime", "Rifle")
        weaponDetailsIdentifier "Stradavar" `shouldBe` ("Stradavar", "Rifle")
        weaponDetailsIdentifier "Stradavar_Prime"
          `shouldBe` ("Stradavar_Prime", "Rifle")
        weaponDetailsIdentifier "Supra" `shouldBe` ("Supra", "Rifle")
        weaponDetailsIdentifier "Supra_Vandal"
          `shouldBe` ("Supra_Vandal", "Rifle")
        weaponDetailsIdentifier "Sybaris" `shouldBe` ("Sybaris", "Rifle")
        weaponDetailsIdentifier "Sybaris_Prime"
          `shouldBe` ("Sybaris_Prime", "Rifle")
        weaponDetailsIdentifier "Synapse" `shouldBe` ("Synapse", "Rifle")
        weaponDetailsIdentifier "Synoid_Simulor"
          `shouldBe` ("Synoid_Simulor", "Rifle")
        weaponDetailsIdentifier "Telos_Boltor"
          `shouldBe` ("Telos_Boltor", "Rifle")
        weaponDetailsIdentifier "Tenora" `shouldBe` ("Tenora", "Rifle")
        weaponDetailsIdentifier "Tetra" `shouldBe` ("Tetra", "Rifle")
        weaponDetailsIdentifier "Tiberon" `shouldBe` ("Tiberon", "Rifle")
        weaponDetailsIdentifier "Tiberon_Prime"
          `shouldBe` ("Tiberon_Prime", "Rifle")
        weaponDetailsIdentifier "Tonkor" `shouldBe` ("Tonkor", "Rifle")
        weaponDetailsIdentifier "Torid" `shouldBe` ("Torid", "Rifle")
        weaponDetailsIdentifier "Vectis" `shouldBe` ("Vectis", "Rifle")
        weaponDetailsIdentifier "Vectis_Prime"
          `shouldBe` ("Vectis_Prime", "Rifle")
        weaponDetailsIdentifier "Veldt" `shouldBe` ("Veldt", "Rifle")
        weaponDetailsIdentifier "Vulkar" `shouldBe` ("Vulkar", "Rifle")
        weaponDetailsIdentifier "Vulkar_Wraith"
          `shouldBe` ("Vulkar_Wraith", "Rifle")
        weaponDetailsIdentifier "Zarr" `shouldBe` ("Zarr", "Rifle")
        weaponDetailsIdentifier "Zenith" `shouldBe` ("Zenith", "Rifle")
        weaponDetailsIdentifier "Zhuge" `shouldBe` ("Zhuge", "Rifle")
        weaponDetailsIdentifier "Acrid" `shouldBe` ("Acrid", "Secondary")
        weaponDetailsIdentifier "Afuris" `shouldBe` ("Afuris", "Secondary")
        weaponDetailsIdentifier "Akbolto" `shouldBe` ("Akbolto", "Secondary")
        weaponDetailsIdentifier "Akbolto_Prime"
          `shouldBe` ("Akbolto_Prime", "Secondary")
        weaponDetailsIdentifier "Akbronco" `shouldBe` ("Akbronco", "Secondary")
        weaponDetailsIdentifier "Akbronco_Prime"
          `shouldBe` ("Akbronco_Prime", "Secondary")
        weaponDetailsIdentifier "Akjagara" `shouldBe` ("Akjagara", "Secondary")
        weaponDetailsIdentifier "Akjagara_Prime"
          `shouldBe` ("Akjagara_Prime", "Secondary")
        weaponDetailsIdentifier "Aklato" `shouldBe` ("Aklato", "Secondary")
        weaponDetailsIdentifier "Aklex" `shouldBe` ("Aklex", "Secondary")
        weaponDetailsIdentifier "Aklex_Prime"
          `shouldBe` ("Aklex_Prime", "Secondary")
        weaponDetailsIdentifier "Akmagnus" `shouldBe` ("Akmagnus", "Secondary")
        weaponDetailsIdentifier "Aksomati" `shouldBe` ("Aksomati", "Secondary")
        weaponDetailsIdentifier "Akstiletto"
          `shouldBe` ("Akstiletto", "Secondary")
        weaponDetailsIdentifier "Akstiletto_Prime"
          `shouldBe` ("Akstiletto_Prime", "Secondary")
        weaponDetailsIdentifier "Akvasto" `shouldBe` ("Akvasto", "Secondary")
        weaponDetailsIdentifier "Akvasto_Prime"
          `shouldBe` ("Akvasto_Prime", "Secondary")
        weaponDetailsIdentifier "Akzani" `shouldBe` ("Akzani", "Secondary")
        weaponDetailsIdentifier "Angstrum" `shouldBe` ("Angstrum", "Secondary")
        weaponDetailsIdentifier "Arca_Scisco"
          `shouldBe` ("Arca_Scisco", "Secondary")
        weaponDetailsIdentifier "Artax" `shouldBe` ("Artax", "Secondary")
        weaponDetailsIdentifier "Atomos" `shouldBe` ("Atomos", "Secondary")
        weaponDetailsIdentifier "Azima" `shouldBe` ("Azima", "Secondary")
        weaponDetailsIdentifier "Ballistica"
          `shouldBe` ("Ballistica", "Secondary")
        weaponDetailsIdentifier "Ballistica_Prime"
          `shouldBe` ("Ballistica_Prime", "Secondary")
        weaponDetailsIdentifier "Bolto" `shouldBe` ("Bolto", "Secondary")
        weaponDetailsIdentifier "Brakk" `shouldBe` ("Brakk", "Secondary")
        weaponDetailsIdentifier "Bronco" `shouldBe` ("Bronco", "Secondary")
        weaponDetailsIdentifier "Bronco_Prime"
          `shouldBe` ("Bronco_Prime", "Secondary")
        weaponDetailsIdentifier "Burst_Laser"
          `shouldBe` ("Burst_Laser", "Secondary")
        weaponDetailsIdentifier "Castanas" `shouldBe` ("Castanas", "Secondary")
        weaponDetailsIdentifier "Cestra" `shouldBe` ("Cestra", "Secondary")
        weaponDetailsIdentifier "Cryotra" `shouldBe` ("Cryotra", "Secondary")
        weaponDetailsIdentifier "Cycron" `shouldBe` ("Cycron", "Secondary")
        weaponDetailsIdentifier "Deconstructor"
          `shouldBe` ("Deconstructor", "Secondary")
        weaponDetailsIdentifier "Deconstructor_Prime"
          `shouldBe` ("Deconstructor_Prime", "Secondary")
        weaponDetailsIdentifier "Despair" `shouldBe` ("Despair", "Secondary")
        weaponDetailsIdentifier "Deth_Machine_Rifle"
          `shouldBe` ("Deth_Machine_Rifle", "Secondary")
        weaponDetailsIdentifier "Detron" `shouldBe` ("Detron", "Secondary")
        weaponDetailsIdentifier "Dex_Furis"
          `shouldBe` ("Dex_Furis", "Secondary")
        weaponDetailsIdentifier "Dual_Cestra"
          `shouldBe` ("Dual_Cestra", "Secondary")
        weaponDetailsIdentifier "Dual_Toxocyst"
          `shouldBe` ("Dual_Toxocyst", "Secondary")
        weaponDetailsIdentifier "Embolist" `shouldBe` ("Embolist", "Secondary")
        weaponDetailsIdentifier "Euphona_Prime"
          `shouldBe` ("Euphona_Prime", "Secondary")
        weaponDetailsIdentifier "Furis" `shouldBe` ("Furis", "Secondary")
        weaponDetailsIdentifier "Fusilai" `shouldBe` ("Fusilai", "Secondary")
        weaponDetailsIdentifier "Gammacor" `shouldBe` ("Gammacor", "Secondary")
        weaponDetailsIdentifier "Hikou" `shouldBe` ("Hikou", "Secondary")
        weaponDetailsIdentifier "Hikou_Prime"
          `shouldBe` ("Hikou_Prime", "Secondary")
        weaponDetailsIdentifier "Hystrix" `shouldBe` ("Hystrix", "Secondary")
        weaponDetailsIdentifier "Knell" `shouldBe` ("Knell", "Secondary")
        weaponDetailsIdentifier "Kohmak" `shouldBe` ("Kohmak", "Secondary")
        weaponDetailsIdentifier "Kraken" `shouldBe` ("Kraken", "Secondary")
        weaponDetailsIdentifier "Kulstar" `shouldBe` ("Kulstar", "Secondary")
        weaponDetailsIdentifier "Kunai" `shouldBe` ("Kunai", "Secondary")
        weaponDetailsIdentifier "Laser_Rifle"
          `shouldBe` ("Laser_Rifle", "Secondary")
        weaponDetailsIdentifier "Lato" `shouldBe` ("Lato", "Secondary")
        weaponDetailsIdentifier "Lato_Prime"
          `shouldBe` ("Lato_Prime", "Secondary")
        weaponDetailsIdentifier "Lato_Vandal"
          `shouldBe` ("Lato_Vandal", "Secondary")
        weaponDetailsIdentifier "Lex" `shouldBe` ("Lex", "Secondary")
        weaponDetailsIdentifier "Lex_Prime"
          `shouldBe` ("Lex_Prime", "Secondary")
        weaponDetailsIdentifier "Magnus" `shouldBe` ("Magnus", "Secondary")
        weaponDetailsIdentifier "Mara_Detron"
          `shouldBe` ("Mara_Detron", "Secondary")
        weaponDetailsIdentifier "Marelok" `shouldBe` ("Marelok", "Secondary")
        weaponDetailsIdentifier "Mk1-Furis"
          `shouldBe` ("Mk1-Furis", "Secondary")
        weaponDetailsIdentifier "Mk1-Kunai"
          `shouldBe` ("Mk1-Kunai", "Secondary")
        weaponDetailsIdentifier "Multron" `shouldBe` ("Multron", "Secondary")
        weaponDetailsIdentifier "Nukor" `shouldBe` ("Nukor", "Secondary")
        weaponDetailsIdentifier "Ocucor" `shouldBe` ("Ocucor", "Secondary")
        weaponDetailsIdentifier "Pandero" `shouldBe` ("Pandero", "Secondary")
        weaponDetailsIdentifier "Plinx" `shouldBe` ("Plinx", "Secondary")
        weaponDetailsIdentifier "Pox" `shouldBe` ("Pox", "Secondary")
        weaponDetailsIdentifier "Prime_Laser_Rifle"
          `shouldBe` ("Prime_Laser_Rifle", "Secondary")
        weaponDetailsIdentifier "Prisma_Angstrum"
          `shouldBe` ("Prisma_Angstrum", "Secondary")
        weaponDetailsIdentifier "Prisma_Burst_Laser"
          `shouldBe` ("Prisma_Burst_Laser", "Secondary")
        weaponDetailsIdentifier "Prisma_Twin_Gremlins"
          `shouldBe` ("Prisma_Twin_Gremlins", "Secondary")
        weaponDetailsIdentifier "Pyrana" `shouldBe` ("Pyrana", "Secondary")
        weaponDetailsIdentifier "Pyrana_Prime"
          `shouldBe` ("Pyrana_Prime", "Secondary")
        weaponDetailsIdentifier "Rakta_Ballistica"
          `shouldBe` ("Rakta_Ballistica", "Secondary")
        weaponDetailsIdentifier "Sancti_Castanas"
          `shouldBe` ("Sancti_Castanas", "Secondary")
        weaponDetailsIdentifier "Secura_Dual_Cestra"
          `shouldBe` ("Secura_Dual_Cestra", "Secondary")
        weaponDetailsIdentifier "Seer" `shouldBe` ("Seer", "Secondary")
        weaponDetailsIdentifier "Sicarus" `shouldBe` ("Sicarus", "Secondary")
        weaponDetailsIdentifier "Sicarus_Prime"
          `shouldBe` ("Sicarus_Prime", "Secondary")
        weaponDetailsIdentifier "Sonicor" `shouldBe` ("Sonicor", "Secondary")
        weaponDetailsIdentifier "Spectra" `shouldBe` ("Spectra", "Secondary")
        weaponDetailsIdentifier "Spira" `shouldBe` ("Spira", "Secondary")
        weaponDetailsIdentifier "Spira_Prime"
          `shouldBe` ("Spira_Prime", "Secondary")
        weaponDetailsIdentifier "Staticor" `shouldBe` ("Staticor", "Secondary")
        weaponDetailsIdentifier "Stinger" `shouldBe` ("Stinger", "Secondary")
        weaponDetailsIdentifier "Stubba" `shouldBe` ("Stubba", "Secondary")
        weaponDetailsIdentifier "Stug" `shouldBe` ("Stug", "Secondary")
        weaponDetailsIdentifier "Sweeper" `shouldBe` ("Sweeper", "Secondary")
        weaponDetailsIdentifier "Sweeper_Prime"
          `shouldBe` ("Sweeper_Prime", "Secondary")
        weaponDetailsIdentifier "Synoid_Gammacor"
          `shouldBe` ("Synoid_Gammacor", "Secondary")
        weaponDetailsIdentifier "Talons" `shouldBe` ("Talons", "Secondary")
        weaponDetailsIdentifier "Tazicor" `shouldBe` ("Tazicor", "Secondary")
        weaponDetailsIdentifier "Telos_Akbolto"
          `shouldBe` ("Telos_Akbolto", "Secondary")
        weaponDetailsIdentifier "Twin_Grakatas"
          `shouldBe` ("Twin_Grakatas", "Secondary")
        weaponDetailsIdentifier "Twin_Gremlins"
          `shouldBe` ("Twin_Gremlins", "Secondary")
        weaponDetailsIdentifier "Twin_Kohmak"
          `shouldBe` ("Twin_Kohmak", "Secondary")
        weaponDetailsIdentifier "Twin_Rogga"
          `shouldBe` ("Twin_Rogga", "Secondary")
        weaponDetailsIdentifier "Twin_Vipers"
          `shouldBe` ("Twin_Vipers", "Secondary")
        weaponDetailsIdentifier "Twin_Vipers_Wraith"
          `shouldBe` ("Twin_Vipers_Wraith", "Secondary")
        weaponDetailsIdentifier "Tysis" `shouldBe` ("Tysis", "Secondary")
        weaponDetailsIdentifier "Vasto" `shouldBe` ("Vasto", "Secondary")
        weaponDetailsIdentifier "Vasto_Prime"
          `shouldBe` ("Vasto_Prime", "Secondary")
        weaponDetailsIdentifier "Vaykor_Marelok"
          `shouldBe` ("Vaykor_Marelok", "Secondary")
        weaponDetailsIdentifier "Viper" `shouldBe` ("Viper", "Secondary")
        weaponDetailsIdentifier "Viper_Wraith"
          `shouldBe` ("Viper_Wraith", "Secondary")
        weaponDetailsIdentifier "Vulcax" `shouldBe` ("Vulcax", "Secondary")
        weaponDetailsIdentifier "Vulklok" `shouldBe` ("Vulklok", "Secondary")
        weaponDetailsIdentifier "Zakti" `shouldBe` ("Zakti", "Secondary")
        weaponDetailsIdentifier "Zylok" `shouldBe` ("Zylok", "Secondary")
        weaponDetailsIdentifier "Kitgun_Tombfinger_Haymaker_Splat"
          `shouldBe` ("Kitgun_Tombfinger_Haymaker_Splat", "Secondary")
        weaponDetailsIdentifier "Ack_&_Brunt"
          `shouldBe` ("Ack_&_Brunt", "Melee")
        weaponDetailsIdentifier "Amphis" `shouldBe` ("Amphis", "Melee")
        weaponDetailsIdentifier "Anku" `shouldBe` ("Anku", "Melee")
        weaponDetailsIdentifier "Ankyros" `shouldBe` ("Ankyros", "Melee")
        weaponDetailsIdentifier "Ankyros_Prime"
          `shouldBe` ("Ankyros_Prime", "Melee")
        weaponDetailsIdentifier "Arca_Titron"
          `shouldBe` ("Arca_Titron", "Melee")
        weaponDetailsIdentifier "Atterax" `shouldBe` ("Atterax", "Melee")
        weaponDetailsIdentifier "Bo" `shouldBe` ("Bo", "Melee")
        weaponDetailsIdentifier "Boltace" `shouldBe` ("Boltace", "Melee")
        weaponDetailsIdentifier "Bo_Prime" `shouldBe` ("Bo_Prime", "Melee")
        weaponDetailsIdentifier "Broken_Scepter"
          `shouldBe` ("Broken_Scepter", "Melee")
        weaponDetailsIdentifier "Broken_War" `shouldBe` ("Broken_War", "Melee")
        weaponDetailsIdentifier "Cassowar" `shouldBe` ("Cassowar", "Melee")
        weaponDetailsIdentifier "Caustacyst" `shouldBe` ("Caustacyst", "Melee")
        weaponDetailsIdentifier "Ceramic_Dagger"
          `shouldBe` ("Ceramic_Dagger", "Melee")
        weaponDetailsIdentifier "Cerata" `shouldBe` ("Cerata", "Melee")
        weaponDetailsIdentifier "Cobra_&_Crane"
          `shouldBe` ("Cobra_&_Crane", "Melee")
        weaponDetailsIdentifier "Cronus" `shouldBe` ("Cronus", "Melee")
        weaponDetailsIdentifier "Dakra_Prime"
          `shouldBe` ("Dakra_Prime", "Melee")
        weaponDetailsIdentifier "Dark_Dagger"
          `shouldBe` ("Dark_Dagger", "Melee")
        weaponDetailsIdentifier "Dark_Split-Sword"
          `shouldBe` ("Dark_Split-Sword", "Melee")
        weaponDetailsIdentifier "Dark_Sword" `shouldBe` ("Dark_Sword", "Melee")
        weaponDetailsIdentifier "Destreza" `shouldBe` ("Destreza", "Melee")
        weaponDetailsIdentifier "Destreza_Prime"
          `shouldBe` ("Destreza_Prime", "Melee")
        weaponDetailsIdentifier "Dex_Dakra" `shouldBe` ("Dex_Dakra", "Melee")
        weaponDetailsIdentifier "Dragon_Nikana"
          `shouldBe` ("Dragon_Nikana", "Melee")
        weaponDetailsIdentifier "Dual_Cleavers"
          `shouldBe` ("Dual_Cleavers", "Melee")
        weaponDetailsIdentifier "Dual_Ether" `shouldBe` ("Dual_Ether", "Melee")
        weaponDetailsIdentifier "Dual_Heat_Swords"
          `shouldBe` ("Dual_Heat_Swords", "Melee")
        weaponDetailsIdentifier "Dual_Ichor" `shouldBe` ("Dual_Ichor", "Melee")
        weaponDetailsIdentifier "Dual_Kamas" `shouldBe` ("Dual_Kamas", "Melee")
        weaponDetailsIdentifier "Dual_Kamas_Prime"
          `shouldBe` ("Dual_Kamas_Prime", "Melee")
        weaponDetailsIdentifier "Dual_Keres" `shouldBe` ("Dual_Keres", "Melee")
        weaponDetailsIdentifier "Dual_Raza" `shouldBe` ("Dual_Raza", "Melee")
        weaponDetailsIdentifier "Dual_Skana" `shouldBe` ("Dual_Skana", "Melee")
        weaponDetailsIdentifier "Dual_Zoren" `shouldBe` ("Dual_Zoren", "Melee")
        weaponDetailsIdentifier "Endura" `shouldBe` ("Endura", "Melee")
        weaponDetailsIdentifier "Ether_Daggers"
          `shouldBe` ("Ether_Daggers", "Melee")
        weaponDetailsIdentifier "Ether_Reaper"
          `shouldBe` ("Ether_Reaper", "Melee")
        weaponDetailsIdentifier "Ether_Sword"
          `shouldBe` ("Ether_Sword", "Melee")
        weaponDetailsIdentifier "Falcor" `shouldBe` ("Falcor", "Melee")
        weaponDetailsIdentifier "Fang" `shouldBe` ("Fang", "Melee")
        weaponDetailsIdentifier "Fang_Prime" `shouldBe` ("Fang_Prime", "Melee")
        weaponDetailsIdentifier "Fragor" `shouldBe` ("Fragor", "Melee")
        weaponDetailsIdentifier "Fragor_Prime"
          `shouldBe` ("Fragor_Prime", "Melee")
        weaponDetailsIdentifier "Furax" `shouldBe` ("Furax", "Melee")
        weaponDetailsIdentifier "Furax_Wraith"
          `shouldBe` ("Furax_Wraith", "Melee")
        weaponDetailsIdentifier "Galatine" `shouldBe` ("Galatine", "Melee")
        weaponDetailsIdentifier "Galatine_Prime"
          `shouldBe` ("Galatine_Prime", "Melee")
        weaponDetailsIdentifier "Galvacord" `shouldBe` ("Galvacord", "Melee")
        weaponDetailsIdentifier "Gazal_Machete"
          `shouldBe` ("Gazal_Machete", "Melee")
        weaponDetailsIdentifier "Glaive" `shouldBe` ("Glaive", "Melee")
        weaponDetailsIdentifier "Glaive_Prime"
          `shouldBe` ("Glaive_Prime", "Melee")
        weaponDetailsIdentifier "Gram" `shouldBe` ("Gram", "Melee")
        weaponDetailsIdentifier "Gram_Prime" `shouldBe` ("Gram_Prime", "Melee")
        weaponDetailsIdentifier "Guandao" `shouldBe` ("Guandao", "Melee")
        weaponDetailsIdentifier "Gunsen" `shouldBe` ("Gunsen", "Melee")
        weaponDetailsIdentifier "Halikar" `shouldBe` ("Halikar", "Melee")
        weaponDetailsIdentifier "Hate" `shouldBe` ("Hate", "Melee")
        weaponDetailsIdentifier "Heat_Dagger"
          `shouldBe` ("Heat_Dagger", "Melee")
        weaponDetailsIdentifier "Heat_Sword" `shouldBe` ("Heat_Sword", "Melee")
        weaponDetailsIdentifier "Heliocor" `shouldBe` ("Heliocor", "Melee")
        weaponDetailsIdentifier "Hirudo" `shouldBe` ("Hirudo", "Melee")
        weaponDetailsIdentifier "Jat_Kittag" `shouldBe` ("Jat_Kittag", "Melee")
        weaponDetailsIdentifier "Jat_Kusar" `shouldBe` ("Jat_Kusar", "Melee")
        weaponDetailsIdentifier "Jaw_Sword" `shouldBe` ("Jaw_Sword", "Melee")
        weaponDetailsIdentifier "Kama" `shouldBe` ("Kama", "Melee")
        weaponDetailsIdentifier "Karyst" `shouldBe` ("Karyst", "Melee")
        weaponDetailsIdentifier "Kesheg" `shouldBe` ("Kesheg", "Melee")
        weaponDetailsIdentifier "Kestrel" `shouldBe` ("Kestrel", "Melee")
        weaponDetailsIdentifier "Kogake" `shouldBe` ("Kogake", "Melee")
        weaponDetailsIdentifier "Kogake_Prime"
          `shouldBe` ("Kogake_Prime", "Melee")
        weaponDetailsIdentifier "Korrudo" `shouldBe` ("Korrudo", "Melee")
        weaponDetailsIdentifier "Kreska" `shouldBe` ("Kreska", "Melee")
        weaponDetailsIdentifier "Krohkur" `shouldBe` ("Krohkur", "Melee")
        weaponDetailsIdentifier "Kronen" `shouldBe` ("Kronen", "Melee")
        weaponDetailsIdentifier "Kronen_Prime"
          `shouldBe` ("Kronen_Prime", "Melee")
        weaponDetailsIdentifier "Lacera" `shouldBe` ("Lacera", "Melee")
        weaponDetailsIdentifier "Lecta" `shouldBe` ("Lecta", "Melee")
        weaponDetailsIdentifier "Lesion" `shouldBe` ("Lesion", "Melee")
        weaponDetailsIdentifier "Machete" `shouldBe` ("Machete", "Melee")
        weaponDetailsIdentifier "Machete_Wraith"
          `shouldBe` ("Machete_Wraith", "Melee")
        weaponDetailsIdentifier "Magistar" `shouldBe` ("Magistar", "Melee")
        weaponDetailsIdentifier "Mios" `shouldBe` ("Mios", "Melee")
        weaponDetailsIdentifier "Mire" `shouldBe` ("Mire", "Melee")
        weaponDetailsIdentifier "Mk1-Bo" `shouldBe` ("Mk1-Bo", "Melee")
        weaponDetailsIdentifier "Mk1-Furax" `shouldBe` ("Mk1-Furax", "Melee")
        weaponDetailsIdentifier "Nami_Skyla" `shouldBe` ("Nami_Skyla", "Melee")
        weaponDetailsIdentifier "Nami_Skyla_Prime"
          `shouldBe` ("Nami_Skyla_Prime", "Melee")
        weaponDetailsIdentifier "Nami_Solo" `shouldBe` ("Nami_Solo", "Melee")
        weaponDetailsIdentifier "Nikana" `shouldBe` ("Nikana", "Melee")
        weaponDetailsIdentifier "Nikana_Prime"
          `shouldBe` ("Nikana_Prime", "Melee")
        weaponDetailsIdentifier "Ninkondi" `shouldBe` ("Ninkondi", "Melee")
        weaponDetailsIdentifier "Obex" `shouldBe` ("Obex", "Melee")
        weaponDetailsIdentifier "Ohma" `shouldBe` ("Ohma", "Melee")
        weaponDetailsIdentifier "Okina" `shouldBe` ("Okina", "Melee")
        weaponDetailsIdentifier "Orthos" `shouldBe` ("Orthos", "Melee")
        weaponDetailsIdentifier "Orthos_Prime"
          `shouldBe` ("Orthos_Prime", "Melee")
        weaponDetailsIdentifier "Orvius" `shouldBe` ("Orvius", "Melee")
        weaponDetailsIdentifier "Pangolin_Sword"
          `shouldBe` ("Pangolin_Sword", "Melee")
        weaponDetailsIdentifier "Paracesis" `shouldBe` ("Paracesis", "Melee")
        weaponDetailsIdentifier "Plasma_Sword"
          `shouldBe` ("Plasma_Sword", "Melee")
        weaponDetailsIdentifier "Prisma_Dual_Cleavers"
          `shouldBe` ("Prisma_Dual_Cleavers", "Melee")
        weaponDetailsIdentifier "Prisma_Machete"
          `shouldBe` ("Prisma_Machete", "Melee")
        weaponDetailsIdentifier "Prisma_Obex"
          `shouldBe` ("Prisma_Obex", "Melee")
        weaponDetailsIdentifier "Prisma_Skana"
          `shouldBe` ("Prisma_Skana", "Melee")
        weaponDetailsIdentifier "Prova" `shouldBe` ("Prova", "Melee")
        weaponDetailsIdentifier "Prova_Vandal"
          `shouldBe` ("Prova_Vandal", "Melee")
        weaponDetailsIdentifier "Pupacyst" `shouldBe` ("Pupacyst", "Melee")
        weaponDetailsIdentifier "Rakta_Dark_Dagger"
          `shouldBe` ("Rakta_Dark_Dagger", "Melee")
        weaponDetailsIdentifier "Reaper_Prime"
          `shouldBe` ("Reaper_Prime", "Melee")
        weaponDetailsIdentifier "Redeemer" `shouldBe` ("Redeemer", "Melee")
        weaponDetailsIdentifier "Redeemer_Prime"
          `shouldBe` ("Redeemer_Prime", "Melee")
        weaponDetailsIdentifier "Ripkas" `shouldBe` ("Ripkas", "Melee")
        weaponDetailsIdentifier "Sancti_Magistar"
          `shouldBe` ("Sancti_Magistar", "Melee")
        weaponDetailsIdentifier "Sarpa" `shouldBe` ("Sarpa", "Melee")
        weaponDetailsIdentifier "Scindo" `shouldBe` ("Scindo", "Melee")
        weaponDetailsIdentifier "Scindo_Prime"
          `shouldBe` ("Scindo_Prime", "Melee")
        weaponDetailsIdentifier "Scoliac" `shouldBe` ("Scoliac", "Melee")
        weaponDetailsIdentifier "Secura_Lecta"
          `shouldBe` ("Secura_Lecta", "Melee")
        weaponDetailsIdentifier "Serro" `shouldBe` ("Serro", "Melee")
        weaponDetailsIdentifier "Shaku" `shouldBe` ("Shaku", "Melee")
        weaponDetailsIdentifier "Sheev" `shouldBe` ("Sheev", "Melee")
        weaponDetailsIdentifier "Sibear" `shouldBe` ("Sibear", "Melee")
        weaponDetailsIdentifier "Sigma_&_Octantis"
          `shouldBe` ("Sigma_&_Octantis", "Melee")
        weaponDetailsIdentifier "Silva_&_Aegis"
          `shouldBe` ("Silva_&_Aegis", "Melee")
        weaponDetailsIdentifier "Silva_&_Aegis_Prime"
          `shouldBe` ("Silva_&_Aegis_Prime", "Melee")
        weaponDetailsIdentifier "Skana" `shouldBe` ("Skana", "Melee")
        weaponDetailsIdentifier "Skana_Prime"
          `shouldBe` ("Skana_Prime", "Melee")
        weaponDetailsIdentifier "Skiajati" `shouldBe` ("Skiajati", "Melee")
        weaponDetailsIdentifier "Sydon" `shouldBe` ("Sydon", "Melee")
        weaponDetailsIdentifier "Synoid_Heliocor"
          `shouldBe` ("Synoid_Heliocor", "Melee")
        weaponDetailsIdentifier "Tatsu" `shouldBe` ("Tatsu", "Melee")
        weaponDetailsIdentifier "Tekko" `shouldBe` ("Tekko", "Melee")
        weaponDetailsIdentifier "Telos_Boltace"
          `shouldBe` ("Telos_Boltace", "Melee")
        weaponDetailsIdentifier "Tipedo" `shouldBe` ("Tipedo", "Melee")
        weaponDetailsIdentifier "Tipedo_Prime"
          `shouldBe` ("Tipedo_Prime", "Melee")
        weaponDetailsIdentifier "Tonbo" `shouldBe` ("Tonbo", "Melee")
        weaponDetailsIdentifier "Twin_Basolk"
          `shouldBe` ("Twin_Basolk", "Melee")
        weaponDetailsIdentifier "Twin_Krohkur"
          `shouldBe` ("Twin_Krohkur", "Melee")
        weaponDetailsIdentifier "Vaykor_Sydon"
          `shouldBe` ("Vaykor_Sydon", "Melee")
        weaponDetailsIdentifier "Venka" `shouldBe` ("Venka", "Melee")
        weaponDetailsIdentifier "Venka_Prime"
          `shouldBe` ("Venka_Prime", "Melee")
        weaponDetailsIdentifier "Volnus" `shouldBe` ("Volnus", "Melee")
        weaponDetailsIdentifier "War" `shouldBe` ("War", "Melee")
        weaponDetailsIdentifier "Wolf_Sledge"
          `shouldBe` ("Wolf_Sledge", "Melee")
        weaponDetailsIdentifier "Zenistar" `shouldBe` ("Zenistar", "Melee")
        weaponDetailsIdentifier "Plague_Kripath+Shtung+Ekwana_Jai_II"
          `shouldBe` ("Plague_Kripath+Shtung+Ekwana_Jai_II", "Melee")
        weaponDetailsIdentifier "Arca_Plasmor"
          `shouldBe` ("Arca_Plasmor", "Shotgun")
        weaponDetailsIdentifier "Astilla" `shouldBe` ("Astilla", "Shotgun")
        weaponDetailsIdentifier "Boar" `shouldBe` ("Boar", "Shotgun")
        weaponDetailsIdentifier "Boar_Prime"
          `shouldBe` ("Boar_Prime", "Shotgun")
        weaponDetailsIdentifier "Convectrix"
          `shouldBe` ("Convectrix", "Shotgun")
        weaponDetailsIdentifier "Corinth" `shouldBe` ("Corinth", "Shotgun")
        weaponDetailsIdentifier "Drakgoon" `shouldBe` ("Drakgoon", "Shotgun")
        weaponDetailsIdentifier "Exergis" `shouldBe` ("Exergis", "Shotgun")
        weaponDetailsIdentifier "Hek" `shouldBe` ("Hek", "Shotgun")
        weaponDetailsIdentifier "Kohm" `shouldBe` ("Kohm", "Shotgun")
        weaponDetailsIdentifier "Mk1-Strun" `shouldBe` ("Mk1-Strun", "Shotgun")
        weaponDetailsIdentifier "Phage" `shouldBe` ("Phage", "Shotgun")
        weaponDetailsIdentifier "Phantasma" `shouldBe` ("Phantasma", "Shotgun")
        weaponDetailsIdentifier "Sancti_Tigris"
          `shouldBe` ("Sancti_Tigris", "Shotgun")
        weaponDetailsIdentifier "Sobek" `shouldBe` ("Sobek", "Shotgun")
        weaponDetailsIdentifier "Strun" `shouldBe` ("Strun", "Shotgun")
        weaponDetailsIdentifier "Strun_Wraith"
          `shouldBe` ("Strun_Wraith", "Shotgun")
        weaponDetailsIdentifier "Tigris" `shouldBe` ("Tigris", "Shotgun")
        weaponDetailsIdentifier "Tigris_Prime"
          `shouldBe` ("Tigris_Prime", "Shotgun")
        weaponDetailsIdentifier "Vaykor_Hek"
          `shouldBe` ("Vaykor_Hek", "Shotgun")
