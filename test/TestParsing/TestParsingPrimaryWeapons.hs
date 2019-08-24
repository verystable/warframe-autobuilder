{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestParsing.TestParsingPrimaryWeapons where

import           Test.Hspec
import           ClassyPrelude
import           Types.ComprehensiveWeapon      ( ComprehensiveWeapon )
import           ComprehensiveWeapon.ComprehensiveFunctions
                                                ( makeComprehensive )

import           GenericFunctions.GenericFunctions
                                                ( readWeapon )
import           Data.Either                    ( isRight )
import           System.Directory               ( listDirectory )

testParsingAmprex :: SpecWith ()
testParsingAmprex = it "Checks if Amprex file can be parsed successfully" $ do
  wAmprexTest <- checkIfParsed "Amprex"
  wAmprexTest `shouldBe` True

testParsingFulmin_Auto :: SpecWith ()
testParsingFulmin_Auto =
  it "Checks if Fulmin_Auto can be parsed successfully" $ do
    wFulmin_AutoTest <- checkIfParsed "Fulmin_Auto"
    wFulmin_AutoTest `shouldBe` True

testParsingCernos :: SpecWith ()
testParsingCernos = it "Checks if Cernos can be parsed successfully" $ do
  wCernosTest <- checkIfParsed "Cernos"
  wCernosTest `shouldBe` True

testParsingScourge :: SpecWith ()
testParsingScourge = it "Checks if Scourge can be parsed successfully" $ do
  wScourgeTest <- checkIfParsed "Scourge"
  wScourgeTest `shouldBe` True

testParsingDera_Vandal :: SpecWith ()
testParsingDera_Vandal =
  it "Checks if Dera_Vandal can be parsed successfully" $ do
    wDera_VandalTest <- checkIfParsed "Dera_Vandal"
    wDera_VandalTest `shouldBe` True

testParsingVulkar :: SpecWith ()
testParsingVulkar = it "Checks if Vulkar can be parsed successfully" $ do
  wVulkarTest <- checkIfParsed "Vulkar"
  wVulkarTest `shouldBe` True

testParsingRakta_Cernos :: SpecWith ()
testParsingRakta_Cernos =
  it "Checks if Rakta_Cernos can be parsed successfully" $ do
    wRakta_CernosTest <- checkIfParsed "Rakta_Cernos"
    wRakta_CernosTest `shouldBe` True

testParsingSupra :: SpecWith ()
testParsingSupra = it "Checks if Supra can be parsed successfully" $ do
  wSupraTest <- checkIfParsed "Supra"
  wSupraTest `shouldBe` True

testParsingSecura_Penta :: SpecWith ()
testParsingSecura_Penta =
  it "Checks if Secura_Penta can be parsed successfully" $ do
    wSecura_PentaTest <- checkIfParsed "Secura_Penta"
    wSecura_PentaTest `shouldBe` True

testParsingDex_Sybaris :: SpecWith ()
testParsingDex_Sybaris =
  it "Checks if Dex_Sybaris can be parsed successfully" $ do
    wDex_SybarisTest <- checkIfParsed "Dex_Sybaris"
    wDex_SybarisTest `shouldBe` True

testParsingTiberon :: SpecWith ()
testParsingTiberon = it "Checks if Tiberon can be parsed successfully" $ do
  wTiberonTest <- checkIfParsed "Tiberon"
  wTiberonTest `shouldBe` True

testParsingMk1Braton :: SpecWith ()
testParsingMk1Braton =
  it "Checks if Mk1-Braton can be parsed successfully" $ do
    wMk1BratonTest <- checkIfParsed "Mk1-Braton"
    wMk1BratonTest `shouldBe` True

testParsingSancti_Tigris :: SpecWith ()
testParsingSancti_Tigris =
  it "Checks if Sancti_Tigris can be parsed successfully" $ do
    wSancti_TigrisTest <- checkIfParsed "Sancti_Tigris"
    wSancti_TigrisTest `shouldBe` True

testParsingBoltor :: SpecWith ()
testParsingBoltor = it "Checks if Boltor can be parsed successfully" $ do
  wBoltorTest <- checkIfParsed "Boltor"
  wBoltorTest `shouldBe` True

testParsingDera :: SpecWith ()
testParsingDera = it "Checks if Dera can be parsed successfully" $ do
  wDeraTest <- checkIfParsed "Dera"
  wDeraTest `shouldBe` True

testParsingLatron_Prime :: SpecWith ()
testParsingLatron_Prime =
  it "Checks if Latron_Prime can be parsed successfully" $ do
    wLatron_PrimeTest <- checkIfParsed "Latron_Prime"
    wLatron_PrimeTest `shouldBe` True

testParsingDread :: SpecWith ()
testParsingDread = it "Checks if Dread can be parsed successfully" $ do
  wDreadTest <- checkIfParsed "Dread"
  wDreadTest `shouldBe` True

testParsingVectis :: SpecWith ()
testParsingVectis = it "Checks if Vectis can be parsed successfully" $ do
  wVectisTest <- checkIfParsed "Vectis"
  wVectisTest `shouldBe` True

testParsingBurston_Prime :: SpecWith ()
testParsingBurston_Prime =
  it "Checks if Burston_Prime can be parsed successfully" $ do
    wBurston_PrimeTest <- checkIfParsed "Burston_Prime"
    wBurston_PrimeTest `shouldBe` True

testParsingMiter :: SpecWith ()
testParsingMiter = it "Checks if Miter can be parsed successfully" $ do
  wMiterTest <- checkIfParsed "Miter"
  wMiterTest `shouldBe` True

testParsingSynapse :: SpecWith ()
testParsingSynapse = it "Checks if Synapse can be parsed successfully" $ do
  wSynapseTest <- checkIfParsed "Synapse"
  wSynapseTest `shouldBe` True

testParsingPrisma_Gorgon :: SpecWith ()
testParsingPrisma_Gorgon =
  it "Checks if Prisma_Gorgon can be parsed successfully" $ do
    wPrisma_GorgonTest <- checkIfParsed "Prisma_Gorgon"
    wPrisma_GorgonTest `shouldBe` True

testParsingFlux_Rifle :: SpecWith ()
testParsingFlux_Rifle =
  it "Checks if Flux_Rifle can be parsed successfully" $ do
    wFlux_RifleTest <- checkIfParsed "Flux_Rifle"
    wFlux_RifleTest `shouldBe` True

testParsingDrakgoon :: SpecWith ()
testParsingDrakgoon = it "Checks if Drakgoon can be parsed successfully" $ do
  wDrakgoonTest <- checkIfParsed "Drakgoon"
  wDrakgoonTest `shouldBe` True

testParsingBurston :: SpecWith ()
testParsingBurston = it "Checks if Burston can be parsed successfully" $ do
  wBurstonTest <- checkIfParsed "Burston"
  wBurstonTest `shouldBe` True

testParsingHarpak :: SpecWith ()
testParsingHarpak = it "Checks if Harpak can be parsed successfully" $ do
  wHarpakTest <- checkIfParsed "Harpak"
  wHarpakTest `shouldBe` True

testParsingKarak :: SpecWith ()
testParsingKarak = it "Checks if Karak can be parsed successfully" $ do
  wKarakTest <- checkIfParsed "Karak"
  wKarakTest `shouldBe` True

testParsingZenith :: SpecWith ()
testParsingZenith = it "Checks if Zenith can be parsed successfully" $ do
  wZenithTest <- checkIfParsed "Zenith"
  wZenithTest `shouldBe` True

testParsingPrisma_Grakata :: SpecWith ()
testParsingPrisma_Grakata =
  it "Checks if Prisma_Grakata can be parsed successfully" $ do
    wPrisma_GrakataTest <- checkIfParsed "Prisma_Grakata"
    wPrisma_GrakataTest `shouldBe` True

testParsingAttica :: SpecWith ()
testParsingAttica = it "Checks if Attica can be parsed successfully" $ do
  wAtticaTest <- checkIfParsed "Attica"
  wAtticaTest `shouldBe` True

testParsingQuartakk :: SpecWith ()
testParsingQuartakk = it "Checks if Quartakk can be parsed successfully" $ do
  wQuartakkTest <- checkIfParsed "Quartakk"
  wQuartakkTest `shouldBe` True

testParsingDaikyu :: SpecWith ()
testParsingDaikyu = it "Checks if Daikyu can be parsed successfully" $ do
  wDaikyuTest <- checkIfParsed "Daikyu"
  wDaikyuTest `shouldBe` True

testParsingTorid :: SpecWith ()
testParsingTorid = it "Checks if Torid can be parsed successfully" $ do
  wToridTest <- checkIfParsed "Torid"
  wToridTest `shouldBe` True

testParsingFerrox :: SpecWith ()
testParsingFerrox = it "Checks if Ferrox can be parsed successfully" $ do
  wFerroxTest <- checkIfParsed "Ferrox"
  wFerroxTest `shouldBe` True

testParsingQuanta_Vandal :: SpecWith ()
testParsingQuanta_Vandal =
  it "Checks if Quanta_Vandal can be parsed successfully" $ do
    wQuanta_VandalTest <- checkIfParsed "Quanta_Vandal"
    wQuanta_VandalTest `shouldBe` True

testParsingLenz :: SpecWith ()
testParsingLenz = it "Checks if Lenz can be parsed successfully" $ do
  wLenzTest <- checkIfParsed "Lenz"
  wLenzTest `shouldBe` True

testParsingBraton_Vandal :: SpecWith ()
testParsingBraton_Vandal =
  it "Checks if Braton_Vandal can be parsed successfully" $ do
    wBraton_VandalTest <- checkIfParsed "Braton_Vandal"
    wBraton_VandalTest `shouldBe` True

testParsingGorgon_Wraith :: SpecWith ()
testParsingGorgon_Wraith =
  it "Checks if Gorgon_Wraith can be parsed successfully" $ do
    wGorgon_WraithTest <- checkIfParsed "Gorgon_Wraith"
    wGorgon_WraithTest `shouldBe` True

testParsingConvectrix :: SpecWith ()
testParsingConvectrix =
  it "Checks if Convectrix can be parsed successfully" $ do
    wConvectrixTest <- checkIfParsed "Convectrix"
    wConvectrixTest `shouldBe` True

testParsingPanthera :: SpecWith ()
testParsingPanthera = it "Checks if Panthera can be parsed successfully" $ do
  wPantheraTest <- checkIfParsed "Panthera"
  wPantheraTest `shouldBe` True

testParsingMk1Paris_Uncharged :: SpecWith ()
testParsingMk1Paris_Uncharged =
  it "Checks if Mk1-Paris_Uncharged can be parsed successfully" $ do
    wMk1Paris_UnchargedTest <- checkIfParsed "Mk1-Paris_Uncharged"
    wMk1Paris_UnchargedTest `shouldBe` True

testParsingSoma_Prime :: SpecWith ()
testParsingSoma_Prime =
  it "Checks if Soma_Prime can be parsed successfully" $ do
    wSoma_PrimeTest <- checkIfParsed "Soma_Prime"
    wSoma_PrimeTest `shouldBe` True

testParsingParis_Prime :: SpecWith ()
testParsingParis_Prime =
  it "Checks if Paris_Prime can be parsed successfully" $ do
    wParis_PrimeTest <- checkIfParsed "Paris_Prime"
    wParis_PrimeTest `shouldBe` True

testParsingIgnis_Wraith :: SpecWith ()
testParsingIgnis_Wraith =
  it "Checks if Ignis_Wraith can be parsed successfully" $ do
    wIgnis_WraithTest <- checkIfParsed "Ignis_Wraith"
    wIgnis_WraithTest `shouldBe` True

testParsingQuanta :: SpecWith ()
testParsingQuanta = it "Checks if Quanta can be parsed successfully" $ do
  wQuantaTest <- checkIfParsed "Quanta"
  wQuantaTest `shouldBe` True

testParsingArca_Plasmor :: SpecWith ()
testParsingArca_Plasmor =
  it "Checks if Arca_Plasmor can be parsed successfully" $ do
    wArca_PlasmorTest <- checkIfParsed "Arca_Plasmor"
    wArca_PlasmorTest `shouldBe` True

testParsingCorinth :: SpecWith ()
testParsingCorinth = it "Checks if Corinth can be parsed successfully" $ do
  wCorinthTest <- checkIfParsed "Corinth"
  wCorinthTest `shouldBe` True

testParsingIgnis :: SpecWith ()
testParsingIgnis = it "Checks if Ignis can be parsed successfully" $ do
  wIgnisTest <- checkIfParsed "Ignis"
  wIgnisTest `shouldBe` True

testParsingFulmin_Semi_Auto :: SpecWith ()
testParsingFulmin_Semi_Auto =
  it "Checks if Fulmin_Semi_Auto can be parsed successfully" $ do
    wFulmin_Semi_AutoTest <- checkIfParsed "Fulmin_Semi_Auto"
    wFulmin_Semi_AutoTest `shouldBe` True

testParsingLatron_Wraith :: SpecWith ()
testParsingLatron_Wraith =
  it "Checks if Latron_Wraith can be parsed successfully" $ do
    wLatron_WraithTest <- checkIfParsed "Latron_Wraith"
    wLatron_WraithTest `shouldBe` True

testParsingAstilla :: SpecWith ()
testParsingAstilla = it "Checks if Astilla can be parsed successfully" $ do
  wAstillaTest <- checkIfParsed "Astilla"
  wAstillaTest `shouldBe` True

testParsingExergis :: SpecWith ()
testParsingExergis = it "Checks if Exergis can be parsed successfully" $ do
  wExergisTest <- checkIfParsed "Exergis"
  wExergisTest `shouldBe` True

testParsingCernos_Prime :: SpecWith ()
testParsingCernos_Prime =
  it "Checks if Cernos_Prime can be parsed successfully" $ do
    wCernos_PrimeTest <- checkIfParsed "Cernos_Prime"
    wCernos_PrimeTest `shouldBe` True

testParsingSnipetron :: SpecWith ()
testParsingSnipetron = it "Checks if Snipetron can be parsed successfully" $ do
  wSnipetronTest <- checkIfParsed "Snipetron"
  wSnipetronTest `shouldBe` True

testParsingRubico_Prime :: SpecWith ()
testParsingRubico_Prime =
  it "Checks if Rubico_Prime can be parsed successfully" $ do
    wRubico_PrimeTest <- checkIfParsed "Rubico_Prime"
    wRubico_PrimeTest `shouldBe` True

testParsingSybaris :: SpecWith ()
testParsingSybaris = it "Checks if Sybaris can be parsed successfully" $ do
  wSybarisTest <- checkIfParsed "Sybaris"
  wSybarisTest `shouldBe` True

testParsingTetra :: SpecWith ()
testParsingTetra = it "Checks if Tetra can be parsed successfully" $ do
  wTetraTest <- checkIfParsed "Tetra"
  wTetraTest `shouldBe` True

testParsingGlaxion_Vandal :: SpecWith ()
testParsingGlaxion_Vandal =
  it "Checks if Glaxion_Vandal can be parsed successfully" $ do
    wGlaxion_VandalTest <- checkIfParsed "Glaxion_Vandal"
    wGlaxion_VandalTest `shouldBe` True

testParsingBuzlok :: SpecWith ()
testParsingBuzlok = it "Checks if Buzlok can be parsed successfully" $ do
  wBuzlokTest <- checkIfParsed "Buzlok"
  wBuzlokTest `shouldBe` True

testParsingMk1Paris_Charged :: SpecWith ()
testParsingMk1Paris_Charged =
  it "Checks if Mk1-Paris_Charged can be parsed successfully" $ do
    wMk1Paris_ChargedTest <- checkIfParsed "Mk1-Paris_Charged"
    wMk1Paris_ChargedTest `shouldBe` True

testParsingKohm :: SpecWith ()
testParsingKohm = it "Checks if Kohm can be parsed successfully" $ do
  wKohmTest <- checkIfParsed "Kohm"
  wKohmTest `shouldBe` True

testParsingGrakata :: SpecWith ()
testParsingGrakata = it "Checks if Grakata can be parsed successfully" $ do
  wGrakataTest <- checkIfParsed "Grakata"
  wGrakataTest `shouldBe` True

testParsingMk1Strun :: SpecWith ()
testParsingMk1Strun = it "Checks if Mk1-Strun can be parsed successfully" $ do
  wMk1StrunTest <- checkIfParsed "Mk1-Strun"
  wMk1StrunTest `shouldBe` True

testParsingSybaris_Prime :: SpecWith ()
testParsingSybaris_Prime =
  it "Checks if Sybaris_Prime can be parsed successfully" $ do
    wSybaris_PrimeTest <- checkIfParsed "Sybaris_Prime"
    wSybaris_PrimeTest `shouldBe` True

testParsingVectis_Prime :: SpecWith ()
testParsingVectis_Prime =
  it "Checks if Vectis_Prime can be parsed successfully" $ do
    wVectis_PrimeTest <- checkIfParsed "Vectis_Prime"
    wVectis_PrimeTest `shouldBe` True

testParsingTonkor :: SpecWith ()
testParsingTonkor = it "Checks if Tonkor can be parsed successfully" $ do
  wTonkorTest <- checkIfParsed "Tonkor"
  wTonkorTest `shouldBe` True

testParsingBoltor_Prime :: SpecWith ()
testParsingBoltor_Prime =
  it "Checks if Boltor_Prime can be parsed successfully" $ do
    wBoltor_PrimeTest <- checkIfParsed "Boltor_Prime"
    wBoltor_PrimeTest `shouldBe` True

testParsingHema :: SpecWith ()
testParsingHema = it "Checks if Hema can be parsed successfully" $ do
  wHemaTest <- checkIfParsed "Hema"
  wHemaTest `shouldBe` True

testParsingSoma :: SpecWith ()
testParsingSoma = it "Checks if Soma can be parsed successfully" $ do
  wSomaTest <- checkIfParsed "Soma"
  wSomaTest `shouldBe` True

testParsingMutalist_Cernos :: SpecWith ()
testParsingMutalist_Cernos =
  it "Checks if Mutalist_Cernos can be parsed successfully" $ do
    wMutalist_CernosTest <- checkIfParsed "Mutalist_Cernos"
    wMutalist_CernosTest `shouldBe` True

testParsingParacyst :: SpecWith ()
testParsingParacyst = it "Checks if Paracyst can be parsed successfully" $ do
  wParacystTest <- checkIfParsed "Paracyst"
  wParacystTest `shouldBe` True

testParsingMutalist_Quanta :: SpecWith ()
testParsingMutalist_Quanta =
  it "Checks if Mutalist_Quanta can be parsed successfully" $ do
    wMutalist_QuantaTest <- checkIfParsed "Mutalist_Quanta"
    wMutalist_QuantaTest `shouldBe` True

testParsingSnipetron_Vandal :: SpecWith ()
testParsingSnipetron_Vandal =
  it "Checks if Snipetron_Vandal can be parsed successfully" $ do
    wSnipetron_VandalTest <- checkIfParsed "Snipetron_Vandal"
    wSnipetron_VandalTest `shouldBe` True

testParsingTelos_Boltor :: SpecWith ()
testParsingTelos_Boltor =
  it "Checks if Telos_Boltor can be parsed successfully" $ do
    wTelos_BoltorTest <- checkIfParsed "Telos_Boltor"
    wTelos_BoltorTest `shouldBe` True

testParsingSobek :: SpecWith ()
testParsingSobek = it "Checks if Sobek can be parsed successfully" $ do
  wSobekTest <- checkIfParsed "Sobek"
  wSobekTest `shouldBe` True

testParsingZhuge :: SpecWith ()
testParsingZhuge = it "Checks if Zhuge can be parsed successfully" $ do
  wZhugeTest <- checkIfParsed "Zhuge"
  wZhugeTest `shouldBe` True

testParsingGlaxion :: SpecWith ()
testParsingGlaxion = it "Checks if Glaxion can be parsed successfully" $ do
  wGlaxionTest <- checkIfParsed "Glaxion"
  wGlaxionTest `shouldBe` True

testParsingPhantasma :: SpecWith ()
testParsingPhantasma = it "Checks if Phantasma can be parsed successfully" $ do
  wPhantasmaTest <- checkIfParsed "Phantasma"
  wPhantasmaTest `shouldBe` True

testParsingZarr :: SpecWith ()
testParsingZarr = it "Checks if Zarr can be parsed successfully" $ do
  wZarrTest <- checkIfParsed "Zarr"
  wZarrTest `shouldBe` True

testParsingArgonak :: SpecWith ()
testParsingArgonak = it "Checks if Argonak can be parsed successfully" $ do
  wArgonakTest <- checkIfParsed "Argonak"
  wArgonakTest `shouldBe` True

testParsingSynoid_Simulor :: SpecWith ()
testParsingSynoid_Simulor =
  it "Checks if Synoid_Simulor can be parsed successfully" $ do
    wSynoid_SimulorTest <- checkIfParsed "Synoid_Simulor"
    wSynoid_SimulorTest `shouldBe` True

testParsingPrisma_Tetra :: SpecWith ()
testParsingPrisma_Tetra =
  it "Checks if Prisma_Tetra can be parsed successfully" $ do
    wPrisma_TetraTest <- checkIfParsed "Prisma_Tetra"
    wPrisma_TetraTest `shouldBe` True

testParsingStrun_Wraith :: SpecWith ()
testParsingStrun_Wraith =
  it "Checks if Strun_Wraith can be parsed successfully" $ do
    wStrun_WraithTest <- checkIfParsed "Strun_Wraith"
    wStrun_WraithTest `shouldBe` True

testParsingOpticor :: SpecWith ()
testParsingOpticor = it "Checks if Opticor can be parsed successfully" $ do
  wOpticorTest <- checkIfParsed "Opticor"
  wOpticorTest `shouldBe` True

testParsingGrinlok :: SpecWith ()
testParsingGrinlok = it "Checks if Grinlok can be parsed successfully" $ do
  wGrinlokTest <- checkIfParsed "Grinlok"
  wGrinlokTest `shouldBe` True

testParsingTigris_Prime :: SpecWith ()
testParsingTigris_Prime =
  it "Checks if Tigris_Prime can be parsed successfully" $ do
    wTigris_PrimeTest <- checkIfParsed "Tigris_Prime"
    wTigris_PrimeTest `shouldBe` True

testParsingPenta :: SpecWith ()
testParsingPenta = it "Checks if Penta can be parsed successfully" $ do
  wPentaTest <- checkIfParsed "Penta"
  wPentaTest `shouldBe` True


testParsingJavlok :: SpecWith ()
testParsingJavlok = it "Checks if Javlok can be parsed successfully" $ do
  wJavlokTest <- checkIfParsed "Javlok"
  wJavlokTest `shouldBe` True

testParsingOgris :: SpecWith ()
testParsingOgris = it "Checks if Ogris can be parsed successfully" $ do
  wOgrisTest <- checkIfParsed "Ogris"
  wOgrisTest `shouldBe` True

testParsingBaza :: SpecWith ()
testParsingBaza = it "Checks if Baza can be parsed successfully" $ do
  wBazaTest <- checkIfParsed "Baza"
  wBazaTest `shouldBe` True

testParsingParis :: SpecWith ()
testParsingParis = it "Checks if Paris can be parsed successfully" $ do
  wParisTest <- checkIfParsed "Paris"
  wParisTest `shouldBe` True

testParsingGorgon :: SpecWith ()
testParsingGorgon = it "Checks if Gorgon can be parsed successfully" $ do
  wGorgonTest <- checkIfParsed "Gorgon"
  wGorgonTest `shouldBe` True

testParsingTenora :: SpecWith ()
testParsingTenora = it "Checks if Tenora can be parsed successfully" $ do
  wTenoraTest <- checkIfParsed "Tenora"
  wTenoraTest `shouldBe` True

testParsingNagantaka :: SpecWith ()
testParsingNagantaka = it "Checks if Nagantaka can be parsed successfully" $ do

  wNagantakaTest <- checkIfParsed "Nagantaka"
  wNagantakaTest `shouldBe` True

testParsingStradavar_Prime :: SpecWith ()
testParsingStradavar_Prime =
  it "Checks if Stradavar_Prime can be parsed successfully" $ do
    wStradavar_PrimeTest <- checkIfParsed "Stradavar_Prime"
    wStradavar_PrimeTest `shouldBe` True

testParsingSupra_Vandal :: SpecWith ()
testParsingSupra_Vandal =
  it "Checks if Supra_Vandal can be parsed successfully" $ do
    wSupra_VandalTest <- checkIfParsed "Supra_Vandal"
    wSupra_VandalTest `shouldBe` True

testParsingLanka :: SpecWith ()
testParsingLanka = it "Checks if Lanka can be parsed successfully" $ do
  wLankaTest <- checkIfParsed "Lanka"
  wLankaTest `shouldBe` True

testParsingSimulor :: SpecWith ()
testParsingSimulor = it "Checks if Simulor can be parsed successfully" $ do
  wSimulorTest <- checkIfParsed "Simulor"
  wSimulorTest `shouldBe` True

testParsingOpticor_Vandal :: SpecWith ()
testParsingOpticor_Vandal =
  it "Checks if Opticor_Vandal can be parsed successfully" $ do
    wOpticor_VandalTest <- checkIfParsed "Opticor_Vandal"
    wOpticor_VandalTest `shouldBe` True

testParsingLatron :: SpecWith ()
testParsingLatron = it "Checks if Latron can be parsed successfully" $ do
  wLatronTest <- checkIfParsed "Latron"
  wLatronTest `shouldBe` True

testParsingHek :: SpecWith ()
testParsingHek = it "Checks if Hek can be parsed successfully" $ do
  wHekTest <- checkIfParsed "Hek"
  wHekTest `shouldBe` True

testParsingBoar :: SpecWith ()
testParsingBoar = it "Checks if Boar can be parsed successfully" $ do
  wBoarTest <- checkIfParsed "Boar"
  wBoarTest `shouldBe` True

testParsingPrisma_Grinlok :: SpecWith ()
testParsingPrisma_Grinlok =
  it "Checks if Prisma_Grinlok can be parsed successfully" $ do
    wPrisma_GrinlokTest <- checkIfParsed "Prisma_Grinlok"
    wPrisma_GrinlokTest `shouldBe` True

testParsingKarak_Wraith :: SpecWith ()
testParsingKarak_Wraith =
  it "Checks if Karak_Wraith can be parsed successfully" $ do
    wKarak_WraithTest <- checkIfParsed "Karak_Wraith"
    wKarak_WraithTest `shouldBe` True

testParsingRubico :: SpecWith ()
testParsingRubico = it "Checks if Rubico can be parsed successfully" $ do
  wRubicoTest <- checkIfParsed "Rubico"
  wRubicoTest `shouldBe` True

testParsingBraton_Prime :: SpecWith ()
testParsingBraton_Prime =
  it "Checks if Braton_Prime can be parsed successfully" $ do
    wBraton_PrimeTest <- checkIfParsed "Braton_Prime"
    wBraton_PrimeTest `shouldBe` True

testParsingVulkar_Wraith :: SpecWith ()
testParsingVulkar_Wraith =
  it "Checks if Vulkar_Wraith can be parsed successfully" $ do
    wVulkar_WraithTest <- checkIfParsed "Vulkar_Wraith"
    wVulkar_WraithTest `shouldBe` True

testParsingVeldt :: SpecWith ()
testParsingVeldt = it "Checks if Veldt can be parsed successfully" $ do
  wVeldtTest <- checkIfParsed "Veldt"
  wVeldtTest `shouldBe` True

testParsingVaykor_Hek :: SpecWith ()
testParsingVaykor_Hek =
  it "Checks if Vaykor_Hek can be parsed successfully" $ do
    wVaykor_HekTest <- checkIfParsed "Vaykor_Hek"
    wVaykor_HekTest `shouldBe` True

testParsingHind :: SpecWith ()
testParsingHind = it "Checks if Hind can be parsed successfully" $ do
  wHindTest <- checkIfParsed "Hind"
  wHindTest `shouldBe` True

testParsingBraton :: SpecWith ()
testParsingBraton = it "Checks if Braton can be parsed successfully" $ do
  wBratonTest <- checkIfParsed "Braton"
  wBratonTest `shouldBe` True

testParsingStrun :: SpecWith ()
testParsingStrun = it "Checks if Strun can be parsed successfully" $ do
  wStrunTest <- checkIfParsed "Strun"
  wStrunTest `shouldBe` True

testParsingTiberon_Prime :: SpecWith ()
testParsingTiberon_Prime =
  it "Checks if Tiberon_Prime can be parsed successfully" $ do
    wTiberon_PrimeTest <- checkIfParsed "Tiberon_Prime"
    wTiberon_PrimeTest `shouldBe` True

testParsingBoar_Prime :: SpecWith ()
testParsingBoar_Prime =
  it "Checks if Boar_Prime can be parsed successfully" $ do
    wBoar_PrimeTest <- checkIfParsed "Boar_Prime"
    wBoar_PrimeTest `shouldBe` True

testParsingStradavar :: SpecWith ()
testParsingStradavar = it "Checks if Stradavar can be parsed successfully" $ do
  wStradavarTest <- checkIfParsed "Stradavar"
  wStradavarTest `shouldBe` True

testParsingBattacor :: SpecWith ()
testParsingBattacor = it "Checks if Battacor can be parsed successfully" $ do
  wBattacorTest <- checkIfParsed "Battacor"
  wBattacorTest `shouldBe` True

testParsingPhage :: SpecWith ()
testParsingPhage = it "Checks if Phage can be parsed successfully" $ do
  wPhageTest <- checkIfParsed "Phage"
  wPhageTest `shouldBe` True

testParsingTigris :: SpecWith ()
testParsingTigris = it "Checks if Tigris can be parsed successfully" $ do
  wTigrisTest <- checkIfParsed "Tigris"
  wTigrisTest `shouldBe` True

listOfPrimaryWeaponNames :: IO [FilePath]
listOfPrimaryWeaponNames =
  listDirectory "warframe-autobuilder-data/Primary_Weapons/"

checkIfParsed :: String -> IO Bool
checkIfParsed weapon = isRight <$> tryParsingWeapon weapon

tryParsingWeapon :: FilePath -> IO (Either String ComprehensiveWeapon)
tryParsingWeapon weaponName = do
  parsedWeapon <- readWeapon
    ("warframe-autobuilder-data/Primary_Weapons/" ++ weaponName)

  return (makeComprehensive (Just 1) . (, []) <$> parsedWeapon)
