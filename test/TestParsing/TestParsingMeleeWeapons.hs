{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestParsing.TestParsingMeleeWeapons where

import           Test.Hspec
import           ClassyPrelude
import           Types.ComprehensiveWeapon      ( ComprehensiveWeapon )
import           ComprehensiveWeapon.ComprehensiveFunctions
                                                ( makeComprehensive )

import           GenericFunctions.GenericFunctions
                                                ( readWeapon )
import           Data.Either                    ( isRight )
import           System.Directory               ( listDirectory )

testParsingDex_Dakra :: SpecWith ()
testParsingDex_Dakra =
  it "Checks if Dex_Dakra file can be parsed successfully" $ do
    wDex_DakraTest <- checkIfParsed "Dex_Dakra"
    wDex_DakraTest `shouldBe` True

testParsingGalatine_Prime :: SpecWith ()
testParsingGalatine_Prime =
  it "Checks if Galatine_Prime can be parsed successfully" $ do
    wGalatine_PrimeTest <- checkIfParsed "Galatine_Prime"
    wGalatine_PrimeTest `shouldBe` True

testParsingAnku :: SpecWith ()
testParsingAnku = it "Checks if Anku can be parsed successfully" $ do
  wAnkuTest <- checkIfParsed "Anku"
  wAnkuTest `shouldBe` True

testParsingAnkyros_Prime :: SpecWith ()
testParsingAnkyros_Prime =
  it "Checks if Ankyros_Prime can be parsed successfully" $ do
    wAnkyros_PrimeTest <- checkIfParsed "Ankyros_Prime"
    wAnkyros_PrimeTest `shouldBe` True

testParsingSynoid_Heliocor :: SpecWith ()
testParsingSynoid_Heliocor =
  it "Checks if Synoid_Heliocor can be parsed successfully" $ do
    wSynoid_HeliocorTest <- checkIfParsed "Synoid_Heliocor"
    wSynoid_HeliocorTest `shouldBe` True

testParsingLecta :: SpecWith ()
testParsingLecta = it "Checks if Lecta can be parsed successfully" $ do
  wLectaTest <- checkIfParsed "Lecta"
  wLectaTest `shouldBe` True

testParsingDual_Kamas :: SpecWith ()
testParsingDual_Kamas =
  it "Checks if Dual_Kamas can be parsed successfully" $ do
    wDual_KamasTest <- checkIfParsed "Dual_Kamas"
    wDual_KamasTest `shouldBe` True

testParsingFalcor :: SpecWith ()
testParsingFalcor = it "Checks if Falcor can be parsed successfully" $ do
  wFalcorTest <- checkIfParsed "Falcor"
  wFalcorTest `shouldBe` True

testParsingPrisma_Dual_Cleavers :: SpecWith ()
testParsingPrisma_Dual_Cleavers =
  it "Checks if Prisma_Dual_Cleavers can be parsed successfully" $ do
    wPrisma_Dual_CleaversTest <- checkIfParsed "Prisma_Dual_Cleavers"
    wPrisma_Dual_CleaversTest `shouldBe` True

testParsingRedeemer :: SpecWith ()
testParsingRedeemer = it "Checks if Redeemer can be parsed successfully" $ do
  wRedeemerTest <- checkIfParsed "Redeemer"
  wRedeemerTest `shouldBe` True

testParsingAmphis :: SpecWith ()
testParsingAmphis = it "Checks if Amphis can be parsed successfully" $ do
  wAmphisTest <- checkIfParsed "Amphis"
  wAmphisTest `shouldBe` True

testParsingJaw_Sword :: SpecWith ()
testParsingJaw_Sword = it "Checks if Jaw_Sword can be parsed successfully" $ do
  wJaw_SwordTest <- checkIfParsed "Jaw_Sword"
  wJaw_SwordTest `shouldBe` True

testParsingKronen_Prime :: SpecWith ()
testParsingKronen_Prime =
  it "Checks if Kronen_Prime can be parsed successfully" $ do
    wKronen_PrimeTest <- checkIfParsed "Kronen_Prime"
    wKronen_PrimeTest `shouldBe` True

testParsingTelos_Boltace :: SpecWith ()
testParsingTelos_Boltace =
  it "Checks if Telos_Boltace can be parsed successfully" $ do
    wTelos_BoltaceTest <- checkIfParsed "Telos_Boltace"
    wTelos_BoltaceTest `shouldBe` True

testParsingProva :: SpecWith ()
testParsingProva = it "Checks if Prova can be parsed successfully" $ do
  wProvaTest <- checkIfParsed "Prova"
  wProvaTest `shouldBe` True

testParsingHeat_Dagger :: SpecWith ()
testParsingHeat_Dagger =
  it "Checks if Heat_Dagger can be parsed successfully" $ do
    wHeat_DaggerTest <- checkIfParsed "Heat_Dagger"
    wHeat_DaggerTest `shouldBe` True

testParsingDestreza :: SpecWith ()
testParsingDestreza = it "Checks if Destreza can be parsed successfully" $ do
  wDestrezaTest <- checkIfParsed "Destreza"
  wDestrezaTest `shouldBe` True

testParsingTekko :: SpecWith ()
testParsingTekko = it "Checks if Tekko can be parsed successfully" $ do
  wTekkoTest <- checkIfParsed "Tekko"
  wTekkoTest `shouldBe` True

testParsingAnkyros :: SpecWith ()
testParsingAnkyros = it "Checks if Ankyros can be parsed successfully" $ do
  wAnkyrosTest <- checkIfParsed "Ankyros"
  wAnkyrosTest `shouldBe` True

testParsingDestreza_Prime :: SpecWith ()
testParsingDestreza_Prime =
  it "Checks if Destreza_Prime can be parsed successfully" $ do
    wDestreza_PrimeTest <- checkIfParsed "Destreza_Prime"
    wDestreza_PrimeTest `shouldBe` True

testParsingDual_Kamas_Prime :: SpecWith ()
testParsingDual_Kamas_Prime =
  it "Checks if Dual_Kamas_Prime can be parsed successfully" $ do
    wDual_Kamas_PrimeTest <- checkIfParsed "Dual_Kamas_Prime"
    wDual_Kamas_PrimeTest `shouldBe` True

testParsingAckAndBrunt :: SpecWith ()
testParsingAckAndBrunt =
  it "Checks if Ack_&_Brunt can be parsed successfully" $ do
    wAckAndBruntTest <- checkIfParsed "Ack_&_Brunt"
    wAckAndBruntTest `shouldBe` True

testParsingNami_Skyla_Prime :: SpecWith ()
testParsingNami_Skyla_Prime =
  it "Checks if Nami_Skyla_Prime can be parsed successfully" $ do
    wNami_Skyla_PrimeTest <- checkIfParsed "Nami_Skyla_Prime"
    wNami_Skyla_PrimeTest `shouldBe` True

testParsingMire :: SpecWith ()
testParsingMire = it "Checks if Mire can be parsed successfully" $ do
  wMireTest <- checkIfParsed "Mire"
  wMireTest `shouldBe` True

testParsingKorrudo :: SpecWith ()
testParsingKorrudo = it "Checks if Korrudo can be parsed successfully" $ do
  wKorrudoTest <- checkIfParsed "Korrudo"
  wKorrudoTest `shouldBe` True

testParsingJat_Kittag :: SpecWith ()
testParsingJat_Kittag =
  it "Checks if Jat_Kittag can be parsed successfully" $ do
    wJat_KittagTest <- checkIfParsed "Jat_Kittag"
    wJat_KittagTest `shouldBe` True

testParsingSydon :: SpecWith ()
testParsingSydon = it "Checks if Sydon can be parsed successfully" $ do
  wSydonTest <- checkIfParsed "Sydon"
  wSydonTest `shouldBe` True

testParsingTonbo :: SpecWith ()
testParsingTonbo = it "Checks if Tonbo can be parsed successfully" $ do
  wTonboTest <- checkIfParsed "Tonbo"
  wTonboTest `shouldBe` True

testParsingNinkondi :: SpecWith ()
testParsingNinkondi = it "Checks if Ninkondi can be parsed successfully" $ do
  wNinkondiTest <- checkIfParsed "Ninkondi"
  wNinkondiTest `shouldBe` True

testParsingNami_Skyla :: SpecWith ()
testParsingNami_Skyla =
  it "Checks if Nami_Skyla can be parsed successfully" $ do
    wNami_SkylaTest <- checkIfParsed "Nami_Skyla"
    wNami_SkylaTest `shouldBe` True

testParsingFurax_Wraith :: SpecWith ()
testParsingFurax_Wraith =
  it "Checks if Furax_Wraith can be parsed successfully" $ do
    wFurax_WraithTest <- checkIfParsed "Furax_Wraith"
    wFurax_WraithTest `shouldBe` True

testParsingEndura :: SpecWith ()
testParsingEndura = it "Checks if Endura can be parsed successfully" $ do
  wEnduraTest <- checkIfParsed "Endura"
  wEnduraTest `shouldBe` True

testParsingBo :: SpecWith ()
testParsingBo = it "Checks if Bo can be parsed successfully" $ do
  wBoTest <- checkIfParsed "Bo"
  wBoTest `shouldBe` True

testParsingPlague_KripathShtungEkwana_Jai_II :: SpecWith ()
testParsingPlague_KripathShtungEkwana_Jai_II =
  it "Checks if Plague_Kripath+Shtung+Ekwana_Jai_II can be parsed successfully"
    $ do
        wPlague_KripathShtungEkwana_Jai_IITest <- checkIfParsed
          "Plague_Kripath+Shtung+Ekwana_Jai_II"
        wPlague_KripathShtungEkwana_Jai_IITest `shouldBe` True

testParsingRedeemer_Prime :: SpecWith ()
testParsingRedeemer_Prime =
  it "Checks if Redeemer_Prime can be parsed successfully" $ do
    wRedeemer_PrimeTest <- checkIfParsed "Redeemer_Prime"
    wRedeemer_PrimeTest `shouldBe` True

testParsingCaustacyst :: SpecWith ()
testParsingCaustacyst =
  it "Checks if Caustacyst can be parsed successfully" $ do
    wCaustacystTest <- checkIfParsed "Caustacyst"
    wCaustacystTest `shouldBe` True

testParsingTatsu :: SpecWith ()
testParsingTatsu = it "Checks if Tatsu can be parsed successfully" $ do
  wTatsuTest <- checkIfParsed "Tatsu"
  wTatsuTest `shouldBe` True

testParsingTipedo :: SpecWith ()
testParsingTipedo = it "Checks if Tipedo can be parsed successfully" $ do
  wTipedoTest <- checkIfParsed "Tipedo"
  wTipedoTest `shouldBe` True

testParsingReaper_Prime :: SpecWith ()
testParsingReaper_Prime =
  it "Checks if Reaper_Prime can be parsed successfully" $ do
    wReaper_PrimeTest <- checkIfParsed "Reaper_Prime"
    wReaper_PrimeTest `shouldBe` True

testParsingJat_Kusar :: SpecWith ()
testParsingJat_Kusar = it "Checks if Jat_Kusar can be parsed successfully" $ do
  wJat_KusarTest <- checkIfParsed "Jat_Kusar"
  wJat_KusarTest `shouldBe` True

testParsingDual_Zoren :: SpecWith ()
testParsingDual_Zoren =
  it "Checks if Dual_Zoren can be parsed successfully" $ do
    wDual_ZorenTest <- checkIfParsed "Dual_Zoren"
    wDual_ZorenTest `shouldBe` True

testParsingEther_Daggers :: SpecWith ()
testParsingEther_Daggers =
  it "Checks if Ether_Daggers can be parsed successfully" $ do
    wEther_DaggersTest <- checkIfParsed "Ether_Daggers"
    wEther_DaggersTest `shouldBe` True

testParsingOrthos :: SpecWith ()
testParsingOrthos = it "Checks if Orthos can be parsed successfully" $ do
  wOrthosTest <- checkIfParsed "Orthos"
  wOrthosTest `shouldBe` True

testParsingShaku :: SpecWith ()
testParsingShaku = it "Checks if Shaku can be parsed successfully" $ do
  wShakuTest <- checkIfParsed "Shaku"
  wShakuTest `shouldBe` True

testParsingMk1Bo :: SpecWith ()
testParsingMk1Bo = it "Checks if Mk1-Bo can be parsed successfully" $ do
  wMk1BoTest <- checkIfParsed "Mk1-Bo"
  wMk1BoTest `shouldBe` True

testParsingFang_Prime :: SpecWith ()
testParsingFang_Prime =
  it "Checks if Fang_Prime can be parsed successfully" $ do
    wFang_PrimeTest <- checkIfParsed "Fang_Prime"
    wFang_PrimeTest `shouldBe` True

testParsingGuandao :: SpecWith ()
testParsingGuandao = it "Checks if Guandao can be parsed successfully" $ do
  wGuandaoTest <- checkIfParsed "Guandao"
  wGuandaoTest `shouldBe` True

testParsingSerro :: SpecWith ()
testParsingSerro = it "Checks if Serro can be parsed successfully" $ do
  wSerroTest <- checkIfParsed "Serro"
  wSerroTest `shouldBe` True

testParsingKaryst :: SpecWith ()
testParsingKaryst = it "Checks if Karyst can be parsed successfully" $ do
  wKarystTest <- checkIfParsed "Karyst"
  wKarystTest `shouldBe` True

testParsingScindo_Prime :: SpecWith ()
testParsingScindo_Prime =
  it "Checks if Scindo_Prime can be parsed successfully" $ do
    wScindo_PrimeTest <- checkIfParsed "Scindo_Prime"
    wScindo_PrimeTest `shouldBe` True

testParsingGram_Prime :: SpecWith ()
testParsingGram_Prime =
  it "Checks if Gram_Prime can be parsed successfully" $ do
    wGram_PrimeTest <- checkIfParsed "Gram_Prime"
    wGram_PrimeTest `shouldBe` True

testParsingRakta_Dark_Dagger :: SpecWith ()
testParsingRakta_Dark_Dagger =
  it "Checks if Rakta_Dark_Dagger can be parsed successfully" $ do
    wRakta_Dark_DaggerTest <- checkIfParsed "Rakta_Dark_Dagger"
    wRakta_Dark_DaggerTest `shouldBe` True

testParsingSheev :: SpecWith ()
testParsingSheev = it "Checks if Sheev can be parsed successfully" $ do
  wSheevTest <- checkIfParsed "Sheev"
  wSheevTest `shouldBe` True

testParsingDual_Skana :: SpecWith ()
testParsingDual_Skana =
  it "Checks if Dual_Skana can be parsed successfully" $ do
    wDual_SkanaTest <- checkIfParsed "Dual_Skana"
    wDual_SkanaTest `shouldBe` True

testParsingGunsen :: SpecWith ()
testParsingGunsen = it "Checks if Gunsen can be parsed successfully" $ do
  wGunsenTest <- checkIfParsed "Gunsen"
  wGunsenTest `shouldBe` True

testParsingFragor :: SpecWith ()
testParsingFragor = it "Checks if Fragor can be parsed successfully" $ do
  wFragorTest <- checkIfParsed "Fragor"
  wFragorTest `shouldBe` True

testParsingBo_Prime :: SpecWith ()
testParsingBo_Prime = it "Checks if Bo_Prime can be parsed successfully" $ do
  wBo_PrimeTest <- checkIfParsed "Bo_Prime"
  wBo_PrimeTest `shouldBe` True

testParsingDual_Heat_Swords :: SpecWith ()
testParsingDual_Heat_Swords =
  it "Checks if Dual_Heat_Swords can be parsed successfully" $ do
    wDual_Heat_SwordsTest <- checkIfParsed "Dual_Heat_Swords"
    wDual_Heat_SwordsTest `shouldBe` True

testParsingVenka :: SpecWith ()
testParsingVenka = it "Checks if Venka can be parsed successfully" $ do
  wVenkaTest <- checkIfParsed "Venka"
  wVenkaTest `shouldBe` True

testParsingLesion :: SpecWith ()
testParsingLesion = it "Checks if Lesion can be parsed successfully" $ do
  wLesionTest <- checkIfParsed "Lesion"
  wLesionTest `shouldBe` True

testParsingDual_Cleavers :: SpecWith ()
testParsingDual_Cleavers =
  it "Checks if Dual_Cleavers can be parsed successfully" $ do
    wDual_CleaversTest <- checkIfParsed "Dual_Cleavers"
    wDual_CleaversTest `shouldBe` True

testParsingSkana_Prime :: SpecWith ()
testParsingSkana_Prime =
  it "Checks if Skana_Prime can be parsed successfully" $ do
    wSkana_PrimeTest <- checkIfParsed "Skana_Prime"
    wSkana_PrimeTest `shouldBe` True

testParsingOrvius :: SpecWith ()
testParsingOrvius = it "Checks if Orvius can be parsed successfully" $ do
  wOrviusTest <- checkIfParsed "Orvius"
  wOrviusTest `shouldBe` True

testParsingEther_Sword :: SpecWith ()
testParsingEther_Sword =
  it "Checks if Ether_Sword can be parsed successfully" $ do
    wEther_SwordTest <- checkIfParsed "Ether_Sword"
    wEther_SwordTest `shouldBe` True

testParsingCassowar :: SpecWith ()
testParsingCassowar = it "Checks if Cassowar can be parsed successfully" $ do
  wCassowarTest <- checkIfParsed "Cassowar"
  wCassowarTest `shouldBe` True

testParsingAtterax :: SpecWith ()
testParsingAtterax = it "Checks if Atterax can be parsed successfully" $ do
  wAtteraxTest <- checkIfParsed "Atterax"
  wAtteraxTest `shouldBe` True

testParsingPlasma_Sword :: SpecWith ()
testParsingPlasma_Sword =
  it "Checks if Plasma_Sword can be parsed successfully" $ do
    wPlasma_SwordTest <- checkIfParsed "Plasma_Sword"
    wPlasma_SwordTest `shouldBe` True

testParsingFragor_Prime :: SpecWith ()
testParsingFragor_Prime =
  it "Checks if Fragor_Prime can be parsed successfully" $ do
    wFragor_PrimeTest <- checkIfParsed "Fragor_Prime"
    wFragor_PrimeTest `shouldBe` True

testParsingWar :: SpecWith ()
testParsingWar = it "Checks if War can be parsed successfully" $ do
  wWarTest <- checkIfParsed "War"
  wWarTest `shouldBe` True

testParsingGlaive_Prime :: SpecWith ()
testParsingGlaive_Prime =
  it "Checks if Glaive_Prime can be parsed successfully" $ do
    wGlaive_PrimeTest <- checkIfParsed "Glaive_Prime"
    wGlaive_PrimeTest `shouldBe` True

testParsingSarpa :: SpecWith ()
testParsingSarpa = it "Checks if Sarpa can be parsed successfully" $ do
  wSarpaTest <- checkIfParsed "Sarpa"
  wSarpaTest `shouldBe` True

testParsingCeramic_Dagger :: SpecWith ()
testParsingCeramic_Dagger =
  it "Checks if Ceramic_Dagger can be parsed successfully" $ do
    wCeramic_DaggerTest <- checkIfParsed "Ceramic_Dagger"
    wCeramic_DaggerTest `shouldBe` True

testParsingBroken_War :: SpecWith ()
testParsingBroken_War =
  it "Checks if Broken_War can be parsed successfully" $ do
    wBroken_WarTest <- checkIfParsed "Broken_War"
    wBroken_WarTest `shouldBe` True

testParsingHeat_Sword :: SpecWith ()
testParsingHeat_Sword =
  it "Checks if Heat_Sword can be parsed successfully" $ do
    wHeat_SwordTest <- checkIfParsed "Heat_Sword"
    wHeat_SwordTest `shouldBe` True

testParsingEther_Reaper :: SpecWith ()
testParsingEther_Reaper =
  it "Checks if Ether_Reaper can be parsed successfully" $ do
    wEther_ReaperTest <- checkIfParsed "Ether_Reaper"
    wEther_ReaperTest `shouldBe` True

testParsingMachete :: SpecWith ()
testParsingMachete = it "Checks if Machete can be parsed successfully" $ do
  wMacheteTest <- checkIfParsed "Machete"
  wMacheteTest `shouldBe` True

testParsingNikana :: SpecWith ()
testParsingNikana = it "Checks if Nikana can be parsed successfully" $ do
  wNikanaTest <- checkIfParsed "Nikana"
  wNikanaTest `shouldBe` True

testParsingGalvacord :: SpecWith ()
testParsingGalvacord = it "Checks if Galvacord can be parsed successfully" $ do
  wGalvacordTest <- checkIfParsed "Galvacord"
  wGalvacordTest `shouldBe` True

testParsingCobraAndCrane :: SpecWith ()
testParsingCobraAndCrane =
  it "Checks if Cobra_&_Crane can be parsed successfully" $ do
    wCobraAndCraneTest <- checkIfParsed "Cobra_&_Crane"
    wCobraAndCraneTest `shouldBe` True

testParsingBroken_Scepter :: SpecWith ()
testParsingBroken_Scepter =
  it "Checks if Broken_Scepter can be parsed successfully" $ do
    wBroken_ScepterTest <- checkIfParsed "Broken_Scepter"
    wBroken_ScepterTest `shouldBe` True

testParsingParacesis :: SpecWith ()
testParsingParacesis = it "Checks if Paracesis can be parsed successfully" $ do
  wParacesisTest <- checkIfParsed "Paracesis"
  wParacesisTest `shouldBe` True

testParsingKesheg :: SpecWith ()
testParsingKesheg = it "Checks if Kesheg can be parsed successfully" $ do
  wKeshegTest <- checkIfParsed "Kesheg"
  wKeshegTest `shouldBe` True

testParsingDark_Sword :: SpecWith ()
testParsingDark_Sword =
  it "Checks if Dark_Sword can be parsed successfully" $ do
    wDark_SwordTest <- checkIfParsed "Dark_Sword"
    wDark_SwordTest `shouldBe` True

testParsingFurax :: SpecWith ()
testParsingFurax = it "Checks if Furax can be parsed successfully" $ do
  wFuraxTest <- checkIfParsed "Furax"
  wFuraxTest `shouldBe` True

testParsingFang :: SpecWith ()
testParsingFang = it "Checks if Fang can be parsed successfully" $ do
  wFangTest <- checkIfParsed "Fang"
  wFangTest `shouldBe` True

testParsingTwin_Basolk :: SpecWith ()
testParsingTwin_Basolk =
  it "Checks if Twin_Basolk can be parsed successfully" $ do
    wTwin_BasolkTest <- checkIfParsed "Twin_Basolk"
    wTwin_BasolkTest `shouldBe` True

testParsingDragon_Nikana :: SpecWith ()
testParsingDragon_Nikana =
  it "Checks if Dragon_Nikana can be parsed successfully" $ do
    wDragon_NikanaTest <- checkIfParsed "Dragon_Nikana"
    wDragon_NikanaTest `shouldBe` True

testParsingPupacyst :: SpecWith ()
testParsingPupacyst = it "Checks if Pupacyst can be parsed successfully" $ do
  wPupacystTest <- checkIfParsed "Pupacyst"
  wPupacystTest `shouldBe` True

testParsingSigmaAndOctantis :: SpecWith ()
testParsingSigmaAndOctantis =
  it "Checks if Sigma_&_Octantis can be parsed successfully" $ do
    wSigmaAndOctantisTest <- checkIfParsed "Sigma_&_Octantis"
    wSigmaAndOctantisTest `shouldBe` True

testParsingPangolin_Sword :: SpecWith ()
testParsingPangolin_Sword =
  it "Checks if Pangolin_Sword can be parsed successfully" $ do
    wPangolin_SwordTest <- checkIfParsed "Pangolin_Sword"
    wPangolin_SwordTest `shouldBe` True

testParsingSancti_Magistar :: SpecWith ()
testParsingSancti_Magistar =
  it "Checks if Sancti_Magistar can be parsed successfully" $ do
    wSancti_MagistarTest <- checkIfParsed "Sancti_Magistar"
    wSancti_MagistarTest `shouldBe` True

testParsingHate :: SpecWith ()
testParsingHate = it "Checks if Hate can be parsed successfully" $ do
  wHateTest <- checkIfParsed "Hate"
  wHateTest `shouldBe` True

testParsingKama :: SpecWith ()
testParsingKama = it "Checks if Kama can be parsed successfully" $ do
  wKamaTest <- checkIfParsed "Kama"
  wKamaTest `shouldBe` True

testParsingGlaive :: SpecWith ()
testParsingGlaive = it "Checks if Glaive can be parsed successfully" $ do
  wGlaiveTest <- checkIfParsed "Glaive"
  wGlaiveTest `shouldBe` True

testParsingKestrel :: SpecWith ()
testParsingKestrel = it "Checks if Kestrel can be parsed successfully" $ do
  wKestrelTest <- checkIfParsed "Kestrel"
  wKestrelTest `shouldBe` True

testParsingBoltace :: SpecWith ()
testParsingBoltace = it "Checks if Boltace can be parsed successfully" $ do
  wBoltaceTest <- checkIfParsed "Boltace"
  wBoltaceTest `shouldBe` True

testParsingMagistar :: SpecWith ()
testParsingMagistar = it "Checks if Magistar can be parsed successfully" $ do
  wMagistarTest <- checkIfParsed "Magistar"
  wMagistarTest `shouldBe` True

testParsingNikana_Prime :: SpecWith ()
testParsingNikana_Prime =
  it "Checks if Nikana_Prime can be parsed successfully" $ do
    wNikana_PrimeTest <- checkIfParsed "Nikana_Prime"
    wNikana_PrimeTest `shouldBe` True

testParsingSkiajati :: SpecWith ()
testParsingSkiajati = it "Checks if Skiajati can be parsed successfully" $ do
  wSkiajatiTest <- checkIfParsed "Skiajati"
  wSkiajatiTest `shouldBe` True

testParsingTwin_Krohkur :: SpecWith ()
testParsingTwin_Krohkur =
  it "Checks if Twin_Krohkur can be parsed successfully" $ do
    wTwin_KrohkurTest <- checkIfParsed "Twin_Krohkur"
    wTwin_KrohkurTest `shouldBe` True

testParsingGazal_Machete :: SpecWith ()
testParsingGazal_Machete =
  it "Checks if Gazal_Machete can be parsed successfully" $ do
    wGazal_MacheteTest <- checkIfParsed "Gazal_Machete"
    wGazal_MacheteTest `shouldBe` True

testParsingPrisma_Skana :: SpecWith ()
testParsingPrisma_Skana =
  it "Checks if Prisma_Skana can be parsed successfully" $ do
    wPrisma_SkanaTest <- checkIfParsed "Prisma_Skana"
    wPrisma_SkanaTest `shouldBe` True

testParsingKogake_Prime :: SpecWith ()
testParsingKogake_Prime =
  it "Checks if Kogake_Prime can be parsed successfully" $ do
    wKogake_PrimeTest <- checkIfParsed "Kogake_Prime"
    wKogake_PrimeTest `shouldBe` True

testParsingHirudo :: SpecWith ()
testParsingHirudo = it "Checks if Hirudo can be parsed successfully" $ do
  wHirudoTest <- checkIfParsed "Hirudo"
  wHirudoTest `shouldBe` True

testParsingVenka_Prime :: SpecWith ()
testParsingVenka_Prime =
  it "Checks if Venka_Prime can be parsed successfully" $ do
    wVenka_PrimeTest <- checkIfParsed "Venka_Prime"
    wVenka_PrimeTest `shouldBe` True

testParsingMk1Furax :: SpecWith ()
testParsingMk1Furax = it "Checks if Mk1-Furax can be parsed successfully" $ do
  wMk1FuraxTest <- checkIfParsed "Mk1-Furax"
  wMk1FuraxTest `shouldBe` True

testParsingHalikar :: SpecWith ()
testParsingHalikar = it "Checks if Halikar can be parsed successfully" $ do
  wHalikarTest <- checkIfParsed "Halikar"
  wHalikarTest `shouldBe` True

testParsingSibear :: SpecWith ()
testParsingSibear = it "Checks if Sibear can be parsed successfully" $ do
  wSibearTest <- checkIfParsed "Sibear"
  wSibearTest `shouldBe` True

testParsingWolf_Sledge :: SpecWith ()
testParsingWolf_Sledge =
  it "Checks if Wolf_Sledge can be parsed successfully" $ do
    wWolf_SledgeTest <- checkIfParsed "Wolf_Sledge"
    wWolf_SledgeTest `shouldBe` True

testParsingScindo :: SpecWith ()
testParsingScindo = it "Checks if Scindo can be parsed successfully" $ do
  wScindoTest <- checkIfParsed "Scindo"
  wScindoTest `shouldBe` True

testParsingLacera :: SpecWith ()
testParsingLacera = it "Checks if Lacera can be parsed successfully" $ do
  wLaceraTest <- checkIfParsed "Lacera"
  wLaceraTest `shouldBe` True

testParsingDark_SplitSword :: SpecWith ()
testParsingDark_SplitSword =
  it "Checks if Dark_Split-Sword can be parsed successfully" $ do
    wDark_SplitSwordTest <- checkIfParsed "Dark_Split-Sword"
    wDark_SplitSwordTest `shouldBe` True

testParsingKronen :: SpecWith ()
testParsingKronen = it "Checks if Kronen can be parsed successfully" $ do
  wKronenTest <- checkIfParsed "Kronen"
  wKronenTest `shouldBe` True

testParsingObex :: SpecWith ()
testParsingObex = it "Checks if Obex can be parsed successfully" $ do
  wObexTest <- checkIfParsed "Obex"
  wObexTest `shouldBe` True

testParsingMios :: SpecWith ()
testParsingMios = it "Checks if Mios can be parsed successfully" $ do
  wMiosTest <- checkIfParsed "Mios"
  wMiosTest `shouldBe` True

testParsingDual_Raza :: SpecWith ()
testParsingDual_Raza = it "Checks if Dual_Raza can be parsed successfully" $ do
  wDual_RazaTest <- checkIfParsed "Dual_Raza"
  wDual_RazaTest `shouldBe` True

testParsingCerata :: SpecWith ()
testParsingCerata = it "Checks if Cerata can be parsed successfully" $ do
  wCerataTest <- checkIfParsed "Cerata"
  wCerataTest `shouldBe` True

testParsingRipkas :: SpecWith ()
testParsingRipkas = it "Checks if Ripkas can be parsed successfully" $ do
  wRipkasTest <- checkIfParsed "Ripkas"
  wRipkasTest `shouldBe` True

testParsingVolnus :: SpecWith ()
testParsingVolnus = it "Checks if Volnus can be parsed successfully" $ do
  wVolnusTest <- checkIfParsed "Volnus"
  wVolnusTest `shouldBe` True

testParsingGalatine :: SpecWith ()
testParsingGalatine = it "Checks if Galatine can be parsed successfully" $ do
  wGalatineTest <- checkIfParsed "Galatine"
  wGalatineTest `shouldBe` True

testParsingNami_Solo :: SpecWith ()
testParsingNami_Solo = it "Checks if Nami_Solo can be parsed successfully" $ do
  wNami_SoloTest <- checkIfParsed "Nami_Solo"
  wNami_SoloTest `shouldBe` True

testParsingTipedo_Prime :: SpecWith ()
testParsingTipedo_Prime =
  it "Checks if Tipedo_Prime can be parsed successfully" $ do
    wTipedo_PrimeTest <- checkIfParsed "Tipedo_Prime"
    wTipedo_PrimeTest `shouldBe` True

testParsingOrthos_Prime :: SpecWith ()
testParsingOrthos_Prime =
  it "Checks if Orthos_Prime can be parsed successfully" $ do
    wOrthos_PrimeTest <- checkIfParsed "Orthos_Prime"
    wOrthos_PrimeTest `shouldBe` True

testParsingOhma :: SpecWith ()
testParsingOhma = it "Checks if Ohma can be parsed successfully" $ do
  wOhmaTest <- checkIfParsed "Ohma"
  wOhmaTest `shouldBe` True

testParsingPrisma_Obex :: SpecWith ()
testParsingPrisma_Obex =
  it "Checks if Prisma_Obex can be parsed successfully" $ do
    wPrisma_ObexTest <- checkIfParsed "Prisma_Obex"
    wPrisma_ObexTest `shouldBe` True

testParsingArca_Titron :: SpecWith ()
testParsingArca_Titron =
  it "Checks if Arca_Titron can be parsed successfully" $ do
    wArca_TitronTest <- checkIfParsed "Arca_Titron"
    wArca_TitronTest `shouldBe` True

testParsingKreska :: SpecWith ()
testParsingKreska = it "Checks if Kreska can be parsed successfully" $ do
  wKreskaTest <- checkIfParsed "Kreska"
  wKreskaTest `shouldBe` True

testParsingOkina :: SpecWith ()
testParsingOkina = it "Checks if Okina can be parsed successfully" $ do
  wOkinaTest <- checkIfParsed "Okina"
  wOkinaTest `shouldBe` True

testParsingKrohkur :: SpecWith ()
testParsingKrohkur = it "Checks if Krohkur can be parsed successfully" $ do
  wKrohkurTest <- checkIfParsed "Krohkur"
  wKrohkurTest `shouldBe` True

testParsingSecura_Lecta :: SpecWith ()
testParsingSecura_Lecta =
  it "Checks if Secura_Lecta can be parsed successfully" $ do
    wSecura_LectaTest <- checkIfParsed "Secura_Lecta"
    wSecura_LectaTest `shouldBe` True

testParsingLandslide :: SpecWith ()
testParsingLandslide = it "Checks if Landslide can be parsed successfully" $ do
  wLandslideTest <- checkIfParsed "Landslide"
  wLandslideTest `shouldBe` True

testParsingProva_Vandal :: SpecWith ()
testParsingProva_Vandal =
  it "Checks if Prova_Vandal can be parsed successfully" $ do
    wProva_VandalTest <- checkIfParsed "Prova_Vandal"
    wProva_VandalTest `shouldBe` True

testParsingSkana :: SpecWith ()
testParsingSkana = it "Checks if Skana can be parsed successfully" $ do
  wSkanaTest <- checkIfParsed "Skana"
  wSkanaTest `shouldBe` True

testParsingPrisma_Machete :: SpecWith ()
testParsingPrisma_Machete =
  it "Checks if Prisma_Machete can be parsed successfully" $ do
    wPrisma_MacheteTest <- checkIfParsed "Prisma_Machete"
    wPrisma_MacheteTest `shouldBe` True

testParsingDark_Dagger :: SpecWith ()
testParsingDark_Dagger =
  it "Checks if Dark_Dagger can be parsed successfully" $ do
    wDark_DaggerTest <- checkIfParsed "Dark_Dagger"
    wDark_DaggerTest `shouldBe` True

testParsingVaykor_Sydon :: SpecWith ()
testParsingVaykor_Sydon =
  it "Checks if Vaykor_Sydon can be parsed successfully" $ do
    wVaykor_SydonTest <- checkIfParsed "Vaykor_Sydon"
    wVaykor_SydonTest `shouldBe` True

testParsingSilvaAndAegis :: SpecWith ()
testParsingSilvaAndAegis =
  it "Checks if Silva_&_Aegis can be parsed successfully" $ do
    wSilvaAndAegisTest <- checkIfParsed "Silva_&_Aegis"
    wSilvaAndAegisTest `shouldBe` True

testParsingZenistar :: SpecWith ()
testParsingZenistar = it "Checks if Zenistar can be parsed successfully" $ do
  wZenistarTest <- checkIfParsed "Zenistar"
  wZenistarTest `shouldBe` True

testParsingKogake :: SpecWith ()
testParsingKogake = it "Checks if Kogake can be parsed successfully" $ do
  wKogakeTest <- checkIfParsed "Kogake"
  wKogakeTest `shouldBe` True

testParsingScoliac :: SpecWith ()
testParsingScoliac = it "Checks if Scoliac can be parsed successfully" $ do
  wScoliacTest <- checkIfParsed "Scoliac"
  wScoliacTest `shouldBe` True

testParsingDual_Keres :: SpecWith ()
testParsingDual_Keres =
  it "Checks if Dual_Keres can be parsed successfully" $ do
    wDual_KeresTest <- checkIfParsed "Dual_Keres"
    wDual_KeresTest `shouldBe` True

testParsingDakra_Prime :: SpecWith ()
testParsingDakra_Prime =
  it "Checks if Dakra_Prime can be parsed successfully" $ do
    wDakra_PrimeTest <- checkIfParsed "Dakra_Prime"
    wDakra_PrimeTest `shouldBe` True

testParsingSilvaAndAegis_Prime :: SpecWith ()
testParsingSilvaAndAegis_Prime =
  it "Checks if Silva_&_Aegis_Prime can be parsed successfully" $ do
    wSilvaAndAegis_PrimeTest <- checkIfParsed "Silva_&_Aegis_Prime"
    wSilvaAndAegis_PrimeTest `shouldBe` True

testParsingHeliocor :: SpecWith ()
testParsingHeliocor = it "Checks if Heliocor can be parsed successfully" $ do
  wHeliocorTest <- checkIfParsed "Heliocor"
  wHeliocorTest `shouldBe` True

testParsingGram :: SpecWith ()
testParsingGram = it "Checks if Gram can be parsed successfully" $ do
  wGramTest <- checkIfParsed "Gram"
  wGramTest `shouldBe` True

testParsingCronus :: SpecWith ()
testParsingCronus = it "Checks if Cronus can be parsed successfully" $ do
  wCronusTest <- checkIfParsed "Cronus"
  wCronusTest `shouldBe` True

testParsingMachete_Wraith :: SpecWith ()
testParsingMachete_Wraith =
  it "Checks if Machete_Wraith can be parsed successfully" $ do
    wMachete_WraithTest <- checkIfParsed "Machete_Wraith"
    wMachete_WraithTest `shouldBe` True

testParsingDual_Ichor :: SpecWith ()
testParsingDual_Ichor =
  it "Checks if Dual_Ichor can be parsed successfully" $ do
    wDual_IchorTest <- checkIfParsed "Dual_Ichor"
    wDual_IchorTest `shouldBe` True

testParsingDual_Ether :: SpecWith ()
testParsingDual_Ether =
  it "Checks if Dual_Ether can be parsed successfully" $ do
    wDual_EtherTest <- checkIfParsed "Dual_Ether"
    wDual_EtherTest `shouldBe` True

listOfMeleeWeaponNames :: IO [FilePath]
listOfMeleeWeaponNames =
  listDirectory "warframe-autobuilder-data/Melee_Weapons/"

checkIfParsed :: String -> IO Bool
checkIfParsed weapon = isRight <$> tryParsingWeapon weapon

tryParsingWeapon :: FilePath -> IO (Either String ComprehensiveWeapon)
tryParsingWeapon weaponName = do
  parsedWeapon <- readWeapon
    ("warframe-autobuilder-data/Melee_Weapons/" ++ weaponName)

  return (makeComprehensive (Just 1) . (, []) <$> parsedWeapon)
