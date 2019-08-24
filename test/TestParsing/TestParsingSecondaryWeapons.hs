{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestParsing.TestParsingSecondaryWeapons where

import           Test.Hspec
import           ClassyPrelude
import           Types.ComprehensiveWeapon      ( ComprehensiveWeapon )
import           ComprehensiveWeapon.ComprehensiveFunctions
                                                ( makeComprehensive )

import           GenericFunctions.GenericFunctions
                                                ( readWeapon )
import           Data.Either                    ( isRight )
import           System.Directory               ( listDirectory )

testParsingTelos_Akbolto :: SpecWith ()
testParsingTelos_Akbolto =
  it "Checks if Telos_Akbolto file can be parsed successfully" $ do
    wTelos_AkboltoTest <- checkIfParsed "Telos_Akbolto"
    wTelos_AkboltoTest `shouldBe` True

testParsingViper_Wraith :: SpecWith ()
testParsingViper_Wraith =
  it "Check if Viper_Wraith file can be parsed successfully" $ do
    wViper_WraithTest <- checkIfParsed "Viper_Wraith"
    wViper_WraithTest `shouldBe` True

testParsingKitgun_Tombfinger_Haymaker_Splat :: SpecWith ()
testParsingKitgun_Tombfinger_Haymaker_Splat =
  it "Check if Kitgun_Tombfinger_Haymaker_Splat file can be parsed successfully"
    $ do
        wKitgun_Tombfinger_Haymaker_SplatTest <- checkIfParsed
          "Kitgun_Tombfinger_Haymaker_Splat"
        wKitgun_Tombfinger_Haymaker_SplatTest `shouldBe` True

testParsingSweeper_Prime :: SpecWith ()
testParsingSweeper_Prime =
  it "Check if Sweeper_Prime file can be parsed successfully" $ do
    wSweeper_PrimeTest <- checkIfParsed "Sweeper_Prime"
    wSweeper_PrimeTest `shouldBe` True

testParsingDeth_Machine_Rifle :: SpecWith ()
testParsingDeth_Machine_Rifle =
  it "Check if Deth_Machine_Rifle file can be parsed successfully" $ do
    wDeth_Machine_RifleTest <- checkIfParsed "Deth_Machine_Rifle"
    wDeth_Machine_RifleTest `shouldBe` True

testParsingRakta_Ballistica :: SpecWith ()
testParsingRakta_Ballistica =
  it "Check if Rakta_Ballistica file can be parsed successfully" $ do
    wRakta_BallisticaTest <- checkIfParsed "Rakta_Ballistica"
    wRakta_BallisticaTest `shouldBe` True

testParsingPrisma_Angstrum :: SpecWith ()
testParsingPrisma_Angstrum =
  it "Check if Prisma_Angstrum file can be parsed successfully" $ do
    wPrisma_AngstrumTest <- checkIfParsed "Prisma_Angstrum"
    wPrisma_AngstrumTest `shouldBe` True

testParsingAkjagara_Prime :: SpecWith ()
testParsingAkjagara_Prime =
  it "Check if Akjagara_Prime file can be parsed successfully" $ do
    wAkjagara_PrimeTest <- checkIfParsed "Akjagara_Prime"
    wAkjagara_PrimeTest `shouldBe` True

testParsingAkbronco_Prime :: SpecWith ()
testParsingAkbronco_Prime =
  it "Check if Akbronco_Prime file can be parsed successfully" $ do
    wAkbronco_PrimeTest <- checkIfParsed "Akbronco_Prime"
    wAkbronco_PrimeTest `shouldBe` True

testParsingTwin_Gremlins :: SpecWith ()
testParsingTwin_Gremlins =
  it "Check if Twin_Gremlins file can be parsed successfully" $ do
    wTwin_GremlinsTest <- checkIfParsed "Twin_Gremlins"
    wTwin_GremlinsTest `shouldBe` True

testParsingBallistica_Prime :: SpecWith ()
testParsingBallistica_Prime =
  it "Check if Ballistica_Prime file can be parsed successfully" $ do
    wBallistica_PrimeTest <- checkIfParsed "Ballistica_Prime"
    wBallistica_PrimeTest `shouldBe` True

testParsingViper :: SpecWith ()
testParsingViper = it "Check if Viper file can be parsed successfully" $ do
  wViperTest <- checkIfParsed "Viper"
  wViperTest `shouldBe` True

testParsingKunai :: SpecWith ()
testParsingKunai = it "Check if Kunai file can be parsed successfully" $ do
  wKunaiTest <- checkIfParsed "Kunai"
  wKunaiTest `shouldBe` True

testParsingMultron :: SpecWith ()
testParsingMultron = it "Check if Multron file can be parsed successfully" $ do
  wMultronTest <- checkIfParsed "Multron"
  wMultronTest `shouldBe` True

testParsingAkvasto :: SpecWith ()
testParsingAkvasto = it "Check if Akvasto file can be parsed successfully" $ do
  wAkvastoTest <- checkIfParsed "Akvasto"
  wAkvastoTest `shouldBe` True

testParsingAkbronco :: SpecWith ()
testParsingAkbronco =
  it "Check if Akbronco file can be parsed successfully" $ do
    wAkbroncoTest <- checkIfParsed "Akbronco"
    wAkbroncoTest `shouldBe` True

testParsingPyrana_Prime :: SpecWith ()
testParsingPyrana_Prime =
  it "Check if Pyrana_Prime file can be parsed successfully" $ do
    wPyrana_PrimeTest <- checkIfParsed "Pyrana_Prime"
    wPyrana_PrimeTest `shouldBe` True

testParsingOcucor :: SpecWith ()
testParsingOcucor = it "Check if Ocucor file can be parsed successfully" $ do
  wOcucorTest <- checkIfParsed "Ocucor"
  wOcucorTest `shouldBe` True

testParsingAkmagnus :: SpecWith ()
testParsingAkmagnus =
  it "Check if Akmagnus file can be parsed successfully" $ do
    wAkmagnusTest <- checkIfParsed "Akmagnus"
    wAkmagnusTest `shouldBe` True

testParsingStug :: SpecWith ()
testParsingStug = it "Check if Stug file can be parsed successfully" $ do
  wStugTest <- checkIfParsed "Stug"
  wStugTest `shouldBe` True

testParsingSecura_Dual_Cestra :: SpecWith ()
testParsingSecura_Dual_Cestra =
  it "Check if Secura_Dual_Cestra file can be parsed successfully" $ do
    wSecura_Dual_CestraTest <- checkIfParsed "Secura_Dual_Cestra"
    wSecura_Dual_CestraTest `shouldBe` True

testParsingKraken :: SpecWith ()
testParsingKraken = it "Check if Kraken file can be parsed successfully" $ do
  wKrakenTest <- checkIfParsed "Kraken"
  wKrakenTest `shouldBe` True

testParsingAksomati :: SpecWith ()
testParsingAksomati =
  it "Check if Aksomati file can be parsed successfully" $ do
    wAksomatiTest <- checkIfParsed "Aksomati"
    wAksomatiTest `shouldBe` True

testParsingPrisma_Twin_Gremlins :: SpecWith ()
testParsingPrisma_Twin_Gremlins =
  it "Check if Prisma_Twin_Gremlins file can be parsed successfully" $ do
    wPrisma_Twin_GremlinsTest <- checkIfParsed "Prisma_Twin_Gremlins"
    wPrisma_Twin_GremlinsTest `shouldBe` True

testParsingBallistica :: SpecWith ()
testParsingBallistica =
  it "Check if Ballistica file can be parsed successfully" $ do
    wBallisticaTest <- checkIfParsed "Ballistica"
    wBallisticaTest `shouldBe` True

testParsingCastanas :: SpecWith ()
testParsingCastanas =
  it "Check if Castanas file can be parsed successfully" $ do
    wCastanasTest <- checkIfParsed "Castanas"
    wCastanasTest `shouldBe` True

testParsingTwin_Rogga :: SpecWith ()
testParsingTwin_Rogga =
  it "Check if Twin_Rogga file can be parsed successfully" $ do
    wTwin_RoggaTest <- checkIfParsed "Twin_Rogga"
    wTwin_RoggaTest `shouldBe` True

testParsingTazicor :: SpecWith ()
testParsingTazicor = it "Check if Tazicor file can be parsed successfully" $ do
  wTazicorTest <- checkIfParsed "Tazicor"
  wTazicorTest `shouldBe` True

testParsingKnell :: SpecWith ()
testParsingKnell = it "Check if Knell file can be parsed successfully" $ do
  wKnellTest <- checkIfParsed "Knell"
  wKnellTest `shouldBe` True

testParsingBrakk :: SpecWith ()
testParsingBrakk = it "Check if Brakk file can be parsed successfully" $ do
  wBrakkTest <- checkIfParsed "Brakk"
  wBrakkTest `shouldBe` True

testParsingPandero :: SpecWith ()
testParsingPandero = it "Check if Pandero file can be parsed successfully" $ do
  wPanderoTest <- checkIfParsed "Pandero"
  wPanderoTest `shouldBe` True

testParsingAklex :: SpecWith ()
testParsingAklex = it "Check if Aklex file can be parsed successfully" $ do
  wAklexTest <- checkIfParsed "Aklex"
  wAklexTest `shouldBe` True

testParsingSicarus_Prime :: SpecWith ()
testParsingSicarus_Prime =
  it "Check if Sicarus_Prime file can be parsed successfully" $ do
    wSicarus_PrimeTest <- checkIfParsed "Sicarus_Prime"
    wSicarus_PrimeTest `shouldBe` True

testParsingMarelok :: SpecWith ()
testParsingMarelok = it "Check if Marelok file can be parsed successfully" $ do
  wMarelokTest <- checkIfParsed "Marelok"
  wMarelokTest `shouldBe` True

testParsingAfuris :: SpecWith ()
testParsingAfuris = it "Check if Afuris file can be parsed successfully" $ do
  wAfurisTest <- checkIfParsed "Afuris"
  wAfurisTest `shouldBe` True

testParsingSeer :: SpecWith ()
testParsingSeer = it "Check if Seer file can be parsed successfully" $ do
  wSeerTest <- checkIfParsed "Seer"
  wSeerTest `shouldBe` True

testParsingVulcax :: SpecWith ()
testParsingVulcax = it "Check if Vulcax file can be parsed successfully" $ do
  wVulcaxTest <- checkIfParsed "Vulcax"
  wVulcaxTest `shouldBe` True

testParsingDual_Cestra :: SpecWith ()
testParsingDual_Cestra =
  it "Check if Dual_Cestra file can be parsed successfully" $ do
    wDual_CestraTest <- checkIfParsed "Dual_Cestra"
    wDual_CestraTest `shouldBe` True

testParsingAkstiletto_Prime :: SpecWith ()
testParsingAkstiletto_Prime =
  it "Check if Akstiletto_Prime file can be parsed successfully" $ do
    wAkstiletto_PrimeTest <- checkIfParsed "Akstiletto_Prime"
    wAkstiletto_PrimeTest `shouldBe` True

testParsingVasto_Prime :: SpecWith ()
testParsingVasto_Prime =
  it "Check if Vasto_Prime file can be parsed successfully" $ do
    wVasto_PrimeTest <- checkIfParsed "Vasto_Prime"
    wVasto_PrimeTest `shouldBe` True

testParsingPox :: SpecWith ()
testParsingPox = it "Check if Pox file can be parsed successfully" $ do
  wPoxTest <- checkIfParsed "Pox"
  wPoxTest `shouldBe` True

testParsingAkjagara :: SpecWith ()
testParsingAkjagara =
  it "Check if Akjagara file can be parsed successfully" $ do
    wAkjagaraTest <- checkIfParsed "Akjagara"
    wAkjagaraTest `shouldBe` True

testParsingLato_Vandal :: SpecWith ()
testParsingLato_Vandal =
  it "Check if Lato_Vandal file can be parsed successfully" $ do
    wLato_VandalTest <- checkIfParsed "Lato_Vandal"
    wLato_VandalTest `shouldBe` True

testParsingHikou_Prime :: SpecWith ()
testParsingHikou_Prime =
  it "Check if Hikou_Prime file can be parsed successfully" $ do
    wHikou_PrimeTest <- checkIfParsed "Hikou_Prime"
    wHikou_PrimeTest `shouldBe` True

testParsingLex_Prime :: SpecWith ()
testParsingLex_Prime =
  it "Check if Lex_Prime file can be parsed successfully" $ do
    wLex_PrimeTest <- checkIfParsed "Lex_Prime"
    wLex_PrimeTest `shouldBe` True

testParsingPrisma_Burst_Laser :: SpecWith ()
testParsingPrisma_Burst_Laser =
  it "Check if Prisma_Burst_Laser file can be parsed successfully" $ do
    wPrisma_Burst_LaserTest <- checkIfParsed "Prisma_Burst_Laser"
    wPrisma_Burst_LaserTest `shouldBe` True

testParsingZylok :: SpecWith ()
testParsingZylok = it "Check if Zylok file can be parsed successfully" $ do
  wZylokTest <- checkIfParsed "Zylok"
  wZylokTest `shouldBe` True

testParsingBolto :: SpecWith ()
testParsingBolto = it "Check if Bolto file can be parsed successfully" $ do
  wBoltoTest <- checkIfParsed "Bolto"
  wBoltoTest `shouldBe` True

testParsingAklex_Prime :: SpecWith ()
testParsingAklex_Prime =
  it "Check if Aklex_Prime file can be parsed successfully" $ do
    wAklex_PrimeTest <- checkIfParsed "Aklex_Prime"
    wAklex_PrimeTest `shouldBe` True

testParsingKulstar :: SpecWith ()
testParsingKulstar = it "Check if Kulstar file can be parsed successfully" $ do
  wKulstarTest <- checkIfParsed "Kulstar"
  wKulstarTest `shouldBe` True

testParsingDespair :: SpecWith ()
testParsingDespair = it "Check if Despair file can be parsed successfully" $ do
  wDespairTest <- checkIfParsed "Despair"
  wDespairTest `shouldBe` True

testParsingTwin_Kohmak :: SpecWith ()
testParsingTwin_Kohmak =
  it "Check if Twin_Kohmak file can be parsed successfully" $ do
    wTwin_KohmakTest <- checkIfParsed "Twin_Kohmak"
    wTwin_KohmakTest `shouldBe` True

testParsingMk1Furis :: SpecWith ()
testParsingMk1Furis =
  it "Check if Mk1-Furis file can be parsed successfully" $ do
    wMk1FurisTest <- checkIfParsed "Mk1-Furis"
    wMk1FurisTest `shouldBe` True

testParsingSpira_Prime :: SpecWith ()
testParsingSpira_Prime =
  it "Check if Spira_Prime file can be parsed successfully" $ do
    wSpira_PrimeTest <- checkIfParsed "Spira_Prime"
    wSpira_PrimeTest `shouldBe` True

testParsingTwin_Vipers :: SpecWith ()
testParsingTwin_Vipers =
  it "Check if Twin_Vipers file can be parsed successfully" $ do
    wTwin_VipersTest <- checkIfParsed "Twin_Vipers"
    wTwin_VipersTest `shouldBe` True

testParsingFuris :: SpecWith ()
testParsingFuris = it "Check if Furis file can be parsed successfully" $ do
  wFurisTest <- checkIfParsed "Furis"
  wFurisTest `shouldBe` True

testParsingPyrana :: SpecWith ()
testParsingPyrana = it "Check if Pyrana file can be parsed successfully" $ do
  wPyranaTest <- checkIfParsed "Pyrana"
  wPyranaTest `shouldBe` True

testParsingZakti :: SpecWith ()
testParsingZakti = it "Check if Zakti file can be parsed successfully" $ do
  wZaktiTest <- checkIfParsed "Zakti"
  wZaktiTest `shouldBe` True

testParsingLato_Prime :: SpecWith ()
testParsingLato_Prime =
  it "Check if Lato_Prime file can be parsed successfully" $ do
    wLato_PrimeTest <- checkIfParsed "Lato_Prime"
    wLato_PrimeTest `shouldBe` True

testParsingDeconstructor :: SpecWith ()
testParsingDeconstructor =
  it "Check if Deconstructor file can be parsed successfully" $ do
    wDeconstructorTest <- checkIfParsed "Deconstructor"
    wDeconstructorTest `shouldBe` True

testParsingSicarus :: SpecWith ()
testParsingSicarus = it "Check if Sicarus file can be parsed successfully" $ do
  wSicarusTest <- checkIfParsed "Sicarus"
  wSicarusTest `shouldBe` True

testParsingLaser_Rifle :: SpecWith ()
testParsingLaser_Rifle =
  it "Check if Laser_Rifle file can be parsed successfully" $ do
    wLaser_RifleTest <- checkIfParsed "Laser_Rifle"
    wLaser_RifleTest `shouldBe` True

testParsingGammacor :: SpecWith ()
testParsingGammacor =
  it "Check if Gammacor file can be parsed successfully" $ do
    wGammacorTest <- checkIfParsed "Gammacor"
    wGammacorTest `shouldBe` True

testParsingAklato :: SpecWith ()
testParsingAklato = it "Check if Aklato file can be parsed successfully" $ do
  wAklatoTest <- checkIfParsed "Aklato"
  wAklatoTest `shouldBe` True

testParsingMk1Kunai :: SpecWith ()
testParsingMk1Kunai =
  it "Check if Mk1-Kunai file can be parsed successfully" $ do
    wMk1KunaiTest <- checkIfParsed "Mk1-Kunai"
    wMk1KunaiTest `shouldBe` True

testParsingAcrid :: SpecWith ()
testParsingAcrid = it "Check if Acrid file can be parsed successfully" $ do
  wAcridTest <- checkIfParsed "Acrid"
  wAcridTest `shouldBe` True

testParsingSynoid_Gammacor :: SpecWith ()
testParsingSynoid_Gammacor =
  it "Check if Synoid_Gammacor file can be parsed successfully" $ do
    wSynoid_GammacorTest <- checkIfParsed "Synoid_Gammacor"
    wSynoid_GammacorTest `shouldBe` True

testParsingLex :: SpecWith ()
testParsingLex = it "Check if Lex file can be parsed successfully" $ do
  wLexTest <- checkIfParsed "Lex"
  wLexTest `shouldBe` True

testParsingBronco_Prime :: SpecWith ()
testParsingBronco_Prime =
  it "Check if Bronco_Prime file can be parsed successfully" $ do
    wBronco_PrimeTest <- checkIfParsed "Bronco_Prime"
    wBronco_PrimeTest `shouldBe` True

testParsingVasto :: SpecWith ()
testParsingVasto = it "Check if Vasto file can be parsed successfully" $ do
  wVastoTest <- checkIfParsed "Vasto"
  wVastoTest `shouldBe` True

testParsingTysis :: SpecWith ()
testParsingTysis = it "Check if Tysis file can be parsed successfully" $ do
  wTysisTest <- checkIfParsed "Tysis"
  wTysisTest `shouldBe` True

testParsingLato :: SpecWith ()
testParsingLato = it "Check if Lato file can be parsed successfully" $ do
  wLatoTest <- checkIfParsed "Lato"
  wLatoTest `shouldBe` True

testParsingBronco :: SpecWith ()
testParsingBronco = it "Check if Bronco file can be parsed successfully" $ do
  wBroncoTest <- checkIfParsed "Bronco"
  wBroncoTest `shouldBe` True

testParsingEmbolist :: SpecWith ()
testParsingEmbolist =
  it "Check if Embolist file can be parsed successfully" $ do
    wEmbolistTest <- checkIfParsed "Embolist"
    wEmbolistTest `shouldBe` True

testParsingArtax :: SpecWith ()
testParsingArtax = it "Check if Artax file can be parsed successfully" $ do
  wArtaxTest <- checkIfParsed "Artax"
  wArtaxTest `shouldBe` True

testParsingCryotra :: SpecWith ()
testParsingCryotra = it "Check if Cryotra file can be parsed successfully" $ do
  wCryotraTest <- checkIfParsed "Cryotra"
  wCryotraTest `shouldBe` True

testParsingAkvasto_Prime :: SpecWith ()
testParsingAkvasto_Prime =
  it "Check if Akvasto_Prime file can be parsed successfully" $ do
    wAkvasto_PrimeTest <- checkIfParsed "Akvasto_Prime"
    wAkvasto_PrimeTest `shouldBe` True

testParsingArca_Scisco :: SpecWith ()
testParsingArca_Scisco =
  it "Check if Arca_Scisco file can be parsed successfully" $ do
    wArca_SciscoTest <- checkIfParsed "Arca_Scisco"
    wArca_SciscoTest `shouldBe` True

testParsingAzima :: SpecWith ()
testParsingAzima = it "Check if Azima file can be parsed successfully" $ do
  wAzimaTest <- checkIfParsed "Azima"
  wAzimaTest `shouldBe` True

testParsingAtomos :: SpecWith ()
testParsingAtomos = it "Check if Atomos file can be parsed successfully" $ do
  wAtomosTest <- checkIfParsed "Atomos"
  wAtomosTest `shouldBe` True

testParsingAkbolto_Prime :: SpecWith ()
testParsingAkbolto_Prime =
  it "Check if Akbolto_Prime file can be parsed successfully" $ do
    wAkbolto_PrimeTest <- checkIfParsed "Akbolto_Prime"
    wAkbolto_PrimeTest `shouldBe` True

testParsingDual_Toxocyst :: SpecWith ()
testParsingDual_Toxocyst =
  it "Check if Dual_Toxocyst file can be parsed successfully" $ do
    wDual_ToxocystTest <- checkIfParsed "Dual_Toxocyst"
    wDual_ToxocystTest `shouldBe` True

testParsingSpira :: SpecWith ()
testParsingSpira = it "Check if Spira file can be parsed successfully" $ do
  wSpiraTest <- checkIfParsed "Spira"
  wSpiraTest `shouldBe` True

testParsingVaykor_Marelok :: SpecWith ()
testParsingVaykor_Marelok =
  it "Check if Vaykor_Marelok file can be parsed successfully" $ do
    wVaykor_MarelokTest <- checkIfParsed "Vaykor_Marelok"
    wVaykor_MarelokTest `shouldBe` True

testParsingAngstrum :: SpecWith ()
testParsingAngstrum =
  it "Check if Angstrum file can be parsed successfully" $ do
    wAngstrumTest <- checkIfParsed "Angstrum"
    wAngstrumTest `shouldBe` True

testParsingTwin_Vipers_Wraith :: SpecWith ()
testParsingTwin_Vipers_Wraith =
  it "Check if Twin_Vipers_Wraith file can be parsed successfully" $ do
    wTwin_Vipers_WraithTest <- checkIfParsed "Twin_Vipers_Wraith"
    wTwin_Vipers_WraithTest `shouldBe` True

testParsingHikou :: SpecWith ()
testParsingHikou = it "Check if Hikou file can be parsed successfully" $ do
  wHikouTest <- checkIfParsed "Hikou"
  wHikouTest `shouldBe` True

testParsingMagnus :: SpecWith ()
testParsingMagnus = it "Check if Magnus file can be parsed successfully" $ do
  wMagnusTest <- checkIfParsed "Magnus"
  wMagnusTest `shouldBe` True

testParsingStubba :: SpecWith ()
testParsingStubba = it "Check if Stubba file can be parsed successfully" $ do
  wStubbaTest <- checkIfParsed "Stubba"
  wStubbaTest `shouldBe` True

testParsingPrime_Laser_Rifle :: SpecWith ()
testParsingPrime_Laser_Rifle =
  it "Check if Prime_Laser_Rifle file can be parsed successfully" $ do
    wPrime_Laser_RifleTest <- checkIfParsed "Prime_Laser_Rifle"
    wPrime_Laser_RifleTest `shouldBe` True

testParsingVulklok :: SpecWith ()
testParsingVulklok = it "Check if Vulklok file can be parsed successfully" $ do
  wVulklokTest <- checkIfParsed "Vulklok"
  wVulklokTest `shouldBe` True

testParsingDex_Furis :: SpecWith ()
testParsingDex_Furis =
  it "Check if Dex_Furis file can be parsed successfully" $ do
    wDex_FurisTest <- checkIfParsed "Dex_Furis"
    wDex_FurisTest `shouldBe` True

testParsingDetron :: SpecWith ()
testParsingDetron = it "Check if Detron file can be parsed successfully" $ do
  wDetronTest <- checkIfParsed "Detron"
  wDetronTest `shouldBe` True

testParsingSonicor :: SpecWith ()
testParsingSonicor = it "Check if Sonicor file can be parsed successfully" $ do
  wSonicorTest <- checkIfParsed "Sonicor"
  wSonicorTest `shouldBe` True

testParsingTalons :: SpecWith ()
testParsingTalons = it "Check if Talons file can be parsed successfully" $ do
  wTalonsTest <- checkIfParsed "Talons"
  wTalonsTest `shouldBe` True

testParsingCycron :: SpecWith ()
testParsingCycron = it "Check if Cycron file can be parsed successfully" $ do
  wCycronTest <- checkIfParsed "Cycron"
  wCycronTest `shouldBe` True

testParsingAkstiletto :: SpecWith ()
testParsingAkstiletto =
  it "Check if Akstiletto file can be parsed successfully" $ do
    wAkstilettoTest <- checkIfParsed "Akstiletto"
    wAkstilettoTest `shouldBe` True

testParsingAkbolto :: SpecWith ()
testParsingAkbolto = it "Check if Akbolto file can be parsed successfully" $ do
  wAkboltoTest <- checkIfParsed "Akbolto"
  wAkboltoTest `shouldBe` True

testParsingKohmak :: SpecWith ()
testParsingKohmak = it "Check if Kohmak file can be parsed successfully" $ do
  wKohmakTest <- checkIfParsed "Kohmak"
  wKohmakTest `shouldBe` True

testParsingPlinx :: SpecWith ()
testParsingPlinx = it "Check if Plinx file can be parsed successfully" $ do
  wPlinxTest <- checkIfParsed "Plinx"
  wPlinxTest `shouldBe` True

testParsingAkzani :: SpecWith ()
testParsingAkzani = it "Check if Akzani file can be parsed successfully" $ do
  wAkzaniTest <- checkIfParsed "Akzani"
  wAkzaniTest `shouldBe` True

testParsingHystrix :: SpecWith ()
testParsingHystrix = it "Check if Hystrix file can be parsed successfully" $ do
  wHystrixTest <- checkIfParsed "Hystrix"
  wHystrixTest `shouldBe` True

testParsingNukor :: SpecWith ()
testParsingNukor = it "Check if Nukor file can be parsed successfully" $ do
  wNukorTest <- checkIfParsed "Nukor"
  wNukorTest `shouldBe` True

testParsingStaticor :: SpecWith ()
testParsingStaticor =
  it "Check if Staticor file can be parsed successfully" $ do
    wStaticorTest <- checkIfParsed "Staticor"
    wStaticorTest `shouldBe` True

testParsingBurst_Laser :: SpecWith ()
testParsingBurst_Laser =
  it "Check if Burst_Laser file can be parsed successfully" $ do
    wBurst_LaserTest <- checkIfParsed "Burst_Laser"
    wBurst_LaserTest `shouldBe` True

testParsingCestra :: SpecWith ()
testParsingCestra = it "Check if Cestra file can be parsed successfully" $ do
  wCestraTest <- checkIfParsed "Cestra"
  wCestraTest `shouldBe` True

testParsingFusilai :: SpecWith ()
testParsingFusilai = it "Check if Fusilai file can be parsed successfully" $ do
  wFusilaiTest <- checkIfParsed "Fusilai"
  wFusilaiTest `shouldBe` True

testParsingStinger :: SpecWith ()
testParsingStinger = it "Check if Stinger file can be parsed successfully" $ do
  wStingerTest <- checkIfParsed "Stinger"
  wStingerTest `shouldBe` True

testParsingTwin_Grakatas :: SpecWith ()
testParsingTwin_Grakatas =
  it "Check if Twin_Grakatas file can be parsed successfully" $ do
    wTwin_GrakatasTest <- checkIfParsed "Twin_Grakatas"
    wTwin_GrakatasTest `shouldBe` True

testParsingDeconstructor_Prime :: SpecWith ()
testParsingDeconstructor_Prime =
  it "Check if Deconstructor_Prime file can be parsed successfully" $ do
    wDeconstructor_PrimeTest <- checkIfParsed "Deconstructor_Prime"
    wDeconstructor_PrimeTest `shouldBe` True

testParsingEuphona_Prime :: SpecWith ()
testParsingEuphona_Prime =
  it "Check if Euphona_Prime file can be parsed successfully" $ do
    wEuphona_PrimeTest <- checkIfParsed "Euphona_Prime"
    wEuphona_PrimeTest `shouldBe` True

testParsingSweeper :: SpecWith ()
testParsingSweeper = it "Check if Sweeper file can be parsed successfully" $ do
  wSweeperTest <- checkIfParsed "Sweeper"
  wSweeperTest `shouldBe` True

testParsingSpectra :: SpecWith ()
testParsingSpectra = it "Check if Spectra file can be parsed successfully" $ do
  wSpectraTest <- checkIfParsed "Spectra"
  wSpectraTest `shouldBe` True

testParsingSancti_Castanas :: SpecWith ()
testParsingSancti_Castanas =
  it "Check if Sancti_Castanas file can be parsed successfully" $ do
    wSancti_CastanasTest <- checkIfParsed "Sancti_Castanas"
    wSancti_CastanasTest `shouldBe` True

testParsingMara_Detron :: SpecWith ()
testParsingMara_Detron =
  it "Check if Mara_Detron file can be parsed successfully" $ do
    wMara_DetronTest <- checkIfParsed "Mara_Detron"
    wMara_DetronTest `shouldBe` True

listOfSecondaryWeaponNames :: IO [FilePath]
listOfSecondaryWeaponNames =
  listDirectory "warframe-autobuilder-data/Secondary_Weapons/"

checkIfParsed :: String -> IO Bool
checkIfParsed weapon = isRight <$> tryParsingWeapon weapon

tryParsingWeapon :: FilePath -> IO (Either String ComprehensiveWeapon)
tryParsingWeapon weaponName = do
  parsedWeapon <- readWeapon
    ("warframe-autobuilder-data/Secondary_Weapons/" ++ weaponName)

  return (makeComprehensive (Just 1) . (, []) <$> parsedWeapon)
