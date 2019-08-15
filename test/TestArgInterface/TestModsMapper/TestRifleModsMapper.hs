{-# LANGUAGE OverloadedStrings #-}

module TestArgInterface.TestModsMapper.TestRifleModsMapper where

import           ArgInterface.ModsMapper.RifleModsMapper
import           Test.Hspec

testRifleModsMapper :: SpecWith ()
testRifleModsMapper =
  it
      "Corrects a rifle-mod's name and returns best match from rifleModsMetaList fuzzylist"
    $ do

  -- | Damage Mods
        bestMatch "amalgamSerration" `shouldBe` Just "amalgamSerration"
        bestMatch "serration" `shouldBe` Just "serration"
        bestMatch "vileAcceleration" `shouldBe` Just "vileAcceleration"

 -- | Multishot Mods
        bestMatch "splitChamber" `shouldBe` Just "splitChamber"
        bestMatch "vigilanteArmaments" `shouldBe` Just "vigilanteArmaments"

 -- | Critical Chance Mods
        bestMatch "pointStrike" `shouldBe` Just "pointStrike"
        bestMatch "argonScope" `shouldBe` Just "argonScope"
        bestMatch "criticalDelay" `shouldBe` Just "criticalDelay"

 -- | Critical Multiplier Mods
        bestMatch "vitalSense" `shouldBe` Just "vitalSense"
        bestMatch "bladedRounds" `shouldBe` Just "bladedRounds"
        bestMatch "hammerShot" `shouldBe` Just "hammerShot"

 -- | Special Mods
        bestMatch "metalAuger" `shouldBe` Just "metalAuger"
        bestMatch "shred" `shouldBe` Just "shred"
        bestMatch "catalyzerLink" `shouldBe` Just "catalyzerLink"
        bestMatch "rifleAptitude" `shouldBe` Just "rifleAptitude"
        bestMatch "primedShred" `shouldBe` Just "primedShred"
        bestMatch "hunterMunitions" `shouldBe` Just "hunterMunitions"
        bestMatch "hunterTrack" `shouldBe` Just "hunterTrack"
        bestMatch "continuousMisery" `shouldBe` Just "continuousMisery"

 -- | Fire Rate Mods
        bestMatch "springLoadedChamber" `shouldBe` Just "springLoadedChamber"
        bestMatch "speedTrigger" `shouldBe` Just "speedTrigger"

 -- | Magazine Mods
        bestMatch "taintedMag" `shouldBe` Just "taintedMag"

 -- | General Info Mods
        bestMatch "ammoDrum" `shouldBe` Just "ammoDrum"

 -- | Physical Damage Mods
        bestMatch "piercingCaliber" `shouldBe` Just "piercingCaliber"
        bestMatch "piercingHit" `shouldBe` Just "piercingHit"
        bestMatch "fangedFusillade" `shouldBe` Just "fangedFusillade"
        bestMatch "sawtoothClip" `shouldBe` Just "sawtoothClip"
        bestMatch "crashCourse" `shouldBe` Just "crashCourse"
        bestMatch "rupture" `shouldBe` Just "rupture"

 -- | Elemental Damage Mods
        bestMatch "cryoRounds" `shouldBe` Just "cryoRounds"
        bestMatch "primedCryoRounds" `shouldBe` Just "primedCryoRounds"
        bestMatch "hellfire" `shouldBe` Just "hellfire"
        bestMatch "primedHellfire" `shouldBe` Just "primedHellfire"
        bestMatch "infectedClip" `shouldBe` Just "infectedClip"
        bestMatch "primedInfectedClip" `shouldBe` Just "primedInfectedClip"
        bestMatch "stormbringer" `shouldBe` Just "stormbringer"
        bestMatch "primedStormbringer" `shouldBe` Just "primedStormbringer"
        bestMatch "rimeRounds" `shouldBe` Just "rimeRounds"
        bestMatch "thermiteRounds" `shouldBe` Just "thermiteRounds"
        bestMatch "malignantForce" `shouldBe` Just "malignantForce"
        bestMatch "highVoltage" `shouldBe` Just "highVoltage"
        bestMatch "wildfire" `shouldBe` Just "wildfire"
