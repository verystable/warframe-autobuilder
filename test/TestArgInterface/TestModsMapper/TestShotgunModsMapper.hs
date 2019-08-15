{-# LANGUAGE OverloadedStrings #-}

module TestArgInterface.TestModsMapper.TestShotgunModsMapper where

import           ArgInterface.ModsMapper.ShotgunModsMapper
import           Test.Hspec

testShotgunModsMapper :: SpecWith ()
testShotgunModsMapper =
  it
      "Corrects a shotgun-mod's name and returns best match from shotgunModsMetaList fuzzylist"
    $ do

    -- | Damage Mods
        bestMatch "pointBlank" `shouldBe` Just "pointBlank"
        bestMatch "primedPointBlank" `shouldBe` Just "primedPointBlank"

          -- | Multishot Mods
        bestMatch "hellsChamber" `shouldBe` Just "hellsChamber"

          -- | Ammo Mods
        bestMatch "ammoStock" `shouldBe` Just "ammoStock"
        bestMatch "shellCompression" `shouldBe` Just "shellCompression"

          -- | Critical Chance Mods
        bestMatch "blunderbuss" `shouldBe` Just "blunderbuss"
        bestMatch "laserSight" `shouldBe` Just "laserSight"
        bestMatch "criticalDeceleration" `shouldBe` Just "criticalDeceleration"

          -- | Critical Multiplier Mods
        bestMatch "primedRavage" `shouldBe` Just "primedRavage"
        bestMatch "ravage" `shouldBe` Just "ravage"
        bestMatch "sharpnelShot" `shouldBe` Just "sharpnelShot"

          -- | Fire Rate Mods
        bestMatch "repeaterClip" `shouldBe` Just "repeaterClip"
        bestMatch "shotgunSpazz" `shouldBe` Just "shotgunSpazz"

          -- | Magazine Size Mods
        bestMatch "burdenedMagazine" `shouldBe` Just "burdenedMagazine"

          -- | Speical Mods
        bestMatch "hunterMunitions" `shouldBe` Just "hunterMunitions"
        bestMatch "hunterTrack" `shouldBe` Just "hunterTrack"
        bestMatch "lingeringTorment" `shouldBe` Just "lingeringTorment"
        bestMatch "seekingForce" `shouldBe` Just "seekingForce"
        bestMatch "seekingFury" `shouldBe` Just "seekingFury"

          -- | Status Mods
        bestMatch "nanoApplicator" `shouldBe` Just "nanoApplicator"
        bestMatch "shotgunSavvy" `shouldBe` Just "shotgunSavvy"

          -- | Physical Damage Mods
        bestMatch "acceleratedBlast" `shouldBe` Just "acceleratedBlast"
        bestMatch "breachLoader" `shouldBe` Just "breachLoader"
        bestMatch "flechette" `shouldBe` Just "flechette"
        bestMatch "sweepingSerration" `shouldBe` Just "sweepingSerration"
        bestMatch "shredder" `shouldBe` Just "shredder"
        bestMatch "fullContact" `shouldBe` Just "fullContact"
        bestMatch "disruptor" `shouldBe` Just "disruptor"

          -- | Elemental Damage Mods
        bestMatch "chillingGrasp" `shouldBe` Just "chillingGrasp"
        bestMatch "primedChillingGrasp" `shouldBe` Just "primedChillingGrasp"
        bestMatch "incendiaryCoat" `shouldBe` Just "incendiaryCoat"
        bestMatch "primedIncendiaryCoat" `shouldBe` Just "primedIncendiaryCoat"
        bestMatch "contagiousSpread" `shouldBe` Just "contagiousSpread"
        bestMatch "primedContagiousSpread"
          `shouldBe` Just "primedContagiousSpread"
        bestMatch "chargedShell" `shouldBe` Just "chargedShell"
        bestMatch "primedChargedShell" `shouldBe` Just "primedChargedShell"
        bestMatch "frigidBlast" `shouldBe` Just "frigidBlast"
        bestMatch "scatteringInferno" `shouldBe` Just "scatteringInferno"
        bestMatch "toxicBarrage" `shouldBe` Just "toxicBarrage"
        bestMatch "shellShock" `shouldBe` Just "shellShock"
        bestMatch "blaze" `shouldBe` Just "blaze"
        bestMatch "chillingReload" `shouldBe` Just "chillingReload"
