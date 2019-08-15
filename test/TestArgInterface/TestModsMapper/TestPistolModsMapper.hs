{-# LANGUAGE OverloadedStrings #-}

module TestArgInterface.TestModsMapper.TestPistolModsMapper where

import           ArgInterface.ModsMapper.PistolModsMapper
import           Test.Hspec

testPistolModsMapper :: SpecWith ()
testPistolModsMapper =
  it
      "Corrects a pistol-mod's name and returns best match from pistolModsMetaList fuzzylist"
    $ do

    -- | Damage Mods
        bestMatch "hollowPoint" `shouldBe` Just "hollowPoint"
        bestMatch "hornetStrike" `shouldBe` Just "hornetStrike"
        bestMatch "augurPact" `shouldBe` Just "augurPact"

        -- | Multishot Mods
        bestMatch "barrelDiffusion" `shouldBe` Just "barrelDiffusion"
        bestMatch "lethalTorrent" `shouldBe` Just "lethalTorrent"
        bestMatch "amalgamBarrelDiffusion"
          `shouldBe` Just "amalgamBarrelDiffusion"

        -- | Ammo Mods
        bestMatch "trickMag" `shouldBe` Just "trickMag"

        -- | Critical Chance Mods
        bestMatch "pistolGambit" `shouldBe` Just "pistolGambit"
        bestMatch "primedPistolGambit" `shouldBe` Just "primedPistolGambit"
        bestMatch "hydraulicCrosshairs" `shouldBe` Just "hydraulicCrosshairs"
        bestMatch "creepingBullseye" `shouldBe` Just "creepingBullseye"

        -- | Critical Multiplier Mods
        bestMatch "primedTargetCracker" `shouldBe` Just "primedTargetCracker"
        bestMatch "targetCracker" `shouldBe` Just "targetCracker"
        bestMatch "sharpenedBullets" `shouldBe` Just "sharpenedBullets"

        -- | Fire Rate Mods
        bestMatch "anemicAgility" `shouldBe` Just "anemicAgility"
        bestMatch "gunslinger" `shouldBe` Just "gunslinger"

        -- | Magazine Size Mods
        bestMatch "pressurizedMagazine" `shouldBe` Just "pressurizedMagazine"
        bestMatch "taintedClip" `shouldBe` Just "taintedClip"
        bestMatch "primedSlipMagazine" `shouldBe` Just "primedSlipMagazine"
        bestMatch "slipMagazine" `shouldBe` Just "slipMagazine"

        -- | Status Mods
        bestMatch "embeddedCatalyzer" `shouldBe` Just "embeddedCatalyzer"
        bestMatch "sureShot" `shouldBe` Just "sureShot"
        bestMatch "stunningSpeed" `shouldBe` Just "stunningSpeed"

        -- | Special Mods
        bestMatch "augurSeeker" `shouldBe` Just "augurSeeker"
        bestMatch "perpetualAgony" `shouldBe` Just "perpetualAgony"
        bestMatch "seeker" `shouldBe` Just "seeker"

        -- | Physical Damage Mods
        bestMatch "bore" `shouldBe` Just "bore"
        bestMatch "noReturn" `shouldBe` Just "noReturn"
        bestMatch "maim" `shouldBe` Just "maim"
        bestMatch "razorShot" `shouldBe` Just "razorShot"
        bestMatch "pummel" `shouldBe` Just "pummel"
        bestMatch "concussionRounds" `shouldBe` Just "concussionRounds"

        -- | Elemental Damage Mods
        bestMatch "deepFreeze" `shouldBe` Just "deepFreeze"
        bestMatch "primedDeepFreeze" `shouldBe` Just "primedDeepFreeze"
        bestMatch "heatedCharge" `shouldBe` Just "heatedCharge"
        bestMatch "primedHeatedCharge" `shouldBe` Just "primedHeatedCharge"
        bestMatch "pathogenRounds" `shouldBe` Just "pathogenRounds"
        bestMatch "primedPathogenRounds" `shouldBe` Just "primedPathogenRounds"
        bestMatch "convulsion" `shouldBe` Just "convulsion"
        bestMatch "primedConvulsion" `shouldBe` Just "primedConvulsion"
        bestMatch "frostbite" `shouldBe` Just "frostbite"
        bestMatch "scorch" `shouldBe` Just "scorch"
        bestMatch "pistolPestilence" `shouldBe` Just "pistolPestilence"
        bestMatch "jolt" `shouldBe` Just "jolt"
        bestMatch "iceStorm" `shouldBe` Just "iceStorm"
