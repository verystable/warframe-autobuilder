{-# LANGUAGE OverloadedStrings #-}

module TestArgInterface.TestModsMapper.TestMeleeModsMapper where

import           ArgInterface.ModsMapper.MeleeModsMapper
import           Test.Hspec

testMeleeModsMapper :: SpecWith ()
testMeleeModsMapper =
  it
      "Corrects a melee-mod's name and returns best match from meleeModsMetaList fuzzylist"
    $ do

    -- | Damage Mods
        bestMatch "pressurePoint" `shouldBe` Just "pressurePoint"
        bestMatch "primedPressurePoint" `shouldBe` Just "primedPressurePoint"
        bestMatch "sacrificialPressure" `shouldBe` Just "sacrificialPressure"
        bestMatch "spoiledStrike" `shouldBe` Just "spoiledStrike"
        bestMatch "conditionOverload" `shouldBe` Just "conditionOverload"

        -- | Attack Speed Mods
        bestMatch "quickening" `shouldBe` Just "quickening"
        bestMatch "berserker" `shouldBe` Just "berserker"
        bestMatch "fury" `shouldBe` Just "fury"
        bestMatch "primedFury" `shouldBe` Just "primedFury"
        bestMatch "gladiatorVice" `shouldBe` Just "gladiatorVice"

        -- | Critical Chance Mods
        bestMatch "trueSteel" `shouldBe` Just "trueSteel"
        bestMatch "bloodRush" `shouldBe` Just "bloodRush"
        bestMatch "truePunishment" `shouldBe` Just "truePunishment"
        bestMatch "maimingStrike" `shouldBe` Just "maimingStrike"
        bestMatch "sacrificialSteel" `shouldBe` Just "sacrificialSteel"

        -- | Critical Multiplier Mods
        bestMatch "organShatter" `shouldBe` Just "organShatter"
        bestMatch "amalgamOrganShatter" `shouldBe` Just "amalgamOrganShatter"
        bestMatch "gladiatorMight" `shouldBe` Just "gladiatorMight"

        -- | Status Chance Mods
        bestMatch "driftingContact" `shouldBe` Just "driftingContact"
        bestMatch "meleeProwess" `shouldBe` Just "meleeProwess"
        bestMatch "weepingWounds" `shouldBe` Just "weepingWounds"

        -- | Physical Damage Mods
        bestMatch "collisionForce" `shouldBe` Just "collisionForce"
        bestMatch "heavyTrauma" `shouldBe` Just "heavyTrauma"
        bestMatch "primedHeavyTrauma" `shouldBe` Just "primedHeavyTrauma"
        bestMatch "sunderingStrike" `shouldBe` Just "sunderingStrike"
        bestMatch "augerStrike" `shouldBe` Just "augerStrike"
        bestMatch "primedAugerStrike" `shouldBe` Just "primedAugerStrike"
        bestMatch "buzzKill" `shouldBe` Just "buzzKill"
        bestMatch "primedBuzzKill" `shouldBe` Just "primedBuzzKill"
        bestMatch "jaggedEdge" `shouldBe` Just "jaggedEdge"
        bestMatch "rendingStrike" `shouldBe` Just "rendingStrike"

        -- | Elemental Damage Mods
        bestMatch "northWind" `shouldBe` Just "northWind"
        bestMatch "primedNorthWind" `shouldBe` Just "primedNorthWind"
        bestMatch "moltenImpact" `shouldBe` Just "moltenImpact"
        bestMatch "primedMoltenImpact" `shouldBe` Just "primedMoltenImpact"
        bestMatch "feverStrike" `shouldBe` Just "feverStrike"
        bestMatch "primedFeverStrike" `shouldBe` Just "primedFeverStrike"
        bestMatch "shockingTouch" `shouldBe` Just "shockingTouch"
        bestMatch "primedShockingTouch" `shouldBe` Just "primedShockingTouch"
        bestMatch "viciousFrost" `shouldBe` Just "viciousFrost"
        bestMatch "vocanicEdge" `shouldBe` Just "vocanicEdge"
        bestMatch "virulentScourge" `shouldBe` Just "virulentScourge"
        bestMatch "voltaicStrike" `shouldBe` Just "voltaicStrike"
        bestMatch "focusEnergy" `shouldBe` Just "focusEnergy"
