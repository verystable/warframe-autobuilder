
import           Test.Hspec
import           TestArgInterface.TestModsMapper.TestRifleModsMapper
import           TestArgInterface.TestModsMapper.TestShotgunModsMapper
import           TestArgInterface.TestModsMapper.TestMeleeModsMapper
import           TestArgInterface.TestModsMapper.TestPistolModsMapper
import           TestArgInterface.TestWeaponDetailsIdentifier

main :: IO ()
main = hspec $ describe "ArgInterface.ModsMapper.RifleModsMapper" $ do
  testRifleModsMapper
  testPistolModsMapper
  testMeleeModsMapper
  testShotgunModsMapper
  testWeaponDetailsIdentifier
