import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "All working" (it "works" $ 4 + 4 `shouldBe` 8)

main :: IO ()
main = hspec spec
