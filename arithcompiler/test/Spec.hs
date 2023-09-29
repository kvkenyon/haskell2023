import Syntax
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = do
  hspec $ describe "src/Syntax.hs" $ do
    it "Plus before times" $
      prettyPrec 0 R expr1 `shouldBe` "4*(5+2)"
    it "Precedence of minus and times" $
      prettyPrec 0 R expr2 `shouldBe` "44-(7*(1+2)-3)"