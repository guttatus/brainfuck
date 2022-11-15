import Bf (BfCommand (..), bfPareser)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = do
  testBfParser

testBfParser :: IO ()
testBfParser = hspec $ do
  describe "test for bfParser" $ do
    it "without comment" $ do
      bfPareser ">[-]<[->+<]" `shouldBe` [GoRight, LoopLeft, Dec, LoopRight, GoLeft, LoopLeft, Dec, GoRight, Inc, GoLeft, LoopRight]
    it "with comment" $ do
      bfPareser ">a[-]p<[->+<]" `shouldBe` [GoRight, LoopLeft, Dec, LoopRight, GoLeft, LoopLeft, Dec, GoRight, Inc, GoLeft, LoopRight]