import Parser (BfCommand (..), bfPareser, bfSyntaxValid)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = do
  testBfParser
  testSyntaxValid

testBfParser :: IO ()
testBfParser = hspec $ do
  describe "test for bfParser" $ do
    it "without comment" $ do
      bfPareser ">[-]<[->+<]" `shouldBe` [GoRight, LoopLeft, Dec, LoopRight, GoLeft, LoopLeft, Dec, GoRight, Inc, GoLeft, LoopRight]
    it "with comment" $ do
      bfPareser ">a[-]p<[->+<]" `shouldBe` [GoRight, LoopLeft, Dec, LoopRight, GoLeft, LoopLeft, Dec, GoRight, Inc, GoLeft, LoopRight]


testSyntaxValid :: IO()
testSyntaxValid = hspec  $ do
    describe "test for syntaxValid" $ do
        it "syntex valid" $ do 
            bfSyntaxValid "[-][[]][]" `shouldBe` True
        it "syntex invalid" $ do
            bfSyntaxValid "[-" `shouldBe` False
            bfSyntaxValid "[]][" `shouldBe` False
            bfSyntaxValid "[[]"  `shouldBe` False