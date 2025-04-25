{-# LANGUAGE BinaryLiterals #-}

module Day3Spec (spec) where
import Test.Hspec
import Day3

spec :: Spec
spec =  do
  describe "Day 3" $ do
    describe "chooseDigit" $ do
      it "" $
        map (chooseDigit 5) [1,2,3,4,5] `shouldBe` [0,0,1,1,1]

    describe "toBin" $ do
      it "list to number as binary" $
        toBin [0,1,1] `shouldBe` 3
      it "full" $
        toBin [1,1,1,1] `shouldBe` 15
      it "order" $
        toBin [1,1,0,1] `shouldBe` 13
      it "empty" $
        toBin [0,0,0] `shouldBe` 0

    describe "part1" $ do
      it "all 1" $
        part1 [[1,1,1,1], [1,1,1,1],[1,1,1,1],[1,1,1,1]] `shouldBe` 0
      it "all 0" $
        part1 [[0,0,0,0], [0,0,0,0],[0,0,0,0],[0,0,0,0]] `shouldBe` 0
      it "mixed" $
        part1 [[1,1,0,0], [1,1,0,0],[1,1,0,0],[1,1,0,0]] `shouldBe` 0b1100 * 0b0011
      it "mixed commutative" $
        part1 [[0,0,1,1], [0,0,1,1],[0,0,1,1],[0,0,1,1]] `shouldBe` 0b1100 * 0b0011
      it "example" $
        part1 [[0,0,1,0,0],[1,1,1,1,0],[1,0,1,1,0],[1,0,1,1,1],[1,0,1,0,1],[0,1,1,1,1],[0,0,1,1,1],[1,1,1,0,0],[1,0,0,0,0],[1,1,0,0,1],[0,0,0,1,0],[0,1,0,1,0]] `shouldBe` 198

