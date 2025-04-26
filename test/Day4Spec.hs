{-# LANGUAGE ImportQualifiedPost #-}

module Day4Spec (spec) where

import Data.IntMap.Strict qualified as IntMap
import Day4
import Test.Hspec

one :: [[Int]]
one = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

two :: [[Int]]
two = [[1, 20, 30], [4, 50, 60], [7, 80, 90]]

bcOne = newCard one

bcTwo = newCard two

spec :: Spec
spec = do
  describe "Day 4" $ do
    describe "buildMap" $ do
      it "basic 3x3" $
        cardPoints 1 one
          `shouldBe` [ (1, CardPoint {cardId = 1, row = 0, col = 0, number = 1}),
                       (2, CardPoint {cardId = 1, row = 0, col = 1, number = 2}),
                       (3, CardPoint {cardId = 1, row = 0, col = 2, number = 3}),
                       (4, CardPoint {cardId = 1, row = 1, col = 0, number = 4}),
                       (5, CardPoint {cardId = 1, row = 1, col = 1, number = 5}),
                       (6, CardPoint {cardId = 1, row = 1, col = 2, number = 6}),
                       (7, CardPoint {cardId = 1, row = 2, col = 0, number = 7}),
                       (8, CardPoint {cardId = 1, row = 2, col = 1, number = 8}),
                       (9, CardPoint {cardId = 1, row = 2, col = 2, number = 9})
                     ]
    describe "multiple maps" $ do
      it "does all the maps" $
        cardsMap [one, two]
          `shouldBe` IntMap.fromList
            [ (1, [CardPoint {cardId = 1, row = 0, col = 0, number = 1}, CardPoint {cardId = 0, row = 0, col = 0, number = 1}]),
              (2, [CardPoint {cardId = 0, row = 0, col = 1, number = 2}]),
              (3, [CardPoint {cardId = 0, row = 0, col = 2, number = 3}]),
              (4, [CardPoint {cardId = 1, row = 1, col = 0, number = 4}, CardPoint {cardId = 0, row = 1, col = 0, number = 4}]),
              (5, [CardPoint {cardId = 0, row = 1, col = 1, number = 5}]),
              (6, [CardPoint {cardId = 0, row = 1, col = 2, number = 6}]),
              (7, [CardPoint {cardId = 1, row = 2, col = 0, number = 7}, CardPoint {cardId = 0, row = 2, col = 0, number = 7}]),
              (8, [CardPoint {cardId = 0, row = 2, col = 1, number = 8}]),
              (9, [CardPoint {cardId = 0, row = 2, col = 2, number = 9}]),
              (20, [CardPoint {cardId = 1, row = 0, col = 1, number = 20}]),
              (30, [CardPoint {cardId = 1, row = 0, col = 2, number = 30}]),
              (50, [CardPoint {cardId = 1, row = 1, col = 1, number = 50}]),
              (60, [CardPoint {cardId = 1, row = 1, col = 2, number = 60}]),
              (80, [CardPoint {cardId = 1, row = 2, col = 1, number = 80}]),
              (90, [CardPoint {cardId = 1, row = 2, col = 2, number = 90}])
            ]
    describe "callCard" $ do
      it "adds a number to a card map" $
        callCard CardPoint {cardId = 1, row = 2, col = 0, number = 7} bcTwo
          `shouldBe` bcTwo {vertical = IntMap.fromList [(0, 1)], horizontal = IntMap.fromList [(2, 1)], callHistory = [7]}
    describe "incEntry" $ do
      it "increments" $
        incEntry 0 (IntMap.fromList [(0, 0)]) `shouldBe` IntMap.fromList [(0, 1)]
      it "starts from default 0" $ 
        incEntry 0 IntMap.empty `shouldBe` IntMap.fromList [(0, 1)]
