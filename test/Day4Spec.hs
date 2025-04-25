{-# LANGUAGE ImportQualifiedPost #-}

module Day4Spec (spec) where

import Data.IntMap.Strict qualified as IntMap
import Day4
import Test.Hspec

one :: [[Int]]
one = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

two :: [[Int]]
two = [[1, 20, 30], [4, 50, 60], [7, 80, 90]]

spec :: Spec
spec = do
  describe "Day 4" $ do
    describe "buildMap" $ do
      it "basic 3x3" $
        cardPoints 1 one
          `shouldBe` [ (1, CardPoint {cardId = 1, row = 0, col = 0}),
                       (2, CardPoint {cardId = 1, row = 0, col = 1}),
                       (3, CardPoint {cardId = 1, row = 0, col = 2}),
                       (4, CardPoint {cardId = 1, row = 1, col = 0}),
                       (5, CardPoint {cardId = 1, row = 1, col = 1}),
                       (6, CardPoint {cardId = 1, row = 1, col = 2}),
                       (7, CardPoint {cardId = 1, row = 2, col = 0}),
                       (8, CardPoint {cardId = 1, row = 2, col = 1}),
                       (9, CardPoint {cardId = 1, row = 2, col = 2})
                     ]
    describe "multiple maps" $ do
      it "does all the maps" $
        cardsMap [one, two]
          `shouldBe` IntMap.fromList
            [ (1, [CardPoint {cardId = 1, row = 0, col = 0}, CardPoint {cardId = 0, row = 0, col = 0}]),
              (2, [CardPoint {cardId = 0, row = 0, col = 1}]),
              (3, [CardPoint {cardId = 0, row = 0, col = 2}]),
              (4, [CardPoint {cardId = 1, row = 1, col = 0}, CardPoint {cardId = 0, row = 1, col = 0}]),
              (5, [CardPoint {cardId = 0, row = 1, col = 1}]),
              (6, [CardPoint {cardId = 0, row = 1, col = 2}]),
              (7, [CardPoint {cardId = 1, row = 2, col = 0}, CardPoint {cardId = 0, row = 2, col = 0}]),
              (8, [CardPoint {cardId = 0, row = 2, col = 1}]),
              (9, [CardPoint {cardId = 0, row = 2, col = 2}]),
              (20, [CardPoint {cardId = 1, row = 0, col = 1}]),
              (30, [CardPoint {cardId = 1, row = 0, col = 2}]),
              (50, [CardPoint {cardId = 1, row = 1, col = 1}]),
              (60, [CardPoint {cardId = 1, row = 1, col = 2}]),
              (80, [CardPoint {cardId = 1, row = 2, col = 1}]),
              (90, [CardPoint {cardId = 1, row = 2, col = 2}])
            ]

