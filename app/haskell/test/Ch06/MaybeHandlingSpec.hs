{-|
Module      : Ch06.MaybeHandlingSpec
Description : 第6章のテスト
-}
module Ch06.MaybeHandlingSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Ch06.MaybeHandling

spec :: Spec
spec = do
    describe "Ch06.MaybeHandling" $ do
        -- ============================================
        -- TV番組のパース
        -- ============================================
        describe "extractName" $ do
            it "extracts name from valid input" $ do
                extractName "Breaking Bad (2008-2013)" `shouldBe` Just "Breaking Bad"

            it "returns Nothing for invalid input" $ do
                extractName "(2008-2013)" `shouldBe` Nothing
                extractName "No Bracket" `shouldBe` Nothing

        describe "extractYearStart" $ do
            it "extracts start year" $ do
                extractYearStart "Breaking Bad (2008-2013)" `shouldBe` Just 2008

            it "returns Nothing for invalid year" $ do
                extractYearStart "Breaking Bad (oops-2013)" `shouldBe` Nothing

            it "returns Nothing for missing bracket" $ do
                extractYearStart "Breaking Bad 2008-2013" `shouldBe` Nothing

        describe "extractYearEnd" $ do
            it "extracts end year" $ do
                extractYearEnd "Breaking Bad (2008-2013)" `shouldBe` Just 2013

            it "returns Nothing for invalid year" $ do
                extractYearEnd "Breaking Bad (2008-oops)" `shouldBe` Nothing

        describe "extractSingleYear" $ do
            it "extracts single year" $ do
                extractSingleYear "Chernobyl (2019)" `shouldBe` Just 2019

            it "returns Nothing when dash present" $ do
                extractSingleYear "Breaking Bad (2008-2013)" `shouldBe` Nothing

        describe "parseShowSafe" $ do
            it "parses valid show with range" $ do
                parseShowSafe "Breaking Bad (2008-2013)"
                    `shouldBe` Just (TvShow "Breaking Bad" 2008 2013)

            it "parses valid show with single year" $ do
                parseShowSafe "Chernobyl (2019)"
                    `shouldBe` Just (TvShow "Chernobyl" 2019 2019)

            it "returns Nothing for invalid format" $ do
                parseShowSafe "Invalid" `shouldBe` Nothing
                parseShowSafe "The Wire 2002-2008" `shouldBe` Nothing
                parseShowSafe "(2008-2013)" `shouldBe` Nothing

        -- ============================================
        -- Maybe の基本操作
        -- ============================================
        describe "safeDivide" $ do
            it "divides when divisor is non-zero" $ do
                safeDivide 10 2 `shouldBe` Just 5
                safeDivide 7 2 `shouldBe` Just 3

            it "returns Nothing when divisor is zero" $ do
                safeDivide 10 0 `shouldBe` Nothing

            it "property: safeDivide a b == Just (a div b) when b /= 0" $ property $
                \a b -> b /= 0 ==> safeDivide a b == Just (a `div` b)

        describe "safeHead" $ do
            it "returns Just first element" $ do
                safeHead [1,2,3 :: Int] `shouldBe` Just 1

            it "returns Nothing for empty list" $ do
                safeHead ([] :: [Int]) `shouldBe` Nothing

        describe "safeLast" $ do
            it "returns Just last element" $ do
                safeLast [1,2,3 :: Int] `shouldBe` Just 3

            it "returns Nothing for empty list" $ do
                safeLast ([] :: [Int]) `shouldBe` Nothing

        describe "safeIndex" $ do
            it "returns element at index" $ do
                safeIndex 1 [1,2,3 :: Int] `shouldBe` Just 2

            it "returns Nothing for out of bounds" $ do
                safeIndex 10 [1,2,3 :: Int] `shouldBe` Nothing

            it "returns Nothing for negative index" $ do
                safeIndex (-1) [1,2,3 :: Int] `shouldBe` Nothing

        describe "safeReadInt" $ do
            it "parses valid integer" $ do
                safeReadInt "123" `shouldBe` Just 123
                safeReadInt "-456" `shouldBe` Just (-456)

            it "returns Nothing for invalid input" $ do
                safeReadInt "abc" `shouldBe` Nothing
                safeReadInt "12.34" `shouldBe` Nothing

        -- ============================================
        -- Maybe の合成
        -- ============================================
        describe "addStrings" $ do
            it "adds two valid numbers" $ do
                addStrings "10" "20" `shouldBe` Just 30

            it "returns Nothing when first is invalid" $ do
                addStrings "abc" "20" `shouldBe` Nothing

            it "returns Nothing when second is invalid" $ do
                addStrings "10" "xyz" `shouldBe` Nothing

        describe "multiplyStrings" $ do
            it "multiplies two valid numbers" $ do
                multiplyStrings "3" "4" `shouldBe` Just 12

            it "returns Nothing for invalid input" $ do
                multiplyStrings "3" "abc" `shouldBe` Nothing

        -- ============================================
        -- エラーハンドリング戦略
        -- ============================================
        describe "parseShowsBestEffort" $ do
            it "returns only valid shows" $ do
                let input = ["Breaking Bad (2008-2013)", "Invalid", "Mad Men (2007-2015)"]
                parseShowsBestEffort input `shouldBe`
                    [TvShow "Breaking Bad" 2008 2013, TvShow "Mad Men" 2007 2015]

            it "returns empty list when all invalid" $ do
                parseShowsBestEffort ["Invalid1", "Invalid2"] `shouldBe` []

        describe "parseShowsAllOrNothing" $ do
            it "returns Just list when all valid" $ do
                let input = ["Breaking Bad (2008-2013)", "Mad Men (2007-2015)"]
                parseShowsAllOrNothing input `shouldBe`
                    Just [TvShow "Breaking Bad" 2008 2013, TvShow "Mad Men" 2007 2015]

            it "returns Nothing when any invalid" $ do
                let input = ["Breaking Bad (2008-2013)", "Invalid"]
                parseShowsAllOrNothing input `shouldBe` Nothing

        -- ============================================
        -- orElse
        -- ============================================
        describe "orElse" $ do
            it "returns first when Just" $ do
                Just 5 `orElse` Just 10 `shouldBe` Just (5 :: Int)

            it "returns second when first is Nothing" $ do
                Nothing `orElse` Just 10 `shouldBe` Just (10 :: Int)

            it "returns Nothing when both Nothing" $ do
                (Nothing :: Maybe Int) `orElse` Nothing `shouldBe` Nothing

        describe "firstJust" $ do
            it "returns first Just in list" $ do
                firstJust [Nothing, Just 5, Just 10] `shouldBe` Just (5 :: Int)

            it "returns Nothing when all Nothing" $ do
                firstJust [Nothing, Nothing :: Maybe Int] `shouldBe` Nothing
