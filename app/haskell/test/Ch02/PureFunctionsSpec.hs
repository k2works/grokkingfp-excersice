{-|
Module      : Ch02.PureFunctionsSpec
Description : 第2章のテスト
-}
module Ch02.PureFunctionsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Ch02.PureFunctions

spec :: Spec
spec = do
    describe "Ch02.PureFunctions" $ do
        -- ============================================
        -- 純粋関数のテスト
        -- ============================================
        describe "pureIncrement" $ do
            it "increments 5 to 6" $ do
                pureIncrement 5 `shouldBe` 6

            it "same input always gives same output" $ do
                pureIncrement 10 `shouldBe` pureIncrement 10

            it "property: pureIncrement x == x + 1" $ property $
                \x -> pureIncrement x == x + 1

        describe "pureAdd" $ do
            it "adds 2 + 3 = 5" $ do
                pureAdd 2 3 `shouldBe` 5

            it "property: addition is commutative" $ property $
                \a b -> pureAdd a b == pureAdd b a

        describe "pureGetFirstChar" $ do
            it "returns 'H' for \"Hello\"" $ do
                pureGetFirstChar "Hello" `shouldBe` 'H'

        -- ============================================
        -- ショッピングカート
        -- ============================================
        describe "getDiscountPercentage" $ do
            it "returns 5 when Book is in cart" $ do
                getDiscountPercentage ["Apple", "Book", "Pen"] `shouldBe` 5

            it "returns 0 when Book is not in cart" $ do
                getDiscountPercentage ["Apple", "Pen"] `shouldBe` 0

            it "returns 0 for empty cart" $ do
                getDiscountPercentage [] `shouldBe` 0

            it "returns 5 when only Book is in cart" $ do
                getDiscountPercentage ["Book"] `shouldBe` 5

        describe "calculateDiscount" $ do
            it "calculates 5% discount for cart with Book" $ do
                calculateDiscount 100 ["Apple", "Book"] `shouldBe` 5.0

            it "calculates 0% discount for cart without Book" $ do
                calculateDiscount 100 ["Apple"] `shouldBe` 0.0

        -- ============================================
        -- チップ計算
        -- ============================================
        describe "getTipPercentage" $ do
            it "returns 20 for groups larger than 5" $ do
                getTipPercentage ["A", "B", "C", "D", "E", "F"] `shouldBe` 20

            it "returns 10 for groups of 1-5" $ do
                getTipPercentage ["A", "B"] `shouldBe` 10

            it "returns 10 for exactly 5 people" $ do
                getTipPercentage ["A", "B", "C", "D", "E"] `shouldBe` 10

            it "returns 0 for empty group" $ do
                getTipPercentage [] `shouldBe` 0

        describe "calculateTip" $ do
            it "calculates 10% tip for small group" $ do
                calculateTip 100 ["A", "B", "C"] `shouldBe` 10.0

            it "calculates 20% tip for large group" $ do
                calculateTip 100 ["A", "B", "C", "D", "E", "F"] `shouldBe` 20.0

        -- ============================================
        -- ワードスコア
        -- ============================================
        describe "wordScoreWithoutA" $ do
            it "returns 3 for \"Scala\"" $ do
                wordScoreWithoutA "Scala" `shouldBe` 3

            it "returns 7 for \"Haskell\"" $ do
                wordScoreWithoutA "Haskell" `shouldBe` 7

            it "returns 0 for \"aaa\"" $ do
                wordScoreWithoutA "aaa" `shouldBe` 0

            it "returns 0 for empty string" $ do
                wordScoreWithoutA "" `shouldBe` 0

            it "property: score <= length" $ property $
                \s -> wordScoreWithoutA s <= length s

        describe "wordScoreUpperCase" $ do
            it "returns 5 for \"hello\"" $ do
                wordScoreUpperCase "hello" `shouldBe` 5

            it "returns same as length" $ property $
                \s -> wordScoreUpperCase s == length s

        -- ============================================
        -- テスト用ヘルパー
        -- ============================================
        describe "isEven" $ do
            it "returns True for 4" $ do
                isEven 4 `shouldBe` True

            it "returns False for 3" $ do
                isEven 3 `shouldBe` False

            it "returns True for 0" $ do
                isEven 0 `shouldBe` True

            it "returns True for -2" $ do
                isEven (-2) `shouldBe` True

            it "property: n and n+2 have same parity" $ property $
                \n -> isEven n == isEven (n + 2)

        describe "absoluteValue" $ do
            it "returns 5 for -5" $ do
                absoluteValue (-5) `shouldBe` 5

            it "returns 5 for 5" $ do
                absoluteValue 5 `shouldBe` 5

            it "returns 0 for 0" $ do
                absoluteValue 0 `shouldBe` 0

            it "property: result is always non-negative" $ property $
                \n -> absoluteValue n >= 0

        describe "safeHead" $ do
            it "returns Just 1 for [1,2,3]" $ do
                safeHead [1,2,3 :: Int] `shouldBe` Just 1

            it "returns Nothing for empty list" $ do
                safeHead ([] :: [Int]) `shouldBe` Nothing

        describe "safeTail" $ do
            it "returns Just [2,3] for [1,2,3]" $ do
                safeTail [1,2,3 :: Int] `shouldBe` Just [2,3]

            it "returns Nothing for empty list" $ do
                safeTail ([] :: [Int]) `shouldBe` Nothing

            it "returns Just [] for singleton list" $ do
                safeTail [1 :: Int] `shouldBe` Just []

        -- ============================================
        -- 参照透過性
        -- ============================================
        describe "referentialTransparencyExample" $ do
            it "demonstrates referential transparency" $ do
                referentialTransparencyExample `shouldBe` True
