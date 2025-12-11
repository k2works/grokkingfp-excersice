{-|
Module      : Ch03.ImmutableValuesSpec
Description : 第3章のテスト
-}
module Ch03.ImmutableValuesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Ch03.ImmutableValues

spec :: Spec
spec = do
    describe "Ch03.ImmutableValues" $ do
        -- ============================================
        -- リストの基本操作
        -- ============================================
        describe "appended" $ do
            it "appends element to list" $ do
                appended [1,2,3] 4 `shouldBe` [1,2,3,4]

            it "appends to empty list" $ do
                appended [] 1 `shouldBe` [1]

            it "property: length increases by 1" $ property $
                \xs x -> length (appended xs (x :: Int)) == length xs + 1

        describe "appendedAll" $ do
            it "concatenates two lists" $ do
                appendedAll [1,2] [3,4] `shouldBe` [1,2,3,4]

            it "handles empty lists" $ do
                appendedAll [] [1,2] `shouldBe` [1,2]
                appendedAll [1,2] [] `shouldBe` [1,2]

        describe "slice" $ do
            it "slices from start to end" $ do
                slice 1 3 ["a","b","c","d"] `shouldBe` ["b","c"]

            it "handles full slice" $ do
                slice 0 3 ["a","b","c"] `shouldBe` ["a","b","c"]

            it "handles empty slice" $ do
                slice 1 1 ["a","b","c"] `shouldBe` []

        describe "firstTwo" $ do
            it "takes first two elements" $ do
                firstTwo ["a","b","c"] `shouldBe` ["a","b"]

            it "returns less if list is shorter" $ do
                firstTwo ["a"] `shouldBe` ["a"]

        describe "lastTwo" $ do
            it "takes last two elements" $ do
                lastTwo ["a","b","c"] `shouldBe` ["b","c"]

            it "returns whole list if shorter" $ do
                lastTwo ["a"] `shouldBe` ["a"]

        -- ============================================
        -- リストの変換
        -- ============================================
        describe "movedFirstTwoToTheEnd" $ do
            it "moves first two to end" $ do
                movedFirstTwoToTheEnd ["a","b","c"] `shouldBe` ["c","a","b"]

            it "handles longer list" $ do
                movedFirstTwoToTheEnd ["a","b","c","d","e"] `shouldBe` ["c","d","e","a","b"]

        describe "insertedBeforeLast" $ do
            it "inserts before last element" $ do
                insertedBeforeLast ["a","b"] "c" `shouldBe` ["a","c","b"]

            it "works with longer list" $ do
                insertedBeforeLast ["a","b","c","d"] "X" `shouldBe` ["a","b","c","X","d"]

        describe "insertAtMiddle" $ do
            it "inserts at middle of even-length list" $ do
                insertAtMiddle ["a","b","c","d"] "X" `shouldBe` ["a","b","X","c","d"]

            it "inserts at middle of odd-length list" $ do
                insertAtMiddle ["a","b","c"] "X" `shouldBe` ["a","X","b","c"]

        -- ============================================
        -- 旅程の例
        -- ============================================
        describe "replan" $ do
            it "inserts new city before specified city" $ do
                replan ["Paris","Berlin","Krakow"] "Vienna" "Krakow"
                    `shouldBe` ["Paris","Berlin","Vienna","Krakow"]

            it "inserts at beginning" $ do
                replan ["Paris","Berlin"] "London" "Paris"
                    `shouldBe` ["London","Paris","Berlin"]

            it "preserves original list (immutability)" $ do
                let planA = ["Paris","Berlin","Krakow"]
                let planB = replan planA "Vienna" "Krakow"
                planA `shouldBe` ["Paris","Berlin","Krakow"]
                planB `shouldBe` ["Paris","Berlin","Vienna","Krakow"]

        -- ============================================
        -- 文字列操作
        -- ============================================
        describe "abbreviate" $ do
            it "abbreviates full name" $ do
                abbreviate "Alonzo Church" `shouldBe` "A. Church"

            it "handles already abbreviated name" $ do
                abbreviate "A. Church" `shouldBe` "A. Church"

            it "handles different names" $ do
                abbreviate "Haskell Curry" `shouldBe` "H. Curry"
