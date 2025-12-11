{-|
Module      : Ch04.FunctionsAsValuesSpec
Description : 第4章のテスト
-}
module Ch04.FunctionsAsValuesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Ch04.FunctionsAsValues

spec :: Spec
spec = do
    describe "Ch04.FunctionsAsValues" $ do
        -- ============================================
        -- 基本的な高階関数
        -- ============================================
        describe "myMap" $ do
            it "maps function over list" $ do
                myMap (+1) [1,2,3] `shouldBe` [2,3,4]

            it "handles empty list" $ do
                myMap (+1) [] `shouldBe` []

            it "property: behaves like map" $ property $
                \xs -> myMap (*2) xs == map (*2) (xs :: [Int])

        describe "myFilter" $ do
            it "filters list by predicate" $ do
                myFilter even [1,2,3,4,5] `shouldBe` [2,4]

            it "handles empty list" $ do
                myFilter even [] `shouldBe` []

            it "property: behaves like filter" $ property $
                \xs -> myFilter even xs == filter even (xs :: [Int])

        describe "myFoldl" $ do
            it "folds list from left" $ do
                myFoldl (+) 0 [1,2,3,4,5] `shouldBe` 15

            it "folds with different initial value" $ do
                myFoldl (+) 100 [1,2,3] `shouldBe` 106

            it "handles empty list" $ do
                myFoldl (+) 42 [] `shouldBe` 42

        -- ============================================
        -- ワードスコアリング
        -- ============================================
        describe "score" $ do
            it "counts non-'a' characters" $ do
                score "scala" `shouldBe` 3
                score "rust" `shouldBe` 4
                score "java" `shouldBe` 2
                score "aaa" `shouldBe` 0

        describe "bonus" $ do
            it "returns 5 for words with 'c'" $ do
                bonus "scala" `shouldBe` 5
                bonus "c" `shouldBe` 5

            it "returns 0 for words without 'c'" $ do
                bonus "java" `shouldBe` 0
                bonus "rust" `shouldBe` 0

        describe "penalty" $ do
            it "returns 7 for words with 's'" $ do
                penalty "rust" `shouldBe` 7
                penalty "scala" `shouldBe` 7

            it "returns 0 for words without 's'" $ do
                penalty "java" `shouldBe` 0

        describe "rankedWords" $ do
            it "ranks words by score descending" $ do
                rankedWords score ["ada","haskell","scala","java","rust"]
                    `shouldBe` ["haskell","rust","scala","java","ada"]

            it "ranks by score + bonus" $ do
                let scoreWithBonus w = score w + bonus w
                rankedWords scoreWithBonus ["ada","haskell","scala","java","rust"]
                    `shouldBe` ["scala","haskell","rust","java","ada"]

        describe "totalScore" $ do
            it "sums scores of all words" $ do
                totalScore score ["java","scala"] `shouldBe` 5

        -- ============================================
        -- 関数を返す関数
        -- ============================================
        describe "largerThan" $ do
            it "returns function that filters" $ do
                filter (largerThan 4) [5,1,2,4,0] `shouldBe` [5]

            it "returns function that filters with different threshold" $ do
                filter (largerThan 1) [5,1,2,4,0] `shouldBe` [5,2,4]

        describe "divisibleBy" $ do
            it "returns function that checks divisibility" $ do
                filter (divisibleBy 3) [1,2,3,6,9,10] `shouldBe` [3,6,9]

            it "handles divisibility by 2" $ do
                filter (divisibleBy 2) [1,2,3,4,5,6] `shouldBe` [2,4,6]

        describe "containsChar" $ do
            it "returns function that checks character" $ do
                filter (containsChar 's') ["scala","java","rust"]
                    `shouldBe` ["scala","rust"]

        -- ============================================
        -- プログラミング言語の例
        -- ============================================
        describe "ProgrammingLanguage" $ do
            let java  = ProgrammingLanguage "Java" 1995
            let scala = ProgrammingLanguage "Scala" 2004
            let haskell = ProgrammingLanguage "Haskell" 1990
            let langs = [java, scala, haskell]

            it "filters by year" $ do
                map plName (filterByYear 2000 langs) `shouldBe` ["Scala"]

            it "gets names" $ do
                getNames langs `shouldBe` ["Java","Scala","Haskell"]

            it "sorts by year" $ do
                map plName (sortByYear langs) `shouldBe` ["Haskell","Java","Scala"]

        -- ============================================
        -- カリー化の例
        -- ============================================
        describe "add" $ do
            it "adds two numbers from tuple" $ do
                add (2, 3) `shouldBe` 5

        describe "addCurried" $ do
            it "adds two numbers" $ do
                addCurried 2 3 `shouldBe` 5

            it "supports partial application" $ do
                let add5 = addCurried 5
                add5 3 `shouldBe` 8
                add5 10 `shouldBe` 15

        describe "multiply3" $ do
            it "multiplies three numbers" $ do
                multiply3 2 3 4 `shouldBe` 24

            it "supports partial application" $ do
                let double = multiply3 2 1
                double 5 `shouldBe` 10
                double 7 `shouldBe` 14
