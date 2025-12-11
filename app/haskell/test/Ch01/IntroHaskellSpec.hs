{-|
Module      : Ch01.IntroHaskellSpec
Description : 第1章のテスト
-}
module Ch01.IntroHaskellSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Ch01.IntroHaskell

spec :: Spec
spec = do
    describe "Ch01.IntroHaskell" $ do
        -- ============================================
        -- 基本的な関数のテスト
        -- ============================================
        describe "increment" $ do
            it "increments 5 to 6" $ do
                increment 5 `shouldBe` 6

            it "increments 0 to 1" $ do
                increment 0 `shouldBe` 1

            it "increments -1 to 0" $ do
                increment (-1) `shouldBe` 0

            it "property: increment x == x + 1" $ property $
                \x -> increment x == x + 1

        describe "getFirstCharacter" $ do
            it "returns 'H' for \"Hello\"" $ do
                getFirstCharacter "Hello" `shouldBe` 'H'

            it "returns 'a' for \"abc\"" $ do
                getFirstCharacter "abc" `shouldBe` 'a'

        describe "wordScore" $ do
            it "returns 7 for \"Haskell\"" $ do
                wordScore "Haskell" `shouldBe` 7

            it "returns 0 for empty string" $ do
                wordScore "" `shouldBe` 0

            it "property: wordScore is same as length" $ property $
                \s -> wordScore s == length s

        -- ============================================
        -- 命令型 vs 関数型
        -- ============================================
        describe "calculateScore" $ do
            it "imperative and functional give same results" $ property $
                \s -> calculateScoreImperative s == calculateScoreFunctional s

        -- ============================================
        -- 基本的な関数
        -- ============================================
        describe "add" $ do
            it "adds 2 + 3 = 5" $ do
                add 2 3 `shouldBe` 5

            it "property: addition is commutative" $ property $
                \a b -> add a b == add b a

            it "property: addition is associative" $ property $
                \a b c -> add (add a b) c == add a (add b c)

            it "property: 0 is identity" $ property $
                \a -> add a 0 == a

        describe "multiply" $ do
            it "multiplies 4 * 5 = 20" $ do
                multiply 4 5 `shouldBe` 20

            it "property: multiplication is commutative" $ property $
                \a b -> multiply a b == multiply b a

        describe "isPositive" $ do
            it "returns True for 5" $ do
                isPositive 5 `shouldBe` True

            it "returns False for -3" $ do
                isPositive (-3) `shouldBe` False

            it "returns False for 0" $ do
                isPositive 0 `shouldBe` False

        describe "greet" $ do
            it "greets \"World\" as \"Hello, World!\"" $ do
                greet "World" `shouldBe` "Hello, World!"

            it "greets \"Haskell\" as \"Hello, Haskell!\"" $ do
                greet "Haskell" `shouldBe` "Hello, Haskell!"

        -- ============================================
        -- 遅延評価
        -- ============================================
        describe "takeFive" $ do
            it "returns [1,1,1,1,1]" $ do
                takeFive `shouldBe` [1,1,1,1,1]

            it "takes 5 elements from infinite list" $ do
                length takeFive `shouldBe` 5
