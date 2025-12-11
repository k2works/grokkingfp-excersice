{-|
Module      : Ch09.StreamProcessingSpec
Description : 第9章のテスト
-}
module Ch09.StreamProcessingSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Ch09.StreamProcessing

spec :: Spec
spec = do
    describe "Ch09.StreamProcessing" $ do
        -- ============================================
        -- 無限ストリームの基本
        -- ============================================
        describe "naturals" $ do
            it "starts with 1, 2, 3, ..." $ do
                take 5 naturals `shouldBe` [1, 2, 3, 4, 5]

            it "is infinite (can take many)" $ do
                length (take 1000 naturals) `shouldBe` 1000

        describe "evens" $ do
            it "returns even numbers" $ do
                take 5 evens `shouldBe` [2, 4, 6, 8, 10]

            it "all values are even" $ do
                all even (take 100 evens) `shouldBe` True

        describe "odds" $ do
            it "returns odd numbers" $ do
                take 5 odds `shouldBe` [1, 3, 5, 7, 9]

            it "all values are odd" $ do
                all odd (take 100 odds) `shouldBe` True

        describe "fibs" $ do
            it "starts with 0, 1, 1, 2, 3, 5, ..." $ do
                take 10 fibs `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

        describe "primes" $ do
            it "starts with 2, 3, 5, 7, 11, ..." $ do
                take 10 primes `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

            it "all values are prime" $ do
                let isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2..n-1]
                all isPrime (take 50 primes) `shouldBe` True

        -- ============================================
        -- ストリーム生成
        -- ============================================
        describe "repeatVal" $ do
            it "repeats same value" $ do
                take 5 (repeatVal 1) `shouldBe` [1, 1, 1, 1, 1]

            it "works with any value" $ do
                take 3 (repeatVal "hello") `shouldBe` ["hello", "hello", "hello"]

        describe "cycleList" $ do
            it "cycles through list" $ do
                take 7 (cycleList [1, 2, 3]) `shouldBe` [1, 2, 3, 1, 2, 3, 1]

        describe "iterate'" $ do
            it "applies function repeatedly" $ do
                take 5 (iterate' (*2) 1) `shouldBe` [1, 2, 4, 8, 16]

            it "starts with initial value" $ do
                head (iterate' (+1) 100) `shouldBe` 100

        describe "unfold" $ do
            it "generates list from seed" $ do
                take 5 (unfold (\n -> Just (n, n+1)) 0) `shouldBe` [0, 1, 2, 3, 4]

            it "terminates with Nothing" $ do
                unfold (\n -> if n > 3 then Nothing else Just (n, n+1)) 0
                    `shouldBe` [0, 1, 2, 3]

        -- ============================================
        -- ストリーム操作
        -- ============================================
        describe "takeWhile'" $ do
            it "takes while condition holds" $ do
                takeWhile' (<5) [1, 2, 3, 4, 5, 6] `shouldBe` [1, 2, 3, 4]

            it "returns empty for immediate false" $ do
                takeWhile' (<0) [1, 2, 3] `shouldBe` []

        describe "dropWhile'" $ do
            it "drops while condition holds" $ do
                dropWhile' (<5) [1, 2, 3, 4, 5, 6] `shouldBe` [5, 6]

            it "returns whole list if condition never holds" $ do
                dropWhile' (<0) [1, 2, 3] `shouldBe` [1, 2, 3]

        describe "sliding" $ do
            it "creates sliding windows" $ do
                sliding 3 [1, 2, 3, 4, 5] `shouldBe` [[1, 2, 3], [2, 3, 4], [3, 4, 5]]

            it "returns empty for list shorter than window" $ do
                sliding 5 [1, 2, 3] `shouldBe` []

            it "works with window size 1" $ do
                sliding 1 [1, 2, 3] `shouldBe` [[1], [2], [3]]

        describe "chunksOf" $ do
            it "splits into chunks" $ do
                chunksOf 3 [1, 2, 3, 4, 5, 6, 7, 8] `shouldBe` [[1, 2, 3], [4, 5, 6], [7, 8]]

            it "handles empty list" $ do
                chunksOf 3 ([] :: [Int]) `shouldBe` []

        -- ============================================
        -- トレンド検出
        -- ============================================
        describe "trending" $ do
            it "returns True for ascending sequence" $ do
                trending [0.81, 0.82, 0.83] `shouldBe` True

            it "returns False for non-ascending sequence" $ do
                trending [0.81, 0.84, 0.83] `shouldBe` False

            it "returns False for single element" $ do
                trending [0.81] `shouldBe` False

            it "returns False for equal elements" $ do
                trending [1.0, 1.0, 1.0] `shouldBe` False

        describe "isStable" $ do
            it "returns True for all same values" $ do
                isStable [5, 5, 5] `shouldBe` True

            it "returns False for different values" $ do
                isStable [5, 5, 6] `shouldBe` False

            it "returns False for less than 3 elements" $ do
                isStable [5, 5] `shouldBe` False

        describe "findTrend" $ do
            it "finds first trending window" $ do
                findTrend 3 [1, 2, 3, 2, 3, 4, 5] `shouldBe` Just [1, 2, 3]

            it "returns Nothing if no trend" $ do
                findTrend 3 [3, 2, 1, 2, 1, 2] `shouldBe` Nothing

        -- ============================================
        -- 通貨交換レートの例
        -- ============================================
        describe "simulateRates" $ do
            it "generates infinite stream from initial rate" $ do
                let rates = simulateRates 0.80
                length (take 100 rates) `shouldBe` 100

            it "starts with initial rate" $ do
                head (simulateRates 0.80) `shouldBe` 0.80

        describe "exchangeIfTrending" $ do
            it "returns exchanged amount when trend found" $ do
                exchangeIfTrending 100 3 [0.80, 0.81, 0.82, 0.83, 0.80]
                    `shouldBe` Just 82.0

            it "returns Nothing when no trend" $ do
                exchangeIfTrending 100 3 [0.83, 0.82, 0.81]
                    `shouldBe` Nothing

        -- ============================================
        -- ストリーム合成
        -- ============================================
        describe "zipWithIndex" $ do
            it "adds indices to elements" $ do
                take 3 (zipWithIndex ['a', 'b', 'c'])
                    `shouldBe` [(0, 'a'), (1, 'b'), (2, 'c')]

        describe "interleave" $ do
            it "interleaves two lists" $ do
                take 6 (interleave [1, 3, 5] [2, 4, 6])
                    `shouldBe` [1, 2, 3, 4, 5, 6]

            it "handles different length lists" $ do
                interleave [1, 3] [2, 4, 6, 8]
                    `shouldBe` [1, 2, 3, 4, 6, 8]

        describe "merge" $ do
            it "merges two sorted lists" $ do
                take 10 (merge [1, 3, 5, 7, 9] [2, 4, 6, 8, 10])
                    `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

            it "handles duplicates" $ do
                merge [1, 2, 3] [2, 3, 4]
                    `shouldBe` [1, 2, 2, 3, 3, 4]
