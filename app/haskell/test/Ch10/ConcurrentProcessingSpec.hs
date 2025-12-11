{-|
Module      : Ch10.ConcurrentProcessingSpec
Description : 第10章のテスト
-}
module Ch10.ConcurrentProcessingSpec (spec) where

import Test.Hspec
import Ch10.ConcurrentProcessing
import Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "Ch10.ConcurrentProcessing" $ do
        -- ============================================
        -- IORef - 可変参照
        -- ============================================
        describe "Counter (IORef)" $ do
            it "starts with initial value" $ do
                counter <- newCounter 0
                result <- getCounter counter
                result `shouldBe` 0

            it "increments correctly" $ do
                counter <- newCounter 0
                incrementCounter counter
                incrementCounter counter
                incrementCounter counter
                result <- getCounter counter
                result `shouldBe` 3

            it "modifies with function" $ do
                counter <- newCounter 10
                modifyCounter counter (*2)
                result <- getCounter counter
                result `shouldBe` 20

        -- ============================================
        -- 並列実行
        -- ============================================
        describe "mapConcurrently'" $ do
            it "applies function to all elements" $ do
                results <- mapConcurrently' (\x -> return (x * 2)) [1, 2, 3, 4, 5]
                results `shouldBe` [2, 4, 6, 8, 10]

            it "handles empty list" $ do
                results <- mapConcurrently' (\x -> return (x * 2)) ([] :: [Int])
                results `shouldBe` []

        describe "sequenceConcurrently" $ do
            it "executes all actions" $ do
                results <- sequenceConcurrently [return 1, return 2, return 3]
                results `shouldBe` [1, 2, 3]

            it "preserves order" $ do
                results <- sequenceConcurrently [return "a", return "b", return "c"]
                results `shouldBe` ["a", "b", "c"]

        -- ============================================
        -- チェックイン集計
        -- ============================================
        describe "CheckInState" $ do
            it "records check-ins correctly" $ do
                state <- newCheckInState
                recordCheckIn state (City "Tokyo")
                recordCheckIn state (City "Tokyo")
                recordCheckIn state (City "Osaka")
                top <- getTopCities 2 state
                length top `shouldBe` 2
                csCheckIns (head top) `shouldBe` 2

            it "returns top cities in order" $ do
                state <- newCheckInState
                recordCheckIn state (City "A")
                recordCheckIn state (City "A")
                recordCheckIn state (City "A")
                recordCheckIn state (City "B")
                recordCheckIn state (City "B")
                recordCheckIn state (City "C")
                top <- getTopCities 3 state
                map (cityName . csCity) top `shouldBe` ["A", "B", "C"]

        describe "topCities (pure)" $ do
            it "returns top n cities sorted by check-ins" $ do
                let m = Map.fromList [(City "X", 5), (City "Y", 10), (City "Z", 3)]
                let top = topCities 2 m
                map csCheckIns top `shouldBe` [10, 5]

            it "handles empty map" $ do
                let m = Map.empty :: Map.Map City Int
                topCities 3 m `shouldBe` []

        -- ============================================
        -- サイコロの並行処理
        -- ============================================
        describe "castDieConcurrently" $ do
            it "returns two results" $ do
                results <- castDieConcurrently
                length results `shouldBe` 2

            it "all values are between 1 and 6" $ do
                results <- castDieConcurrently
                all (\x -> x >= 1 && x <= 6) results `shouldBe` True

        describe "castDieNTimes" $ do
            it "returns n results" $ do
                results <- castDieNTimes 10
                length results `shouldBe` 10

            it "all values are valid die rolls" $ do
                results <- castDieNTimes 100
                all (\x -> x >= 1 && x <= 6) results `shouldBe` True

        describe "sumDiceConcurrently" $ do
            it "returns sum in valid range" $ do
                total <- sumDiceConcurrently 3
                (total >= 3 && total <= 18) `shouldBe` True

        -- ============================================
        -- Async
        -- ============================================
        describe "asyncRun and asyncWait" $ do
            it "runs action and returns result" $ do
                handle <- asyncRun (return 42)
                result <- asyncWait handle
                result `shouldBe` 42

            it "handles IO actions" $ do
                handle <- asyncRun (return "hello" >>= \s -> return (s ++ " world"))
                result <- asyncWait handle
                result `shouldBe` "hello world"

        describe "forkAndWait" $ do
            it "executes and waits for result" $ do
                result <- forkAndWait (return 100)
                result `shouldBe` 100

        -- ============================================
        -- タイムアウト
        -- ============================================
        describe "timeoutIO" $ do
            it "returns Just for fast action" $ do
                result <- timeoutIO 1000000 (return 42)
                result `shouldBe` Just 42

            it "returns Nothing for slow action" $ do
                result <- timeoutIO 1000 (threadDelay 100000 >> return 42)
                result `shouldBe` Nothing

        describe "asyncWithTimeout" $ do
            it "returns Just for fast action" $ do
                result <- asyncWithTimeout 1000000 (return "done")
                result `shouldBe` Just "done"

        -- ============================================
        -- Race
        -- ============================================
        describe "raceIO" $ do
            it "returns Left when first action wins" $ do
                result <- raceIO (return "first") (threadDelay 100000 >> return "second")
                result `shouldBe` Left "first"

        -- ============================================
        -- ユーティリティ
        -- ============================================
        describe "sleep" $ do
            it "sleeps for specified milliseconds" $ do
                -- Just test that it doesn't throw
                sleep 1
                True `shouldBe` True

        describe "withTimeout" $ do
            it "returns Just for fast action" $ do
                result <- withTimeout 1000 (return 42)
                result `shouldBe` Just 42
