{-|
Module      : Ch11.STMProcessingSpec
Description : 第11章のテスト
-}
module Ch11.STMProcessingSpec (spec) where

import Test.Hspec
import Ch11.STMProcessing
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically, readTVar)

spec :: Spec
spec = do
    describe "Ch11.STMProcessing" $ do
        -- ============================================
        -- TVar - トランザクショナル変数
        -- ============================================
        describe "TCounter" $ do
            it "starts with initial value" $ do
                counter <- newTCounter 0
                result <- getTCounter counter
                result `shouldBe` 0

            it "increments atomically" $ do
                counter <- newTCounter 0
                incrementTCounter counter
                incrementTCounter counter
                incrementTCounter counter
                result <- getTCounter counter
                result `shouldBe` 3

            it "modifies with function" $ do
                counter <- newTCounter 5
                modifyTCounter counter (*3)
                result <- getTCounter counter
                result `shouldBe` 15

            it "handles concurrent increments" $ do
                counter <- newTCounter 0
                -- 10スレッドで同時に100回ずつインクリメント
                let incrementN = mapM_ (\_ -> incrementTCounter counter) [1..100]
                _ <- mapM (\_ -> forkIO incrementN) [1..10]
                threadDelay 100000  -- 同期待ち
                result <- getTCounter counter
                result `shouldBe` 1000

        -- ============================================
        -- 銀行口座
        -- ============================================
        describe "Account" $ do
            it "creates with initial balance" $ do
                acc <- newAccount "A001" 1000
                balance <- getBalance acc
                balance `shouldBe` 1000

            it "deposits correctly" $ do
                acc <- newAccount "A001" 1000
                deposit acc 500
                balance <- getBalance acc
                balance `shouldBe` 1500

            it "withdraws correctly" $ do
                acc <- newAccount "A001" 1000
                result <- withdraw acc 300
                result `shouldBe` Right ()
                balance <- getBalance acc
                balance `shouldBe` 700

            it "fails withdrawal with insufficient funds" $ do
                acc <- newAccount "A001" 100
                result <- withdraw acc 500
                result `shouldBe` Left "Insufficient funds"
                balance <- getBalance acc
                balance `shouldBe` 100

        describe "transferSafe" $ do
            it "transfers atomically" $ do
                acc1 <- newAccount "A001" 1000
                acc2 <- newAccount "A002" 500
                result <- transferSafe acc1 acc2 300
                result `shouldBe` Right ()
                balance1 <- getBalance acc1
                balance2 <- getBalance acc2
                balance1 `shouldBe` 700
                balance2 `shouldBe` 800

            it "fails transfer with insufficient funds" $ do
                acc1 <- newAccount "A001" 100
                acc2 <- newAccount "A002" 500
                result <- transferSafe acc1 acc2 300
                result `shouldBe` Left "Insufficient funds"
                balance1 <- getBalance acc1
                balance2 <- getBalance acc2
                balance1 `shouldBe` 100
                balance2 `shouldBe` 500

            it "preserves total balance in concurrent transfers" $ do
                acc1 <- newAccount "A001" 1000
                acc2 <- newAccount "A002" 1000
                -- 複数スレッドで相互送金
                let transfer12 = mapM_ (\_ -> transferSafe acc1 acc2 10) [1..50]
                let transfer21 = mapM_ (\_ -> transferSafe acc2 acc1 10) [1..50]
                _ <- forkIO transfer12
                _ <- forkIO transfer21
                threadDelay 100000
                balance1 <- getBalance acc1
                balance2 <- getBalance acc2
                -- 合計は常に2000
                (balance1 + balance2) `shouldBe` 2000

        -- ============================================
        -- 在庫管理
        -- ============================================
        describe "Inventory" $ do
            it "adds stock correctly" $ do
                inv <- newInventory
                addStock inv "apple" 10
                stock <- getStock inv "apple"
                stock `shouldBe` 10

            it "removes stock correctly" $ do
                inv <- newInventory
                addStock inv "apple" 10
                result <- removeStock inv "apple" 3
                result `shouldBe` Right ()
                stock <- getStock inv "apple"
                stock `shouldBe` 7

            it "fails removal with insufficient stock" $ do
                inv <- newInventory
                addStock inv "apple" 5
                result <- removeStock inv "apple" 10
                result `shouldBe` Left "Insufficient stock"

            it "returns 0 for non-existent item" $ do
                inv <- newInventory
                stock <- getStock inv "banana"
                stock `shouldBe` 0

        describe "transferStock" $ do
            it "transfers stock between inventories" $ do
                inv1 <- newInventory
                inv2 <- newInventory
                addStock inv1 "apple" 10
                result <- transferStock inv1 inv2 "apple" 5
                result `shouldBe` Right ()
                stock1 <- getStock inv1 "apple"
                stock2 <- getStock inv2 "apple"
                stock1 `shouldBe` 5
                stock2 `shouldBe` 5

        -- ============================================
        -- 並行キュー
        -- ============================================
        describe "TQueue" $ do
            it "writes and reads items" $ do
                q <- newTQueue
                writeTQueue q (1 :: Int)
                writeTQueue q 2
                writeTQueue q 3
                r1 <- readTQueue q
                r2 <- readTQueue q
                r3 <- readTQueue q
                [r1, r2, r3] `shouldBe` [1, 2, 3]

            it "tryReadTQueue returns Nothing for empty queue" $ do
                q <- newTQueue :: IO (TQueue Int)
                result <- tryReadTQueue q
                result `shouldBe` Nothing

            it "isEmptyTQueue detects empty queue" $ do
                q <- newTQueue :: IO (TQueue Int)
                empty <- isEmptyTQueue q
                empty `shouldBe` True
                writeTQueue q 1
                notEmpty <- isEmptyTQueue q
                notEmpty `shouldBe` False

        -- ============================================
        -- Producer-Consumer
        -- ============================================
        describe "producerConsumer" $ do
            it "sums all produced values" $ do
                result <- producerConsumer 10
                result `shouldBe` 55  -- 1+2+3+...+10

            it "handles larger sequences" $ do
                result <- producerConsumer 100
                result `shouldBe` 5050  -- 1+2+3+...+100

        -- ============================================
        -- バリア同期
        -- ============================================
        describe "Barrier" $ do
            it "releases when all threads arrive" $ do
                barrier <- newBarrier 3
                result <- newTCounter 0

                -- 3つのスレッドを起動
                _ <- forkIO $ waitBarrier barrier >> incrementTCounter result
                _ <- forkIO $ waitBarrier barrier >> incrementTCounter result
                _ <- forkIO $ waitBarrier barrier >> incrementTCounter result

                threadDelay 100000
                count <- getTCounter result
                count `shouldBe` 3

        -- ============================================
        -- セマフォ
        -- ============================================
        describe "Semaphore" $ do
            it "limits concurrent access" $ do
                sem <- newSemaphore 2
                counter <- newTCounter 0

                -- セマフォを取得
                acquireSemaphore sem
                acquireSemaphore sem

                -- 3つ目の取得は待機になるはず
                -- (ここではタイムアウトでテスト)
                _ <- forkIO $ acquireSemaphore sem >> incrementTCounter counter
                threadDelay 10000  -- 短い待機

                -- まだカウンターは0のはず
                count1 <- getTCounter counter
                count1 `shouldBe` 0

                -- セマフォを解放
                releaseSemaphore sem
                threadDelay 10000

                -- カウンターが1になるはず
                count2 <- getTCounter counter
                count2 `shouldBe` 1

            it "withSemaphore brackets correctly" $ do
                sem <- newSemaphore 1
                result <- withSemaphore sem (return 42)
                result `shouldBe` 42
