{-|
Module      : Ch11.STMProcessing
Description : 第11章: STM (Software Transactional Memory)
Copyright   : (c) Project Team, 2024
License     : MIT

STM を使った安全な並行処理について学ぶ
STM はトランザクションベースの並行処理を提供する
-}
module Ch11.STMProcessing
    ( -- * TVar - トランザクショナル変数
      TCounter
    , newTCounter
    , incrementTCounter
    , getTCounter
    , modifyTCounter
      -- * STM トランザクション
    , atomicallyIO
    , retrySTM
    , orElseSTM
      -- * 銀行口座の例
    , Account
    , newAccount
    , getBalance
    , deposit
    , withdraw
    , transfer
    , transferSafe
      -- * 在庫管理の例
    , Inventory
    , newInventory
    , addStock
    , removeStock
    , getStock
    , transferStock
      -- * 並行キュー
    , TQueue
    , newTQueue
    , writeTQueue
    , readTQueue
    , tryReadTQueue
    , isEmptyTQueue
      -- * Producer-Consumer パターン
    , producerConsumer
    , producer
    , consumer
      -- * バリア同期
    , Barrier
    , newBarrier
    , waitBarrier
    , signalBarrier
      -- * セマフォ
    , Semaphore
    , newSemaphore
    , acquireSemaphore
    , releaseSemaphore
    , withSemaphore
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (replicateM_, when, forever)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- ============================================
-- TVar - トランザクショナル変数（STM 版の Ref）
-- ============================================

-- | トランザクショナルカウンター
type TCounter = TVar Int

-- | 新しいトランザクショナルカウンターを作成
--
-- >>> counter <- newTCounter 0
-- >>> getTCounter counter
-- 0
newTCounter :: Int -> IO TCounter
newTCounter = newTVarIO

-- | カウンターをインクリメント（アトミック）
--
-- >>> counter <- newTCounter 0
-- >>> incrementTCounter counter
-- >>> getTCounter counter
-- 1
incrementTCounter :: TCounter -> IO ()
incrementTCounter counter = atomically $ modifyTVar' counter (+1)

-- | カウンターの値を取得
getTCounter :: TCounter -> IO Int
getTCounter = readTVarIO

-- | カウンターを関数で変更
modifyTCounter :: TCounter -> (Int -> Int) -> IO ()
modifyTCounter counter f = atomically $ modifyTVar' counter f

-- ============================================
-- STM トランザクション
-- ============================================

-- | STM アクションを IO で実行
atomicallyIO :: STM a -> IO a
atomicallyIO = atomically

-- | トランザクションをリトライ
retrySTM :: STM a
retrySTM = retry

-- | 代替トランザクション
orElseSTM :: STM a -> STM a -> STM a
orElseSTM = orElse

-- ============================================
-- 銀行口座の例
-- ============================================

-- | 口座を表すデータ型
data Account = Account
    { accountId      :: String
    , accountBalance :: TVar Int
    }

-- | 新しい口座を作成
--
-- >>> acc <- newAccount "A001" 1000
-- >>> getBalance acc
-- 1000
newAccount :: String -> Int -> IO Account
newAccount accId initialBalance = do
    balanceVar <- newTVarIO initialBalance
    return $ Account accId balanceVar

-- | 残高を取得
getBalance :: Account -> IO Int
getBalance = readTVarIO . accountBalance

-- | 入金
--
-- >>> acc <- newAccount "A001" 1000
-- >>> deposit acc 500
-- >>> getBalance acc
-- 1500
deposit :: Account -> Int -> IO ()
deposit acc amount = atomically $
    modifyTVar' (accountBalance acc) (+ amount)

-- | 出金（残高不足時はエラー）
--
-- >>> acc <- newAccount "A001" 1000
-- >>> withdraw acc 500
-- Right ()
-- >>> getBalance acc
-- 500
withdraw :: Account -> Int -> IO (Either String ())
withdraw acc amount = atomically $ do
    balance <- readTVar (accountBalance acc)
    if balance >= amount
        then do
            writeTVar (accountBalance acc) (balance - amount)
            return $ Right ()
        else return $ Left "Insufficient funds"

-- | 送金（アトミックではない版 - 問題あり）
transfer :: Account -> Account -> Int -> IO (Either String ())
transfer from to amount = do
    result <- withdraw from amount
    case result of
        Right () -> deposit to amount >> return (Right ())
        Left err -> return (Left err)

-- | 送金（アトミック版 - 安全）
--
-- >>> acc1 <- newAccount "A001" 1000
-- >>> acc2 <- newAccount "A002" 500
-- >>> transferSafe acc1 acc2 300
-- Right ()
-- >>> getBalance acc1
-- 700
-- >>> getBalance acc2
-- 800
transferSafe :: Account -> Account -> Int -> IO (Either String ())
transferSafe from to amount = atomically $ do
    fromBalance <- readTVar (accountBalance from)
    if fromBalance >= amount
        then do
            modifyTVar' (accountBalance from) (subtract amount)
            modifyTVar' (accountBalance to) (+ amount)
            return $ Right ()
        else return $ Left "Insufficient funds"

-- ============================================
-- 在庫管理の例
-- ============================================

-- | 在庫を表すデータ型
type Inventory = TVar (Map String Int)

-- | 新しい在庫を作成
newInventory :: IO Inventory
newInventory = newTVarIO Map.empty

-- | 在庫を追加
--
-- >>> inv <- newInventory
-- >>> addStock inv "apple" 10
-- >>> getStock inv "apple"
-- 10
addStock :: Inventory -> String -> Int -> IO ()
addStock inv item amount = atomically $
    modifyTVar' inv $ Map.insertWith (+) item amount

-- | 在庫を削減
--
-- >>> inv <- newInventory
-- >>> addStock inv "apple" 10
-- >>> removeStock inv "apple" 3
-- Right ()
-- >>> getStock inv "apple"
-- 7
removeStock :: Inventory -> String -> Int -> IO (Either String ())
removeStock inv item amount = atomically $ do
    stock <- readTVar inv
    let currentAmount = Map.findWithDefault 0 item stock
    if currentAmount >= amount
        then do
            modifyTVar' inv $ Map.adjust (subtract amount) item
            return $ Right ()
        else return $ Left "Insufficient stock"

-- | 在庫数を取得
getStock :: Inventory -> String -> IO Int
getStock inv item = do
    stock <- readTVarIO inv
    return $ Map.findWithDefault 0 item stock

-- | 在庫を別の倉庫に移動
transferStock :: Inventory -> Inventory -> String -> Int -> IO (Either String ())
transferStock fromInv toInv item amount = atomically $ do
    fromStock <- readTVar fromInv
    let currentAmount = Map.findWithDefault 0 item fromStock
    if currentAmount >= amount
        then do
            modifyTVar' fromInv $ Map.adjust (subtract amount) item
            modifyTVar' toInv $ Map.insertWith (+) item amount
            return $ Right ()
        else return $ Left "Insufficient stock"

-- ============================================
-- 並行キュー
-- ============================================

-- | トランザクショナルキュー
type TQueue a = Control.Concurrent.STM.TQueue a

-- | 新しいキューを作成
newTQueue :: IO (TQueue a)
newTQueue = newTQueueIO

-- | キューに書き込み
writeTQueue :: TQueue a -> a -> IO ()
writeTQueue q item = atomically $ Control.Concurrent.STM.writeTQueue q item

-- | キューから読み取り（ブロッキング）
readTQueue :: TQueue a -> IO a
readTQueue q = atomically $ Control.Concurrent.STM.readTQueue q

-- | キューから読み取り（ノンブロッキング）
tryReadTQueue :: TQueue a -> IO (Maybe a)
tryReadTQueue q = atomically $ Control.Concurrent.STM.tryReadTQueue q

-- | キューが空かどうか
isEmptyTQueue :: TQueue a -> IO Bool
isEmptyTQueue q = atomically $ Control.Concurrent.STM.isEmptyTQueue q

-- ============================================
-- Producer-Consumer パターン
-- ============================================

-- | Producer を実行
producer :: TQueue Int -> Int -> IO ()
producer q n = mapM_ (\i -> writeTQueue q i >> threadDelay 10000) [1..n]

-- | Consumer を実行
consumer :: TQueue Int -> TVar Int -> Int -> IO ()
consumer q sumVar n = replicateM_ n $ do
    item <- readTQueue q
    atomically $ modifyTVar' sumVar (+ item)

-- | Producer-Consumer パターンを実行
--
-- >>> result <- producerConsumer 10
-- >>> result
-- 55
producerConsumer :: Int -> IO Int
producerConsumer n = do
    q <- newTQueue
    sumVar <- newTVarIO 0

    -- Producer と Consumer を並行実行
    _ <- forkIO $ producer q n
    consumer q sumVar n

    readTVarIO sumVar

-- ============================================
-- バリア同期
-- ============================================

-- | バリアを表すデータ型
data Barrier = Barrier
    { barrierCount    :: TVar Int
    , barrierTotal    :: Int
    , barrierReleased :: TVar Bool
    }

-- | 新しいバリアを作成
newBarrier :: Int -> IO Barrier
newBarrier total = do
    countVar <- newTVarIO 0
    releasedVar <- newTVarIO False
    return $ Barrier countVar total releasedVar

-- | バリアで待機
waitBarrier :: Barrier -> IO ()
waitBarrier barrier = atomically $ do
    count <- readTVar (barrierCount barrier)
    let newCount = count + 1
    writeTVar (barrierCount barrier) newCount
    if newCount >= barrierTotal barrier
        then writeTVar (barrierReleased barrier) True
        else do
            released <- readTVar (barrierReleased barrier)
            when (not released) retry

-- | バリアをシグナル
signalBarrier :: Barrier -> IO ()
signalBarrier barrier = atomically $
    writeTVar (barrierReleased barrier) True

-- ============================================
-- セマフォ
-- ============================================

-- | セマフォを表すデータ型
type Semaphore = TVar Int

-- | 新しいセマフォを作成
--
-- >>> sem <- newSemaphore 3
-- >>> acquireSemaphore sem
-- >>> acquireSemaphore sem
-- >>> acquireSemaphore sem
-- -- 4回目は待機になる
newSemaphore :: Int -> IO Semaphore
newSemaphore = newTVarIO

-- | セマフォを取得
acquireSemaphore :: Semaphore -> IO ()
acquireSemaphore sem = atomically $ do
    count <- readTVar sem
    if count > 0
        then writeTVar sem (count - 1)
        else retry

-- | セマフォを解放
releaseSemaphore :: Semaphore -> IO ()
releaseSemaphore sem = atomically $ modifyTVar' sem (+1)

-- | セマフォで保護されたアクションを実行
withSemaphore :: Semaphore -> IO a -> IO a
withSemaphore sem action = do
    acquireSemaphore sem
    result <- action
    releaseSemaphore sem
    return result
