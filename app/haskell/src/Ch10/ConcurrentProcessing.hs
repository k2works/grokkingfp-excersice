{-|
Module      : Ch10.ConcurrentProcessing
Description : 第10章: 並行処理
Copyright   : (c) Project Team, 2024
License     : MIT

IORef と並行処理について学ぶ
Haskell では IORef, MVar, TVar などを使って共有状態を管理する
-}
module Ch10.ConcurrentProcessing
    ( -- * IORef - 可変参照
      Counter
    , newCounter
    , incrementCounter
    , getCounter
    , modifyCounter
      -- * 基本的な並行処理
    , forkAndWait
    , raceIO
    , timeoutIO
      -- * 並列実行
    , mapConcurrently'
    , forConcurrently'
    , sequenceConcurrently
      -- * チェックイン集計の例
    , City(..)
    , CityStats(..)
    , CheckInState
    , newCheckInState
    , recordCheckIn
    , getTopCities
    , topCities
      -- * サイコロの並行処理
    , castDieConcurrently
    , castDieNTimes
    , sumDiceConcurrently
      -- * Async - 軽量スレッド
    , AsyncHandle
    , asyncRun
    , asyncWait
    , asyncCancel
    , asyncWithTimeout
      -- * ユーティリティ
    , sleep
    , withTimeout
    , repeatUntilCancelled
    ) where

import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId)
import Control.Concurrent.MVar
import Control.Concurrent.Async (async, wait, cancel, race, Async, mapConcurrently, forConcurrently)
import Data.IORef
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))
import System.Random (randomRIO)
import System.Timeout (timeout)

-- ============================================
-- IORef - 可変参照（Scala の Ref に相当）
-- ============================================

-- | カウンター型（IORef のラッパー）
type Counter = IORef Int

-- | 新しいカウンターを作成
--
-- >>> counter <- newCounter 0
-- >>> getCounter counter
-- 0
newCounter :: Int -> IO Counter
newCounter = newIORef

-- | カウンターをインクリメント
--
-- >>> counter <- newCounter 0
-- >>> incrementCounter counter
-- >>> getCounter counter
-- 1
incrementCounter :: Counter -> IO ()
incrementCounter counter = atomicModifyIORef' counter (\n -> (n + 1, ()))

-- | カウンターの値を取得
getCounter :: Counter -> IO Int
getCounter = readIORef

-- | カウンターを関数で変更
modifyCounter :: Counter -> (Int -> Int) -> IO ()
modifyCounter counter f = atomicModifyIORef' counter (\n -> (f n, ()))

-- ============================================
-- 基本的な並行処理
-- ============================================

-- | 別スレッドで実行し、完了を待つ
--
-- >>> result <- forkAndWait (return 42)
-- >>> result
-- 42
forkAndWait :: IO a -> IO a
forkAndWait action = do
    a <- async action
    wait a

-- | 2つの IO を競争させ、先に完了した方の結果を返す
--
-- >>> result <- raceIO (return "first") (threadDelay 1000000 >> return "second")
-- >>> result
-- Left "first"
raceIO :: IO a -> IO b -> IO (Either a b)
raceIO = race

-- | タイムアウト付きで IO を実行
--
-- >>> result <- timeoutIO 100000 (return 42)
-- >>> result
-- Just 42
timeoutIO :: Int -> IO a -> IO (Maybe a)
timeoutIO = timeout

-- ============================================
-- 並列実行（Scala の parSequence に相当）
-- ============================================

-- | リストの各要素に関数を並列適用
--
-- >>> results <- mapConcurrently' (\x -> return (x * 2)) [1, 2, 3]
-- >>> results
-- [2,4,6]
mapConcurrently' :: (a -> IO b) -> [a] -> IO [b]
mapConcurrently' = mapConcurrently

-- | リストの各要素に対して並列にアクションを実行
forConcurrently' :: [a] -> (a -> IO b) -> IO [b]
forConcurrently' = forConcurrently

-- | IO アクションのリストを並列実行
--
-- >>> results <- sequenceConcurrently [return 1, return 2, return 3]
-- >>> results
-- [1,2,3]
sequenceConcurrently :: [IO a] -> IO [a]
sequenceConcurrently = mapConcurrently id

-- ============================================
-- チェックイン集計の例
-- ============================================

-- | 都市を表すデータ型
newtype City = City { cityName :: String }
    deriving (Show, Eq, Ord)

-- | 都市の統計を表すデータ型
data CityStats = CityStats
    { csCity     :: City
    , csCheckIns :: Int
    } deriving (Show, Eq)

-- | チェックイン状態（IORef のラッパー）
type CheckInState = IORef (Map City Int)

-- | 新しいチェックイン状態を作成
newCheckInState :: IO CheckInState
newCheckInState = newIORef Map.empty

-- | チェックインを記録
--
-- >>> state <- newCheckInState
-- >>> recordCheckIn state (City "Tokyo")
-- >>> recordCheckIn state (City "Tokyo")
-- >>> m <- readIORef state
-- >>> Map.lookup (City "Tokyo") m
-- Just 2
recordCheckIn :: CheckInState -> City -> IO ()
recordCheckIn state city = atomicModifyIORef' state $ \m ->
    (Map.insertWith (+) city 1 m, ())

-- | トップ都市を取得（純粋関数）
--
-- >>> let m = Map.fromList [(City "A", 10), (City "B", 20), (City "C", 5)]
-- >>> topCities 2 m
-- [CityStats {csCity = City {cityName = "B"}, csCheckIns = 20},CityStats {csCity = City {cityName = "A"}, csCheckIns = 10}]
topCities :: Int -> Map City Int -> [CityStats]
topCities n cityCheckIns =
    take n
    . sortBy (\a b -> compare (Down $ csCheckIns a) (Down $ csCheckIns b))
    . map (uncurry CityStats)
    $ Map.toList cityCheckIns

-- | 現在のトップ都市を取得
getTopCities :: Int -> CheckInState -> IO [CityStats]
getTopCities n state = topCities n <$> readIORef state

-- ============================================
-- サイコロの並行処理
-- ============================================

-- | サイコロを振る
castDie :: IO Int
castDie = randomRIO (1, 6)

-- | 2つのサイコロを並行して振る
--
-- >>> results <- castDieConcurrently
-- >>> length results
-- 2
castDieConcurrently :: IO [Int]
castDieConcurrently = sequenceConcurrently [castDie, castDie]

-- | n個のサイコロを並行して振る
--
-- >>> results <- castDieNTimes 5
-- >>> length results
-- 5
castDieNTimes :: Int -> IO [Int]
castDieNTimes n = sequenceConcurrently (replicate n castDie)

-- | n個のサイコロを並行して振り、合計を返す
--
-- >>> total <- sumDiceConcurrently 3
-- >>> total >= 3 && total <= 18
-- True
sumDiceConcurrently :: Int -> IO Int
sumDiceConcurrently n = sum <$> castDieNTimes n

-- ============================================
-- Async - 軽量スレッド（Scala の Fiber に相当）
-- ============================================

-- | Async ハンドル型
type AsyncHandle a = Async a

-- | バックグラウンドでアクションを開始
--
-- >>> handle <- asyncRun (return 42)
-- >>> result <- asyncWait handle
-- >>> result
-- 42
asyncRun :: IO a -> IO (AsyncHandle a)
asyncRun = async

-- | Async の完了を待機
asyncWait :: AsyncHandle a -> IO a
asyncWait = wait

-- | Async をキャンセル
asyncCancel :: AsyncHandle a -> IO ()
asyncCancel = cancel

-- | タイムアウト付きで Async を実行
--
-- >>> result <- asyncWithTimeout 100000 (return 42)
-- >>> result
-- Just 42
asyncWithTimeout :: Int -> IO a -> IO (Maybe a)
asyncWithTimeout microseconds action = do
    handle <- async action
    result <- timeout microseconds (wait handle)
    case result of
        Nothing -> cancel handle >> return Nothing
        Just r  -> return (Just r)

-- ============================================
-- ユーティリティ
-- ============================================

-- | 指定ミリ秒スリープ
--
-- >>> sleep 10  -- 10ms スリープ
sleep :: Int -> IO ()
sleep ms = threadDelay (ms * 1000)

-- | タイムアウト付きで実行（ミリ秒）
withTimeout :: Int -> IO a -> IO (Maybe a)
withTimeout ms = timeout (ms * 1000)

-- | キャンセルされるまで繰り返し実行
repeatUntilCancelled :: IO () -> IO ThreadId
repeatUntilCancelled action = forkIO $ action >> repeatUntilCancelled action
