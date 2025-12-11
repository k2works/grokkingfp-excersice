{-|
Module      : Ch09.StreamProcessing
Description : 第9章: ストリーム処理
Copyright   : (c) Project Team, 2024
License     : MIT

遅延リストを使ったストリーム処理について学ぶ
Haskell では遅延評価により、標準のリストが無限ストリームとして機能する
-}
module Ch09.StreamProcessing
    ( -- * 無限ストリームの基本
      naturals
    , evens
    , odds
    , fibs
    , primes
      -- * ストリーム生成
    , repeatVal
    , cycleList
    , iterate'
    , unfold
      -- * ストリーム操作
    , takeWhile'
    , dropWhile'
    , sliding
    , chunksOf
      -- * トレンド検出
    , trending
    , isStable
    , findTrend
      -- * 通貨交換レートの例
    , ExchangeRate
    , simulateRates
    , findUptrend
    , exchangeIfTrending
      -- * ストリーム合成
    , zipWithIndex
    , interleave
    , merge
    ) where

import Data.List (tails)

-- ============================================
-- 無限ストリームの基本
-- ============================================

-- | 自然数の無限ストリーム
--
-- >>> take 10 naturals
-- [1,2,3,4,5,6,7,8,9,10]
naturals :: [Integer]
naturals = [1..]

-- | 偶数の無限ストリーム
--
-- >>> take 5 evens
-- [2,4,6,8,10]
evens :: [Integer]
evens = [2,4..]

-- | 奇数の無限ストリーム
--
-- >>> take 5 odds
-- [1,3,5,7,9]
odds :: [Integer]
odds = [1,3..]

-- | フィボナッチ数列の無限ストリーム
--
-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | 素数の無限ストリーム（エラトステネスのふるい）
--
-- >>> take 10 primes
-- [2,3,5,7,11,13,17,19,23,29]
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve []     = []

-- ============================================
-- ストリーム生成
-- ============================================

-- | 同じ値を無限に繰り返す
--
-- >>> take 5 (repeatVal 1)
-- [1,1,1,1,1]
repeatVal :: a -> [a]
repeatVal x = x : repeatVal x

-- | リストを無限に繰り返す
--
-- >>> take 7 (cycleList [1,2,3])
-- [1,2,3,1,2,3,1]
cycleList :: [a] -> [a]
cycleList xs = xs ++ cycleList xs

-- | 関数を繰り返し適用して無限リストを生成
--
-- >>> take 5 (iterate' (*2) 1)
-- [1,2,4,8,16]
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

-- | unfold: 種から無限リストを生成
--
-- >>> take 5 (unfold (\n -> Just (n, n+1)) 0)
-- [0,1,2,3,4]
unfold :: (b -> Maybe (a, b)) -> b -> [a]
unfold f seed = case f seed of
    Nothing      -> []
    Just (a, b') -> a : unfold f b'

-- ============================================
-- ストリーム操作
-- ============================================

-- | 条件を満たす間、要素を取得
--
-- >>> takeWhile' (<5) [1,2,3,4,5,6]
-- [1,2,3,4]
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []     = []
takeWhile' p (x:xs)
    | p x       = x : takeWhile' p xs
    | otherwise = []

-- | 条件を満たす間、要素をスキップ
--
-- >>> dropWhile' (<5) [1,2,3,4,5,6]
-- [5,6]
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []     = []
dropWhile' p xs@(x:xs')
    | p x       = dropWhile' p xs'
    | otherwise = xs

-- | スライディングウィンドウ
--
-- >>> sliding 3 [1,2,3,4,5]
-- [[1,2,3],[2,3,4],[3,4,5]]
sliding :: Int -> [a] -> [[a]]
sliding n xs
    | length window < n = []
    | otherwise         = window : sliding n (tail xs)
  where
    window = take n xs

-- | 固定サイズのチャンクに分割
--
-- >>> chunksOf 3 [1,2,3,4,5,6,7,8]
-- [[1,2,3],[4,5,6],[7,8]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- ============================================
-- トレンド検出
-- ============================================

-- | 上昇トレンドかどうかを判定
--
-- >>> trending [0.81, 0.82, 0.83]
-- True
-- >>> trending [0.81, 0.84, 0.83]
-- False
trending :: Ord a => [a] -> Bool
trending xs =
    length xs > 1 &&
    all (uncurry (<)) (zip xs (tail xs))

-- | 安定（全て同じ値）かどうかを判定
--
-- >>> isStable [5, 5, 5]
-- True
-- >>> isStable [5, 5, 6]
-- False
isStable :: Eq a => [a] -> Bool
isStable xs =
    length xs >= 3 &&
    all (== head xs) xs

-- | ストリームから上昇トレンドを検出
--
-- >>> findTrend 3 [1, 2, 3, 2, 3, 4, 5]
-- Just [2,3,4]
findTrend :: Ord a => Int -> [a] -> Maybe [a]
findTrend windowSize xs =
    case filter trending (sliding windowSize xs) of
        []    -> Nothing
        (w:_) -> Just w

-- ============================================
-- 通貨交換レートの例
-- ============================================

-- | 為替レート型
type ExchangeRate = Double

-- | 為替レートをシミュレート（無限ストリーム）
--
-- 初期値から小さな変動を加えて無限にレートを生成
simulateRates :: ExchangeRate -> [ExchangeRate]
simulateRates initial = iterate' vary initial
  where
    -- シンプルな変動シミュレーション（決定論的）
    vary rate = rate + sin (rate * 100) * 0.01

-- | 上昇トレンドを検出するまでレートを監視
--
-- >>> findUptrend 3 [0.80, 0.81, 0.82, 0.83, 0.80]
-- Just [0.81,0.82,0.83]
findUptrend :: Int -> [ExchangeRate] -> Maybe [ExchangeRate]
findUptrend = findTrend

-- | 上昇トレンドを検出したら交換
--
-- >>> exchangeIfTrending 100 3 [0.80, 0.81, 0.82, 0.83, 0.80]
-- Just 83.0
exchangeIfTrending :: Double -> Int -> [ExchangeRate] -> Maybe Double
exchangeIfTrending amount windowSize rates =
    case findUptrend windowSize rates of
        Just trend -> Just $ amount * last trend
        Nothing    -> Nothing

-- ============================================
-- ストリーム合成
-- ============================================

-- | インデックス付きストリーム
--
-- >>> take 3 (zipWithIndex ['a', 'b', 'c'])
-- [(0,'a'),(1,'b'),(2,'c')]
zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0..]

-- | 2つのストリームを交互に合成
--
-- >>> take 6 (interleave [1,3,5] [2,4,6])
-- [1,2,3,4,5,6]
interleave :: [a] -> [a] -> [a]
interleave []     ys     = ys
interleave xs     []     = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- | 2つのソート済みストリームをマージ
--
-- >>> take 10 (merge [1,3,5,7,9] [2,4,6,8,10])
-- [1,2,3,4,5,6,7,8,9,10]
merge :: Ord a => [a] -> [a] -> [a]
merge []     ys     = ys
merge xs     []     = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys
