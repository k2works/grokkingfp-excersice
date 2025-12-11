{-|
Module      : Ch02.PureFunctions
Description : 第2章: 純粋関数とテスト
Copyright   : (c) Project Team, 2024
License     : MIT

純粋関数、参照透過性、テストの容易さについて学ぶ
-}
module Ch02.PureFunctions
    ( -- * 純粋関数の例
      pureIncrement
    , pureAdd
    , pureGetFirstChar
      -- * ショッピングカートの例
    , getDiscountPercentage
    , calculateDiscount
      -- * チップ計算
    , getTipPercentage
    , calculateTip
      -- * ワードスコア
    , wordScoreWithoutA
    , wordScoreUpperCase
      -- * テスト用ヘルパー
    , isEven
    , absoluteValue
    , safeHead
    , safeTail
      -- * 参照透過性の例
    , referentialTransparencyExample
    ) where

import Data.Char (toUpper)
import Data.List (filter)

-- ============================================
-- 純粋関数の定義
-- ============================================

{-|
純粋関数とは:
1. 同じ入力に対して常に同じ出力を返す
2. 副作用がない（外部状態を変更しない）

Haskell では、デフォルトですべての関数が純粋です。
副作用は IO モナドで明示的に扱います。
-}

-- | 純粋なインクリメント関数
--
-- >>> pureIncrement 5
-- 6
-- >>> pureIncrement 5
-- 6
pureIncrement :: Int -> Int
pureIncrement x = x + 1

-- | 純粋な加算関数
--
-- >>> pureAdd 2 3
-- 5
pureAdd :: Int -> Int -> Int
pureAdd a b = a + b

-- | 純粋な先頭文字取得
--
-- >>> pureGetFirstChar "Hello"
-- 'H'
pureGetFirstChar :: String -> Char
pureGetFirstChar s = head s

-- ============================================
-- ショッピングカートの例（純粋関数版）
-- ============================================

{-|
状態を持つクラスの問題を純粋関数で解決する例

Java の問題のあるコード:
  class ShoppingCartBad {
      private List<String> items = new ArrayList<>();
      private boolean bookAdded = false;
      ...
  }

Haskell では、状態を引数として渡し、新しい状態を返す
-}

-- | カート内のアイテムから割引率を計算する
--
-- >>> getDiscountPercentage ["Apple", "Book", "Pen"]
-- 5
-- >>> getDiscountPercentage ["Apple", "Pen"]
-- 0
getDiscountPercentage :: [String] -> Int
getDiscountPercentage items
    | "Book" `elem` items = 5
    | otherwise           = 0

-- | 割引額を計算する
--
-- >>> calculateDiscount 100 ["Apple", "Book"]
-- 5.0
-- >>> calculateDiscount 100 ["Apple"]
-- 0.0
calculateDiscount :: Double -> [String] -> Double
calculateDiscount price items =
    price * fromIntegral (getDiscountPercentage items) / 100

-- ============================================
-- チップ計算の例
-- ============================================

-- | グループサイズからチップ率を計算する
--
-- >>> getTipPercentage ["Alice", "Bob", "Charlie", "Dave", "Eve", "Frank"]
-- 20
-- >>> getTipPercentage ["Alice", "Bob"]
-- 10
-- >>> getTipPercentage []
-- 0
getTipPercentage :: [String] -> Int
getTipPercentage names
    | length names > 5  = 20
    | length names > 0  = 10
    | otherwise         = 0

-- | チップ額を計算する
--
-- >>> calculateTip 100 ["A", "B", "C"]
-- 10.0
calculateTip :: Double -> [String] -> Double
calculateTip bill names =
    bill * fromIntegral (getTipPercentage names) / 100

-- ============================================
-- ワードスコアの例
-- ============================================

-- | 'a' を除いた文字数をスコアとして計算
--
-- >>> wordScoreWithoutA "Scala"
-- 3
-- >>> wordScoreWithoutA "Haskell"
-- 7
-- >>> wordScoreWithoutA "aaa"
-- 0
wordScoreWithoutA :: String -> Int
wordScoreWithoutA word = length $ Data.List.filter (/= 'a') word

-- | 大文字に変換してからスコアを計算
--
-- >>> wordScoreUpperCase "hello"
-- 5
wordScoreUpperCase :: String -> Int
wordScoreUpperCase word = length $ map toUpper word

-- ============================================
-- テスト用ヘルパー関数
-- ============================================

-- | 偶数かどうかを判定
--
-- >>> isEven 4
-- True
-- >>> isEven 3
-- False
isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

-- | 絶対値を返す
--
-- >>> absoluteValue (-5)
-- 5
-- >>> absoluteValue 5
-- 5
absoluteValue :: Int -> Int
absoluteValue n
    | n < 0     = -n
    | otherwise = n

-- | 安全な先頭要素取得（Maybe を返す）
--
-- >>> safeHead [1,2,3]
-- Just 1
-- >>> safeHead []
-- Nothing
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- | 安全な末尾取得（Maybe を返す）
--
-- >>> safeTail [1,2,3]
-- Just [2,3]
-- >>> safeTail []
-- Nothing
safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

-- ============================================
-- 参照透過性の例
-- ============================================

{-|
参照透過性（Referential Transparency）:
式をその評価結果で置き換えても、プログラムの意味が変わらないこと

例: wordScoreWithoutA "Scala" は常に 3 を返すので、
    wordScoreWithoutA "Scala" + wordScoreWithoutA "Java"
    は 3 + 4 と置き換えられる
-}

-- | 参照透過性のデモンストレーション
--
-- >>> referentialTransparencyExample
-- True
referentialTransparencyExample :: Bool
referentialTransparencyExample =
    let score1 = wordScoreWithoutA "Scala"  -- 常に 3
        score2 = wordScoreWithoutA "Scala"  -- 常に 3
        -- score1 と score2 は常に同じ値
        total1 = wordScoreWithoutA "Scala" + wordScoreWithoutA "Java"
        total2 = 3 + 4  -- 置き換え可能
    in score1 == score2 && total1 == total2
