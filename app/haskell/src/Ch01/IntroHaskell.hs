{-|
Module      : Ch01.IntroHaskell
Description : 第1章: Haskell 入門
Copyright   : (c) Project Team, 2024
License     : MIT

関数型プログラミング入門 - Haskell の基本構文と考え方
-}
module Ch01.IntroHaskell
    ( -- * 基本的な関数
      increment
    , getFirstCharacter
    , wordScore
      -- * 命令型 vs 関数型の例
    , calculateScoreImperative
    , calculateScoreFunctional
      -- * Haskell の特徴
    , add
    , multiply
    , isPositive
    , greet
      -- * 遅延評価の例
    , infiniteOnes
    , takeFive
    ) where

import Data.Char (toUpper)

-- ============================================
-- 基本的な関数定義
-- ============================================

-- | 数値をインクリメントする
--
-- >>> increment 5
-- 6
increment :: Int -> Int
increment x = x + 1

-- | 文字列の最初の文字を取得する
--
-- >>> getFirstCharacter "Hello"
-- 'H'
getFirstCharacter :: String -> Char
getFirstCharacter s = head s

-- | 単語のスコアを計算する（文字数）
--
-- >>> wordScore "Haskell"
-- 7
wordScore :: String -> Int
wordScore word = length word

-- ============================================
-- 命令型 vs 関数型の比較
-- ============================================

-- | 命令型スタイル（Haskell でも書けるが推奨されない）
-- 累積変数を使った実装
calculateScoreImperative :: String -> Int
calculateScoreImperative word = go word 0
  where
    go [] acc = acc
    go (_:xs) acc = go xs (acc + 1)

-- | 関数型スタイル（推奨）
-- 宣言的に「何をするか」を記述
calculateScoreFunctional :: String -> Int
calculateScoreFunctional = length

-- ============================================
-- Haskell の基本的な関数
-- ============================================

-- | 2つの数を加算する
--
-- >>> add 2 3
-- 5
add :: Int -> Int -> Int
add a b = a + b

-- | 2つの数を乗算する
--
-- >>> multiply 4 5
-- 20
multiply :: Int -> Int -> Int
multiply a b = a * b

-- | 数値が正かどうかを判定する
--
-- >>> isPositive 5
-- True
-- >>> isPositive (-3)
-- False
isPositive :: Int -> Bool
isPositive n = n > 0

-- | 名前で挨拶する
--
-- >>> greet "World"
-- "Hello, World!"
greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

-- ============================================
-- 遅延評価の例
-- ============================================

-- | 無限の 1 のリスト
-- Haskell の遅延評価により、必要な分だけ評価される
infiniteOnes :: [Int]
infiniteOnes = repeat 1

-- | 無限リストから最初の 5 要素を取得
--
-- >>> takeFive
-- [1,1,1,1,1]
takeFive :: [Int]
takeFive = take 5 infiniteOnes

-- ============================================
-- 型推論の例
-- ============================================

-- 型注釈なしでも Haskell は型を推論できる
-- 以下の関数は型注釈がなくても動作する

-- | 文字を大文字に変換
--
-- >>> toUpperChar 'a'
-- 'A'
toUpperChar :: Char -> Char
toUpperChar = toUpper

-- | リストの要素数を2倍にする
--
-- >>> doubleLength [1,2,3]
-- 6
doubleLength :: [a] -> Int
doubleLength xs = length xs * 2
