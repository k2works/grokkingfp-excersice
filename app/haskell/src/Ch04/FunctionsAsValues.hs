{-|
Module      : Ch04.FunctionsAsValues
Description : 第4章: 関数を値として扱う
Copyright   : (c) Project Team, 2024
License     : MIT

高階関数、map、filter、fold、関数を返す関数について学ぶ
-}
module Ch04.FunctionsAsValues
    ( -- * 基本的な高階関数
      myMap
    , myFilter
    , myFoldl
      -- * ワードスコアリング
    , score
    , bonus
    , penalty
    , rankedWords
    , totalScore
      -- * 関数を返す関数
    , largerThan
    , divisibleBy
    , containsChar
      -- * プログラミング言語の例
    , ProgrammingLanguage(..)
    , filterByYear
    , getNames
    , sortByYear
      -- * カリー化の例
    , add
    , addCurried
    , multiply3
    ) where

import Data.List (sortBy)
import Data.Ord (comparing)

-- ============================================
-- 基本的な高階関数
-- ============================================

-- | map の自前実装
--
-- >>> myMap (+1) [1,2,3]
-- [2,3,4]
myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs

-- | filter の自前実装
--
-- >>> myFilter even [1,2,3,4,5]
-- [2,4]
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []     = []
myFilter p (x:xs)
    | p x       = x : myFilter p xs
    | otherwise = myFilter p xs

-- | foldl の自前実装
--
-- >>> myFoldl (+) 0 [1,2,3,4,5]
-- 15
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc []     = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- ============================================
-- ワードスコアリング
-- ============================================

-- | 'a' を除いた文字数をスコアとして計算
--
-- >>> score "scala"
-- 3
-- >>> score "rust"
-- 4
score :: String -> Int
score word = length $ filter (/= 'a') word

-- | 'c' を含む場合のボーナス
--
-- >>> bonus "scala"
-- 5
-- >>> bonus "java"
-- 0
bonus :: String -> Int
bonus word = if 'c' `elem` word then 5 else 0

-- | 's' を含む場合のペナルティ
--
-- >>> penalty "rust"
-- 7
-- >>> penalty "java"
-- 0
penalty :: String -> Int
penalty word = if 's' `elem` word then 7 else 0

-- | スコア関数を使って単語をランキング（降順）
--
-- >>> rankedWords score ["ada","haskell","scala","java","rust"]
-- ["haskell","rust","scala","java","ada"]
rankedWords :: (String -> Int) -> [String] -> [String]
rankedWords wordScore words =
    reverse $ sortBy (comparing wordScore) words

-- | 合計スコアを計算
--
-- >>> totalScore score ["java","scala"]
-- 5
totalScore :: (String -> Int) -> [String] -> Int
totalScore wordScore words = sum $ map wordScore words

-- ============================================
-- 関数を返す関数
-- ============================================

-- | n より大きいかを判定する関数を返す
--
-- >>> filter (largerThan 4) [5,1,2,4,0]
-- [5]
largerThan :: Int -> (Int -> Bool)
largerThan n = \i -> i > n

-- | n で割り切れるかを判定する関数を返す
--
-- >>> filter (divisibleBy 3) [1,2,3,6,9,10]
-- [3,6,9]
divisibleBy :: Int -> (Int -> Bool)
divisibleBy n = \i -> i `mod` n == 0

-- | 特定の文字を含むかを判定する関数を返す
--
-- >>> filter (containsChar 's') ["scala","java","rust"]
-- ["scala","rust"]
containsChar :: Char -> (String -> Bool)
containsChar c = \s -> c `elem` s

-- ============================================
-- プログラミング言語の例
-- ============================================

-- | プログラミング言語を表すデータ型
data ProgrammingLanguage = ProgrammingLanguage
    { plName :: String
    , plYear :: Int
    } deriving (Show, Eq)

-- | 指定年より後に作られた言語をフィルタ
--
-- >>> let langs = [ProgrammingLanguage "Java" 1995, ProgrammingLanguage "Scala" 2004]
-- >>> map plName $ filterByYear 2000 langs
-- ["Scala"]
filterByYear :: Int -> [ProgrammingLanguage] -> [ProgrammingLanguage]
filterByYear year = filter (\lang -> plYear lang > year)

-- | 言語名のリストを取得
--
-- >>> let langs = [ProgrammingLanguage "Java" 1995, ProgrammingLanguage "Scala" 2004]
-- >>> getNames langs
-- ["Java","Scala"]
getNames :: [ProgrammingLanguage] -> [String]
getNames = map plName

-- | 年でソート
--
-- >>> let langs = [ProgrammingLanguage "Scala" 2004, ProgrammingLanguage "Java" 1995]
-- >>> map plName $ sortByYear langs
-- ["Java","Scala"]
sortByYear :: [ProgrammingLanguage] -> [ProgrammingLanguage]
sortByYear = sortBy (comparing plYear)

-- ============================================
-- カリー化の例
-- ============================================

-- | 通常の加算（タプルを受け取る）
--
-- >>> add (2, 3)
-- 5
add :: (Int, Int) -> Int
add (a, b) = a + b

-- | カリー化された加算
--
-- >>> addCurried 2 3
-- 5
-- >>> let add5 = addCurried 5
-- >>> add5 3
-- 8
addCurried :: Int -> Int -> Int
addCurried a b = a + b

-- | 3つの引数を取るカリー化された関数
--
-- >>> multiply3 2 3 4
-- 24
-- >>> let double = multiply3 2 1
-- >>> double 5
-- 10
multiply3 :: Int -> Int -> Int -> Int
multiply3 a b c = a * b * c
