{-|
Module      : Ch03.ImmutableValues
Description : 第3章: イミュータブルなデータ操作
Copyright   : (c) Project Team, 2024
License     : MIT

イミュータブルなリスト操作、スライス、結合について学ぶ
-}
module Ch03.ImmutableValues
    ( -- * リストの基本操作
      appended
    , appendedAll
    , slice
    , firstTwo
    , lastTwo
      -- * リストの変換
    , movedFirstTwoToTheEnd
    , insertedBeforeLast
    , insertAtMiddle
      -- * 旅程の例
    , replan
      -- * 文字列操作
    , abbreviate
    ) where

-- ============================================
-- リストの基本操作
-- ============================================

-- | 要素を末尾に追加（新しいリストを返す）
--
-- >>> appended [1,2,3] 4
-- [1,2,3,4]
appended :: [a] -> a -> [a]
appended xs x = xs ++ [x]

-- | リストを末尾に結合（新しいリストを返す）
--
-- >>> appendedAll [1,2] [3,4]
-- [1,2,3,4]
appendedAll :: [a] -> [a] -> [a]
appendedAll = (++)

-- | リストの一部を切り出す（start <= index < end）
--
-- >>> slice 1 3 ["a","b","c","d"]
-- ["b","c"]
slice :: Int -> Int -> [a] -> [a]
slice start end xs = take (end - start) $ drop start xs

-- | 最初の2要素を取得
--
-- >>> firstTwo ["a","b","c"]
-- ["a","b"]
firstTwo :: [a] -> [a]
firstTwo = take 2

-- | 最後の2要素を取得
--
-- >>> lastTwo ["a","b","c"]
-- ["b","c"]
lastTwo :: [a] -> [a]
lastTwo xs = drop (length xs - 2) xs

-- ============================================
-- リストの変換
-- ============================================

-- | 最初の2要素を末尾に移動
--
-- >>> movedFirstTwoToTheEnd ["a","b","c"]
-- ["c","a","b"]
movedFirstTwoToTheEnd :: [a] -> [a]
movedFirstTwoToTheEnd xs =
    let first = take 2 xs
        rest  = drop 2 xs
    in rest ++ first

-- | 最後の要素の前に挿入
--
-- >>> insertedBeforeLast ["a","b"] "c"
-- ["a","c","b"]
insertedBeforeLast :: [a] -> a -> [a]
insertedBeforeLast xs element =
    let lastElem    = [last xs]
        withoutLast = init xs
    in withoutLast ++ [element] ++ lastElem

-- | 中央に要素を挿入
--
-- >>> insertAtMiddle ["a","b","c","d"] "X"
-- ["a","b","X","c","d"]
insertAtMiddle :: [a] -> a -> [a]
insertAtMiddle xs element =
    let middle = length xs `div` 2
        before = take middle xs
        after  = drop middle xs
    in before ++ [element] ++ after

-- ============================================
-- 旅程の例
-- ============================================

-- | 旅程を再計画する（指定都市の前に新都市を挿入）
--
-- >>> replan ["Paris","Berlin","Krakow"] "Vienna" "Krakow"
-- ["Paris","Berlin","Vienna","Krakow"]
replan :: [String] -> String -> String -> [String]
replan plan newCity beforeCity =
    let beforeCityIndex = findIndex beforeCity plan
        citiesBefore    = take beforeCityIndex plan
        citiesAfter     = drop beforeCityIndex plan
    in citiesBefore ++ [newCity] ++ citiesAfter
  where
    findIndex :: Eq a => a -> [a] -> Int
    findIndex _ []     = 0
    findIndex x (y:ys)
        | x == y    = 0
        | otherwise = 1 + findIndex x ys

-- ============================================
-- 文字列操作
-- ============================================

-- | 名前を省略形にする
--
-- >>> abbreviate "Alonzo Church"
-- "A. Church"
abbreviate :: String -> String
abbreviate name =
    let initial   = take 1 name
        separator = findSpaceIndex name
        lastName  = drop (separator + 1) name
    in initial ++ ". " ++ lastName
  where
    findSpaceIndex :: String -> Int
    findSpaceIndex []       = 0
    findSpaceIndex (' ':_)  = 0
    findSpaceIndex (_:rest) = 1 + findSpaceIndex rest
