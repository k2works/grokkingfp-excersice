{-|
Module      : Ch06.MaybeHandling
Description : 第6章: Maybe 型による安全なエラーハンドリング
Copyright   : (c) Project Team, 2024
License     : MIT

Maybe 型を使った安全なエラーハンドリングについて学ぶ
-}
module Ch06.MaybeHandling
    ( -- * TV番組のパース
      TvShow(..)
    , parseShow
    , parseShowSafe
    , extractName
    , extractYearStart
    , extractYearEnd
    , extractSingleYear
      -- * Maybe の基本操作
    , safeDivide
    , safeHead
    , safeLast
    , safeIndex
    , safeReadInt
      -- * Maybe の合成
    , addStrings
    , multiplyStrings
    , chainedLookup
      -- * エラーハンドリング戦略
    , parseShowsBestEffort
    , parseShowsAllOrNothing
      -- * orElse 相当の操作
    , orElse
    , firstJust
    ) where

import Data.Char (isDigit)
import Data.List (elemIndex)
import Text.Read (readMaybe)

-- ============================================
-- TV番組のパース
-- ============================================

-- | TV番組を表すデータ型
data TvShow = TvShow
    { tvTitle     :: String
    , tvStartYear :: Int
    , tvEndYear   :: Int
    } deriving (Show, Eq)

-- | 名前を抽出
--
-- >>> extractName "Breaking Bad (2008-2013)"
-- Just "Breaking Bad"
-- >>> extractName "(2008-2013)"
-- Nothing
extractName :: String -> Maybe String
extractName rawShow =
    let bracketOpen = elemIndex '(' rawShow
    in case bracketOpen of
        Just idx | idx > 0 -> Just $ trim $ take idx rawShow
        _                  -> Nothing
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- | 開始年を抽出
--
-- >>> extractYearStart "Breaking Bad (2008-2013)"
-- Just 2008
-- >>> extractYearStart "Breaking Bad (oops-2013)"
-- Nothing
extractYearStart :: String -> Maybe Int
extractYearStart rawShow = do
    bracketOpen <- elemIndex '(' rawShow
    dash        <- elemIndex '-' rawShow
    if dash > bracketOpen + 1
        then readMaybe $ take (dash - bracketOpen - 1) $ drop (bracketOpen + 1) rawShow
        else Nothing

-- | 終了年を抽出
--
-- >>> extractYearEnd "Breaking Bad (2008-2013)"
-- Just 2013
-- >>> extractYearEnd "Breaking Bad (2008-oops)"
-- Nothing
extractYearEnd :: String -> Maybe Int
extractYearEnd rawShow = do
    dash         <- elemIndex '-' rawShow
    bracketClose <- elemIndex ')' rawShow
    if bracketClose > dash + 1
        then readMaybe $ take (bracketClose - dash - 1) $ drop (dash + 1) rawShow
        else Nothing

-- | 単年を抽出（ダッシュがない場合）
--
-- >>> extractSingleYear "Chernobyl (2019)"
-- Just 2019
-- >>> extractSingleYear "Breaking Bad (2008-2013)"
-- Nothing
extractSingleYear :: String -> Maybe Int
extractSingleYear rawShow =
    let dash         = elemIndex '-' rawShow
        bracketOpen  = elemIndex '(' rawShow
        bracketClose = elemIndex ')' rawShow
    in case (dash, bracketOpen, bracketClose) of
        (Nothing, Just bo, Just bc) | bc > bo + 1 ->
            readMaybe $ take (bc - bo - 1) $ drop (bo + 1) rawShow
        _ -> Nothing

-- | TV番組をパース（危険な実装 - 例外をスロー）
parseShow :: String -> TvShow
parseShow rawShow =
    let Just name      = extractName rawShow
        Just yearStart = extractYearStart rawShow
        Just yearEnd   = extractYearEnd rawShow
    in TvShow name yearStart yearEnd

-- | TV番組をパース（安全な実装）
--
-- >>> parseShowSafe "Breaking Bad (2008-2013)"
-- Just (TvShow {tvTitle = "Breaking Bad", tvStartYear = 2008, tvEndYear = 2013})
-- >>> parseShowSafe "Chernobyl (2019)"
-- Just (TvShow {tvTitle = "Chernobyl", tvStartYear = 2019, tvEndYear = 2019})
-- >>> parseShowSafe "Invalid"
-- Nothing
parseShowSafe :: String -> Maybe TvShow
parseShowSafe rawShow = do
    name      <- extractName rawShow
    yearStart <- extractYearStart rawShow `orElse` extractSingleYear rawShow
    yearEnd   <- extractYearEnd rawShow `orElse` extractSingleYear rawShow
    return $ TvShow name yearStart yearEnd

-- ============================================
-- Maybe の基本操作
-- ============================================

-- | 安全な除算
--
-- >>> safeDivide 10 2
-- Just 5
-- >>> safeDivide 10 0
-- Nothing
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide a b = Just (a `div` b)

-- | 安全な先頭要素取得
--
-- >>> safeHead [1,2,3]
-- Just 1
-- >>> safeHead []
-- Nothing
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- | 安全な末尾要素取得
--
-- >>> safeLast [1,2,3]
-- Just 3
-- >>> safeLast []
-- Nothing
safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs

-- | 安全なインデックスアクセス
--
-- >>> safeIndex 1 [1,2,3]
-- Just 2
-- >>> safeIndex 10 [1,2,3]
-- Nothing
safeIndex :: Int -> [a] -> Maybe a
safeIndex _ []     = Nothing
safeIndex 0 (x:_)  = Just x
safeIndex n (_:xs) = safeIndex (n - 1) xs

-- | 安全な文字列→整数変換
--
-- >>> safeReadInt "123"
-- Just 123
-- >>> safeReadInt "abc"
-- Nothing
safeReadInt :: String -> Maybe Int
safeReadInt = readMaybe

-- ============================================
-- Maybe の合成
-- ============================================

-- | 2つの文字列を整数として加算
--
-- >>> addStrings "10" "20"
-- Just 30
-- >>> addStrings "10" "abc"
-- Nothing
addStrings :: String -> String -> Maybe Int
addStrings a b = do
    x <- safeReadInt a
    y <- safeReadInt b
    return (x + y)

-- | 2つの文字列を整数として乗算
--
-- >>> multiplyStrings "3" "4"
-- Just 12
-- >>> multiplyStrings "3" "abc"
-- Nothing
multiplyStrings :: String -> String -> Maybe Int
multiplyStrings a b = do
    x <- safeReadInt a
    y <- safeReadInt b
    return (x * y)

-- | 連鎖的なルックアップ
--
-- >>> chainedLookup "a" [("a","b"),("b","c"),("c","found")]
-- Just "found"
-- >>> chainedLookup "x" [("a","b"),("b","c"),("c","found")]
-- Nothing
chainedLookup :: Eq k => k -> [(k, k)] -> Maybe k
chainedLookup key assocs = go key 3  -- 最大3回の連鎖
  where
    go _ 0 = Nothing
    go k n = case lookup k assocs of
        Just v  -> Just v `orElse` go v (n - 1)
        Nothing -> Nothing

-- ============================================
-- エラーハンドリング戦略
-- ============================================

-- | Best-effort 戦略: パースできたものだけ返す
--
-- >>> parseShowsBestEffort ["Breaking Bad (2008-2013)", "Invalid", "Mad Men (2007-2015)"]
-- [TvShow {tvTitle = "Breaking Bad", tvStartYear = 2008, tvEndYear = 2013},TvShow {tvTitle = "Mad Men", tvStartYear = 2007, tvEndYear = 2015}]
parseShowsBestEffort :: [String] -> [TvShow]
parseShowsBestEffort rawShows =
    concatMap (maybe [] (:[]) . parseShowSafe) rawShows

-- | All-or-nothing 戦略: 全部成功するか、全部失敗
--
-- >>> parseShowsAllOrNothing ["Breaking Bad (2008-2013)", "Mad Men (2007-2015)"]
-- Just [TvShow {tvTitle = "Breaking Bad", tvStartYear = 2008, tvEndYear = 2013},TvShow {tvTitle = "Mad Men", tvStartYear = 2007, tvEndYear = 2015}]
-- >>> parseShowsAllOrNothing ["Breaking Bad (2008-2013)", "Invalid"]
-- Nothing
parseShowsAllOrNothing :: [String] -> Maybe [TvShow]
parseShowsAllOrNothing = traverse parseShowSafe

-- ============================================
-- orElse 相当の操作
-- ============================================

-- | 最初の Maybe が Nothing なら代替を使用
--
-- >>> Just 5 `orElse` Just 10
-- Just 5
-- >>> Nothing `orElse` Just 10
-- Just 10
orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing  y = y

-- | リストの中から最初の Just を見つける
--
-- >>> firstJust [Nothing, Just 5, Just 10]
-- Just 5
-- >>> firstJust [Nothing, Nothing]
-- Nothing
firstJust :: [Maybe a] -> Maybe a
firstJust []           = Nothing
firstJust (Just x : _) = Just x
firstJust (Nothing : xs) = firstJust xs
