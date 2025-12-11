{-|
Module      : Ch07.EitherHandling
Description : 第7章: Either 型と複合的なエラー処理
Copyright   : (c) Project Team, 2024
License     : MIT

Either 型を使ったエラーハンドリングと代数的データ型について学ぶ
-}
module Ch07.EitherHandling
    ( -- * TV番組のパース（Either版）
      TvShow(..)
    , parseShowE
    , extractNameE
    , extractYearStartE
    , extractYearEndE
    , extractSingleYearE
      -- * Either の基本操作
    , safeDivideE
    , safeHeadE
    , safeReadIntE
    , validateAge
    , validateEmail
      -- * 音楽アーティストの例
    , Artist(..)
    , MusicGenre(..)
    , Location(..)
    , YearsActive(..)
    , wasArtistActive
    , activeLength
    , searchByGenre
    , searchByOrigin
    , searchByActiveYears
      -- * 検索条件
    , SearchCondition(..)
    , searchArtists
      -- * ユーティリティ
    , orElseE
    , maybeToEither
    , eitherToMaybe
    ) where

import Data.List (elemIndex)
import Text.Read (readMaybe)

-- ============================================
-- TV番組のパース（Either版）
-- ============================================

-- | TV番組を表すデータ型
data TvShow = TvShow
    { tvTitle     :: String
    , tvStartYear :: Int
    , tvEndYear   :: Int
    } deriving (Show, Eq)

-- | 名前を抽出（エラーメッセージ付き）
--
-- >>> extractNameE "Breaking Bad (2008-2013)"
-- Right "Breaking Bad"
-- >>> extractNameE "(2008-2013)"
-- Left "Can't extract name from (2008-2013)"
extractNameE :: String -> Either String String
extractNameE rawShow =
    case elemIndex '(' rawShow of
        Just idx | idx > 0 -> Right $ trim $ take idx rawShow
        _                  -> Left $ "Can't extract name from " ++ rawShow
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- | 開始年を抽出（エラーメッセージ付き）
--
-- >>> extractYearStartE "Breaking Bad (2008-2013)"
-- Right 2008
-- >>> extractYearStartE "Breaking Bad (oops-2013)"
-- Left "Can't parse year: oops"
extractYearStartE :: String -> Either String Int
extractYearStartE rawShow =
    case (elemIndex '(' rawShow, elemIndex '-' rawShow) of
        (Just bo, Just d) | d > bo + 1 ->
            let yearStr = take (d - bo - 1) $ drop (bo + 1) rawShow
            in maybeToEither ("Can't parse year: " ++ yearStr) (readMaybe yearStr)
        _ -> Left $ "Can't extract start year from " ++ rawShow

-- | 終了年を抽出（エラーメッセージ付き）
--
-- >>> extractYearEndE "Breaking Bad (2008-2013)"
-- Right 2013
-- >>> extractYearEndE "Breaking Bad (2008-oops)"
-- Left "Can't parse year: oops"
extractYearEndE :: String -> Either String Int
extractYearEndE rawShow =
    case (elemIndex '-' rawShow, elemIndex ')' rawShow) of
        (Just d, Just bc) | bc > d + 1 ->
            let yearStr = take (bc - d - 1) $ drop (d + 1) rawShow
            in maybeToEither ("Can't parse year: " ++ yearStr) (readMaybe yearStr)
        _ -> Left $ "Can't extract end year from " ++ rawShow

-- | 単年を抽出（エラーメッセージ付き）
--
-- >>> extractSingleYearE "Chernobyl (2019)"
-- Right 2019
-- >>> extractSingleYearE "Breaking Bad (2008-2013)"
-- Left "Can't extract single year from Breaking Bad (2008-2013)"
extractSingleYearE :: String -> Either String Int
extractSingleYearE rawShow =
    case (elemIndex '-' rawShow, elemIndex '(' rawShow, elemIndex ')' rawShow) of
        (Nothing, Just bo, Just bc) | bc > bo + 1 ->
            let yearStr = take (bc - bo - 1) $ drop (bo + 1) rawShow
            in maybeToEither ("Can't parse year: " ++ yearStr) (readMaybe yearStr)
        _ -> Left $ "Can't extract single year from " ++ rawShow

-- | TV番組をパース（エラーメッセージ付き）
--
-- >>> parseShowE "Breaking Bad (2008-2013)"
-- Right (TvShow {tvTitle = "Breaking Bad", tvStartYear = 2008, tvEndYear = 2013})
-- >>> parseShowE "Invalid"
-- Left "Can't extract name from Invalid"
parseShowE :: String -> Either String TvShow
parseShowE rawShow = do
    name      <- extractNameE rawShow
    yearStart <- extractYearStartE rawShow `orElseE` extractSingleYearE rawShow
    yearEnd   <- extractYearEndE rawShow `orElseE` extractSingleYearE rawShow
    return $ TvShow name yearStart yearEnd

-- ============================================
-- Either の基本操作
-- ============================================

-- | 安全な除算（エラーメッセージ付き）
--
-- >>> safeDivideE 10 2
-- Right 5
-- >>> safeDivideE 10 0
-- Left "Division by zero"
safeDivideE :: Int -> Int -> Either String Int
safeDivideE _ 0 = Left "Division by zero"
safeDivideE a b = Right (a `div` b)

-- | 安全な先頭要素取得（エラーメッセージ付き）
--
-- >>> safeHeadE [1,2,3]
-- Right 1
-- >>> safeHeadE ([] :: [Int])
-- Left "Empty list"
safeHeadE :: [a] -> Either String a
safeHeadE []    = Left "Empty list"
safeHeadE (x:_) = Right x

-- | 安全な文字列→整数変換（エラーメッセージ付き）
--
-- >>> safeReadIntE "123"
-- Right 123
-- >>> safeReadIntE "abc"
-- Left "Can't parse 'abc' as Int"
safeReadIntE :: String -> Either String Int
safeReadIntE s = maybeToEither ("Can't parse '" ++ s ++ "' as Int") (readMaybe s)

-- | 年齢のバリデーション
--
-- >>> validateAge 25
-- Right 25
-- >>> validateAge (-5)
-- Left "Age cannot be negative"
-- >>> validateAge 200
-- Left "Age cannot be greater than 150"
validateAge :: Int -> Either String Int
validateAge age
    | age < 0   = Left "Age cannot be negative"
    | age > 150 = Left "Age cannot be greater than 150"
    | otherwise = Right age

-- | メールアドレスのバリデーション
--
-- >>> validateEmail "test@example.com"
-- Right "test@example.com"
-- >>> validateEmail "invalid"
-- Left "Email must contain @"
validateEmail :: String -> Either String String
validateEmail email
    | '@' `notElem` email = Left "Email must contain @"
    | '.' `notElem` email = Left "Email must contain a domain"
    | otherwise           = Right email

-- ============================================
-- 音楽アーティストの例
-- ============================================

-- | 音楽ジャンル
data MusicGenre
    = HeavyMetal
    | Pop
    | HardRock
    | Grunge
    | Punk
    deriving (Show, Eq)

-- | 場所
data Location
    = USA
    | England
    | Australia
    | Canada
    deriving (Show, Eq)

-- | 活動期間
data YearsActive
    = StillActive Int           -- ^ 活動中（開始年）
    | ActiveBetween Int Int     -- ^ 活動終了（開始年、終了年）
    deriving (Show, Eq)

-- | アーティスト
data Artist = Artist
    { artistName        :: String
    , artistGenre       :: MusicGenre
    , artistOrigin      :: Location
    , artistYearsActive :: YearsActive
    } deriving (Show, Eq)

-- | アーティストが指定期間に活動していたかを判定
--
-- >>> wasArtistActive (Artist "Test" HeavyMetal USA (StillActive 1980)) 1990 2000
-- True
-- >>> wasArtistActive (Artist "Test" HeavyMetal USA (ActiveBetween 1980 1985)) 1990 2000
-- False
wasArtistActive :: Artist -> Int -> Int -> Bool
wasArtistActive artist yearStart yearEnd =
    case artistYearsActive artist of
        StillActive since        -> since <= yearEnd
        ActiveBetween start end_ -> start <= yearEnd && end_ >= yearStart

-- | アーティストの活動年数を計算
--
-- >>> activeLength (Artist "Test" HeavyMetal USA (ActiveBetween 1980 1990)) 2024
-- 10
-- >>> activeLength (Artist "Test" HeavyMetal USA (StillActive 2000)) 2024
-- 24
activeLength :: Artist -> Int -> Int
activeLength artist currentYear =
    case artistYearsActive artist of
        StillActive since        -> currentYear - since
        ActiveBetween start end_ -> end_ - start

-- | ジャンルで検索
searchByGenre :: [MusicGenre] -> [Artist] -> [Artist]
searchByGenre genres = filter (\a -> artistGenre a `elem` genres)

-- | 出身地で検索
searchByOrigin :: [Location] -> [Artist] -> [Artist]
searchByOrigin locations = filter (\a -> artistOrigin a `elem` locations)

-- | 活動期間で検索
searchByActiveYears :: Int -> Int -> [Artist] -> [Artist]
searchByActiveYears yearStart yearEnd =
    filter (\a -> wasArtistActive a yearStart yearEnd)

-- ============================================
-- 検索条件
-- ============================================

-- | 検索条件を表す代数的データ型
data SearchCondition
    = SearchByGenre [MusicGenre]
    | SearchByOrigin [Location]
    | SearchByActiveYears Int Int
    deriving (Show, Eq)

-- | 複合検索（全条件を満たすアーティストを検索）
--
-- >>> let artists = [Artist "Metallica" HeavyMetal USA (StillActive 1981)]
-- >>> searchArtists artists [SearchByGenre [HeavyMetal], SearchByOrigin [USA]]
-- [Artist {artistName = "Metallica", artistGenre = HeavyMetal, artistOrigin = USA, artistYearsActive = StillActive 1981}]
searchArtists :: [Artist] -> [SearchCondition] -> [Artist]
searchArtists artists conditions =
    filter (satisfiesAll conditions) artists
  where
    satisfiesAll :: [SearchCondition] -> Artist -> Bool
    satisfiesAll conds artist = all (satisfies artist) conds

    satisfies :: Artist -> SearchCondition -> Bool
    satisfies artist (SearchByGenre genres) =
        artistGenre artist `elem` genres
    satisfies artist (SearchByOrigin locations) =
        artistOrigin artist `elem` locations
    satisfies artist (SearchByActiveYears start end_) =
        wasArtistActive artist start end_

-- ============================================
-- ユーティリティ
-- ============================================

-- | 最初の Either が Left なら代替を使用
--
-- >>> Right 5 `orElseE` Right 10
-- Right 5
-- >>> Left "error" `orElseE` Right 10
-- Right 10
orElseE :: Either e a -> Either e a -> Either e a
orElseE (Right x) _ = Right x
orElseE (Left _)  y = y

-- | Maybe を Either に変換
--
-- >>> maybeToEither "error" (Just 5)
-- Right 5
-- >>> maybeToEither "error" Nothing
-- Left "error"
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither _ (Just x) = Right x
maybeToEither e Nothing  = Left e

-- | Either を Maybe に変換
--
-- >>> eitherToMaybe (Right 5)
-- Just 5
-- >>> eitherToMaybe (Left "error")
-- Nothing
eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _)  = Nothing
