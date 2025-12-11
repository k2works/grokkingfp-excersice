{-|
Module      : Ch12.TestingStrategies
Description : 第12章: テスト戦略と実践的なアプリケーション
Copyright   : (c) Project Team, 2024
License     : MIT

テスト戦略と実践的なアプリケーション構築について学ぶ
- TravelGuide アプリケーションの例
- データアクセス層の抽象化
- リソース管理
- プロパティベーステスト
-}
module Ch12.TestingStrategies
    ( -- * ドメインモデル
      LocationId(..)
    , Location(..)
    , Attraction(..)
    , MusicArtist(..)
    , Movie(..)
    , TravelGuide(..)
    , SearchReport(..)
      -- * データアクセス層
    , DataAccess(..)
    , mkTestDataAccess
    , mkFailingDataAccess
      -- * アプリケーションロジック
    , travelGuide
    , travelGuideWithReport
      -- * キャッシュ
    , CachedDataAccess
    , mkCachedDataAccess
    , cachedFindAttractions
      -- * リソース管理
    , withResource
    , bracket'
    , FileHandle
    , withFile'
    , readFileContents
      -- * 純粋関数（テスト対象）
    , filterPopularLocations
    , sortAttractionsByPopulation
    , combineSubjects
    , validateLocation
    , validateAttraction
      -- * プロパティベーステスト用
    , LocationOrdering(..)
    , AttractionOrdering(..)
    ) where

import Control.Exception (bracket, SomeException, try)
import Data.IORef
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down(..))
import System.IO (Handle, openFile, hClose, hGetContents, IOMode(..))

-- ============================================
-- ドメインモデル
-- ============================================

-- | 位置ID（値オブジェクト）
newtype LocationId = LocationId { unLocationId :: String }
    deriving (Show, Eq, Ord)

-- | ロケーション
data Location = Location
    { locId         :: LocationId
    , locName       :: String
    , locPopulation :: Int
    } deriving (Show, Eq)

-- | アトラクション（観光地）
data Attraction = Attraction
    { attrName        :: String
    , attrDescription :: Maybe String
    , attrLocation    :: Location
    } deriving (Show, Eq)

-- | 音楽アーティスト
data MusicArtist = MusicArtist
    { artistName :: String
    } deriving (Show, Eq)

-- | 映画
data Movie = Movie
    { movieName :: String
    } deriving (Show, Eq)

-- | 検索レポート
data SearchReport = SearchReport
    { srAttractionsSearched :: Int
    , srErrors              :: [String]
    } deriving (Show, Eq)

-- | 旅行ガイド
data TravelGuide = TravelGuide
    { tgAttraction   :: Attraction
    , tgSubjects     :: [String]
    , tgSearchReport :: SearchReport
    } deriving (Show, Eq)

-- ============================================
-- ソート順序
-- ============================================

-- | ロケーションのソート順序
data LocationOrdering
    = LocByName
    | LocByPopulation
    deriving (Show, Eq)

-- | アトラクションのソート順序
data AttractionOrdering
    = AttrByName
    | AttrByLocationPopulation
    deriving (Show, Eq)

-- ============================================
-- データアクセス層の抽象化
-- ============================================

-- | データアクセスインターフェース
data DataAccess = DataAccess
    { findAttractions       :: String -> AttractionOrdering -> Int -> IO [Attraction]
    , findArtistsFromLocation :: LocationId -> Int -> IO (Either String [MusicArtist])
    , findMoviesAboutLocation :: LocationId -> Int -> IO (Either String [Movie])
    }

-- | テスト用データアクセスの作成
--
-- >>> da <- mkTestDataAccess
-- >>> attractions <- findAttractions da "Test" AttrByName 10
-- >>> length attractions
-- 1
mkTestDataAccess :: IO DataAccess
mkTestDataAccess = return DataAccess
    { findAttractions = \name _ limit ->
        return $ take limit
            [ Attraction
                { attrName = "Test Attraction"
                , attrDescription = Just "A test attraction"
                , attrLocation = Location (LocationId "Q123") "Test City" 100000
                }
            | name == "Test" || name == ""
            ]
    , findArtistsFromLocation = \_ limit ->
        return $ Right $ take limit [MusicArtist "Test Artist"]
    , findMoviesAboutLocation = \_ limit ->
        return $ Right $ take limit [Movie "Test Movie"]
    }

-- | 失敗するデータアクセスの作成（エラーテスト用）
mkFailingDataAccess :: IO DataAccess
mkFailingDataAccess = return DataAccess
    { findAttractions = \name _ limit ->
        return $ take limit
            [ Attraction
                { attrName = "Test Attraction"
                , attrDescription = Just "A test attraction"
                , attrLocation = Location (LocationId "Q123") "Test City" 100000
                }
            ]
    , findArtistsFromLocation = \_ _ ->
        return $ Left "Network error"
    , findMoviesAboutLocation = \_ _ ->
        return $ Left "Timeout"
    }

-- ============================================
-- アプリケーションロジック
-- ============================================

-- | 旅行ガイドを取得（シンプル版）
--
-- >>> da <- mkTestDataAccess
-- >>> guide <- travelGuide da "Test"
-- >>> fmap (attrName . tgAttraction) guide
-- Just "Test Attraction"
travelGuide :: DataAccess -> String -> IO (Maybe TravelGuide)
travelGuide da attractionName = do
    attractions <- findAttractions da attractionName AttrByLocationPopulation 1
    case attractions of
        [] -> return Nothing
        (attraction:_) -> do
            let locId' = locId $ attrLocation attraction
            artistsResult <- findArtistsFromLocation da locId' 2
            moviesResult <- findMoviesAboutLocation da locId' 2
            let artists = either (const []) id artistsResult
            let movies = either (const []) id moviesResult
            let subjects = map artistName artists ++ map movieName movies
            return $ Just TravelGuide
                { tgAttraction = attraction
                , tgSubjects = subjects
                , tgSearchReport = SearchReport 1 []
                }

-- | 旅行ガイドを取得（SearchReport 付き）
--
-- >>> da <- mkFailingDataAccess
-- >>> guide <- travelGuideWithReport da "Test"
-- >>> fmap (srErrors . tgSearchReport) guide
-- Just ["Network error","Timeout"]
travelGuideWithReport :: DataAccess -> String -> IO (Maybe TravelGuide)
travelGuideWithReport da attractionName = do
    attractions <- findAttractions da attractionName AttrByLocationPopulation 3
    case attractions of
        [] -> return Nothing
        (attraction:_) -> do
            let locId' = locId $ attrLocation attraction
            artistsResult <- findArtistsFromLocation da locId' 2
            moviesResult <- findMoviesAboutLocation da locId' 2

            let errors = collectErrors [artistsResult, moviesResult]
            let artists = either (const []) id artistsResult
            let movies = either (const []) id moviesResult
            let subjects = map artistName artists ++ map movieName movies

            return $ Just TravelGuide
                { tgAttraction = attraction
                , tgSubjects = subjects
                , tgSearchReport = SearchReport (length attractions) errors
                }
  where
    collectErrors :: [Either String a] -> [String]
    collectErrors = foldr (\r acc -> either (:acc) (const acc) r) []

-- ============================================
-- キャッシュ
-- ============================================

-- | キャッシュ付きデータアクセス
type CachedDataAccess = (DataAccess, IORef (Map String [Attraction]))

-- | キャッシュ付きデータアクセスの作成
mkCachedDataAccess :: DataAccess -> IO CachedDataAccess
mkCachedDataAccess da = do
    cache <- newIORef Map.empty
    return (da, cache)

-- | キャッシュ付きアトラクション検索
--
-- >>> da <- mkTestDataAccess
-- >>> (da', cache) <- mkCachedDataAccess da
-- >>> _ <- cachedFindAttractions (da', cache) "Test" AttrByName 10
-- >>> _ <- cachedFindAttractions (da', cache) "Test" AttrByName 10
-- -- 2回目はキャッシュから取得
cachedFindAttractions :: CachedDataAccess -> String -> AttractionOrdering -> Int -> IO [Attraction]
cachedFindAttractions (da, cache) name ordering limit = do
    let key = name ++ "-" ++ show ordering ++ "-" ++ show limit
    cached <- Map.lookup key <$> readIORef cache
    case cached of
        Just attractions -> return attractions
        Nothing -> do
            attractions <- findAttractions da name ordering limit
            atomicModifyIORef' cache (\m -> (Map.insert key attractions m, ()))
            return attractions

-- ============================================
-- リソース管理
-- ============================================

-- | リソースを安全に使用
--
-- >>> withResource (return "resource") (\_ -> return ()) (\r -> return (r ++ "!"))
-- "resource!"
withResource :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b
withResource acquire release = bracket acquire release

-- | bracket のエイリアス
bracket' :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b
bracket' = bracket

-- | ファイルハンドル型
type FileHandle = Handle

-- | ファイルを安全に開く
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode = bracket (openFile path mode) hClose

-- | ファイル内容を読み取る
--
-- ファイルが存在しない場合は Left を返す
readFileContents :: FilePath -> IO (Either String String)
readFileContents path = do
    result <- try $ withFile' path ReadMode hGetContents
    case result of
        Left e  -> return $ Left (show (e :: SomeException))
        Right c -> return $ Right c

-- ============================================
-- 純粋関数（テスト対象）
-- ============================================

-- | 人口でロケーションをフィルタリング
--
-- >>> filterPopularLocations [Location (LocationId "1") "A" 100, Location (LocationId "2") "B" 200] 150
-- [Location {locId = LocationId {unLocationId = "2"}, locName = "B", locPopulation = 200}]
filterPopularLocations :: [Location] -> Int -> [Location]
filterPopularLocations locations minPopulation =
    filter (\loc -> locPopulation loc >= minPopulation) locations

-- | アトラクションを人口でソート
--
-- >>> let a1 = Attraction "A" Nothing (Location (LocationId "1") "X" 100)
-- >>> let a2 = Attraction "B" Nothing (Location (LocationId "2") "Y" 200)
-- >>> map attrName $ sortAttractionsByPopulation [a1, a2]
-- ["B","A"]
sortAttractionsByPopulation :: [Attraction] -> [Attraction]
sortAttractionsByPopulation =
    sortBy (\a b -> compare (Down $ locPopulation $ attrLocation a)
                            (Down $ locPopulation $ attrLocation b))

-- | アーティストと映画の名前を結合
--
-- >>> combineSubjects [MusicArtist "Artist1"] [Movie "Movie1"]
-- ["Artist1","Movie1"]
combineSubjects :: [MusicArtist] -> [Movie] -> [String]
combineSubjects artists movies =
    map artistName artists ++ map movieName movies

-- | ロケーションのバリデーション
--
-- >>> validateLocation (Location (LocationId "Q1") "Tokyo" 14000000)
-- Right (Location {locId = LocationId {unLocationId = "Q1"}, locName = "Tokyo", locPopulation = 14000000})
-- >>> validateLocation (Location (LocationId "") "Tokyo" 14000000)
-- Left "Location ID cannot be empty"
validateLocation :: Location -> Either String Location
validateLocation loc
    | null (unLocationId $ locId loc) = Left "Location ID cannot be empty"
    | null (locName loc) = Left "Location name cannot be empty"
    | locPopulation loc < 0 = Left "Population cannot be negative"
    | otherwise = Right loc

-- | アトラクションのバリデーション
--
-- >>> let loc = Location (LocationId "Q1") "Tokyo" 14000000
-- >>> validateAttraction (Attraction "Tokyo Tower" (Just "Famous tower") loc)
-- Right ...
-- >>> validateAttraction (Attraction "" Nothing loc)
-- Left "Attraction name cannot be empty"
validateAttraction :: Attraction -> Either String Attraction
validateAttraction attr
    | null (attrName attr) = Left "Attraction name cannot be empty"
    | otherwise = case validateLocation (attrLocation attr) of
        Left err -> Left err
        Right _  -> Right attr
