{-|
Module      : Ch12.TestingStrategiesSpec
Description : 第12章のテスト
-}
module Ch12.TestingStrategiesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Ch12.TestingStrategies
import Data.List (sortBy)
import Data.Ord (Down(..))

spec :: Spec
spec = do
    describe "Ch12.TestingStrategies" $ do
        -- ============================================
        -- ドメインモデル
        -- ============================================
        describe "Domain Model" $ do
            it "creates Location correctly" $ do
                let loc = Location (LocationId "Q123") "Tokyo" 14000000
                locName loc `shouldBe` "Tokyo"
                locPopulation loc `shouldBe` 14000000

            it "creates Attraction correctly" $ do
                let loc = Location (LocationId "Q123") "Tokyo" 14000000
                let attr = Attraction "Tokyo Tower" (Just "Famous tower") loc
                attrName attr `shouldBe` "Tokyo Tower"
                attrDescription attr `shouldBe` Just "Famous tower"

            it "creates TravelGuide correctly" $ do
                let loc = Location (LocationId "Q123") "Tokyo" 14000000
                let attr = Attraction "Tokyo Tower" Nothing loc
                let report = SearchReport 1 []
                let guide = TravelGuide attr ["Artist1", "Movie1"] report
                tgSubjects guide `shouldBe` ["Artist1", "Movie1"]

        -- ============================================
        -- データアクセス層
        -- ============================================
        describe "DataAccess" $ do
            it "mkTestDataAccess returns test data" $ do
                da <- mkTestDataAccess
                attractions <- findAttractions da "Test" AttrByName 10
                length attractions `shouldBe` 1
                attrName (head attractions) `shouldBe` "Test Attraction"

            it "mkTestDataAccess returns empty for unknown name" $ do
                da <- mkTestDataAccess
                attractions <- findAttractions da "Unknown" AttrByName 10
                attractions `shouldBe` []

            it "mkTestDataAccess respects limit" $ do
                da <- mkTestDataAccess
                attractions <- findAttractions da "Test" AttrByName 0
                attractions `shouldBe` []

            it "mkFailingDataAccess returns errors" $ do
                da <- mkFailingDataAccess
                artistsResult <- findArtistsFromLocation da (LocationId "Q123") 10
                artistsResult `shouldBe` Left "Network error"

        -- ============================================
        -- アプリケーションロジック
        -- ============================================
        describe "travelGuide" $ do
            it "returns guide for valid attraction" $ do
                da <- mkTestDataAccess
                guide <- travelGuide da "Test"
                guide `shouldSatisfy` \g -> case g of
                    Just g' -> attrName (tgAttraction g') == "Test Attraction"
                    Nothing -> False

            it "returns Nothing for unknown attraction" $ do
                da <- mkTestDataAccess
                guide <- travelGuide da "Unknown"
                guide `shouldBe` Nothing

            it "includes subjects from artists and movies" $ do
                da <- mkTestDataAccess
                guide <- travelGuide da "Test"
                case guide of
                    Just g -> tgSubjects g `shouldBe` ["Test Artist", "Test Movie"]
                    Nothing -> expectationFailure "Expected Just"

        describe "travelGuideWithReport" $ do
            it "includes search report" $ do
                da <- mkTestDataAccess
                guide <- travelGuideWithReport da "Test"
                case guide of
                    Just g -> srAttractionsSearched (tgSearchReport g) `shouldSatisfy` (> 0)
                    Nothing -> expectationFailure "Expected Just"

            it "collects errors in search report" $ do
                da <- mkFailingDataAccess
                guide <- travelGuideWithReport da "Test"
                case guide of
                    Just g -> do
                        let errors = srErrors (tgSearchReport g)
                        length errors `shouldBe` 2
                        errors `shouldContain` ["Network error"]
                        errors `shouldContain` ["Timeout"]
                    Nothing -> expectationFailure "Expected Just"

            it "still returns guide even with errors" $ do
                da <- mkFailingDataAccess
                guide <- travelGuideWithReport da "Test"
                guide `shouldSatisfy` \g -> case g of
                    Just g' -> attrName (tgAttraction g') == "Test Attraction"
                    Nothing -> False

        -- ============================================
        -- キャッシュ
        -- ============================================
        describe "CachedDataAccess" $ do
            it "caches results" $ do
                da <- mkTestDataAccess
                (da', cache) <- mkCachedDataAccess da
                -- 最初の呼び出し
                result1 <- cachedFindAttractions (da', cache) "Test" AttrByName 10
                -- 2回目の呼び出し（キャッシュから）
                result2 <- cachedFindAttractions (da', cache) "Test" AttrByName 10
                result1 `shouldBe` result2

        -- ============================================
        -- 純粋関数のテスト
        -- ============================================
        describe "filterPopularLocations" $ do
            it "filters by minimum population" $ do
                let locs = [ Location (LocationId "1") "A" 100
                           , Location (LocationId "2") "B" 200
                           , Location (LocationId "3") "C" 150
                           ]
                let filtered = filterPopularLocations locs 150
                length filtered `shouldBe` 2

            it "returns empty for high minimum" $ do
                let locs = [Location (LocationId "1") "A" 100]
                filterPopularLocations locs 1000 `shouldBe` []

            it "returns all for zero minimum" $ do
                let locs = [ Location (LocationId "1") "A" 100
                           , Location (LocationId "2") "B" 200
                           ]
                length (filterPopularLocations locs 0) `shouldBe` 2

        describe "sortAttractionsByPopulation" $ do
            it "sorts by population descending" $ do
                let loc1 = Location (LocationId "1") "A" 100
                let loc2 = Location (LocationId "2") "B" 300
                let loc3 = Location (LocationId "3") "C" 200
                let attrs = [ Attraction "X" Nothing loc1
                            , Attraction "Y" Nothing loc2
                            , Attraction "Z" Nothing loc3
                            ]
                let sorted = sortAttractionsByPopulation attrs
                map attrName sorted `shouldBe` ["Y", "Z", "X"]

            it "handles empty list" $ do
                sortAttractionsByPopulation [] `shouldBe` []

        describe "combineSubjects" $ do
            it "combines artists and movies" $ do
                let artists = [MusicArtist "A1", MusicArtist "A2"]
                let movies = [Movie "M1"]
                combineSubjects artists movies `shouldBe` ["A1", "A2", "M1"]

            it "handles empty lists" $ do
                combineSubjects [] [] `shouldBe` []

        describe "validateLocation" $ do
            it "validates correct location" $ do
                let loc = Location (LocationId "Q1") "Tokyo" 14000000
                validateLocation loc `shouldBe` Right loc

            it "rejects empty ID" $ do
                let loc = Location (LocationId "") "Tokyo" 14000000
                validateLocation loc `shouldBe` Left "Location ID cannot be empty"

            it "rejects empty name" $ do
                let loc = Location (LocationId "Q1") "" 14000000
                validateLocation loc `shouldBe` Left "Location name cannot be empty"

            it "rejects negative population" $ do
                let loc = Location (LocationId "Q1") "Tokyo" (-100)
                validateLocation loc `shouldBe` Left "Population cannot be negative"

        describe "validateAttraction" $ do
            it "validates correct attraction" $ do
                let loc = Location (LocationId "Q1") "Tokyo" 14000000
                let attr = Attraction "Tokyo Tower" Nothing loc
                validateAttraction attr `shouldBe` Right attr

            it "rejects empty name" $ do
                let loc = Location (LocationId "Q1") "Tokyo" 14000000
                let attr = Attraction "" Nothing loc
                validateAttraction attr `shouldBe` Left "Attraction name cannot be empty"

            it "propagates location validation errors" $ do
                let loc = Location (LocationId "") "Tokyo" 14000000
                let attr = Attraction "Tower" Nothing loc
                validateAttraction attr `shouldBe` Left "Location ID cannot be empty"

        -- ============================================
        -- プロパティベーステスト
        -- ============================================
        describe "Property-based tests" $ do
            it "filterPopularLocations result size <= input size" $
                property $ \(locs :: [Location]) (minPop :: Int) ->
                    length (filterPopularLocations locs (abs minPop)) <= length locs

            it "filterPopularLocations all results meet minimum" $
                property $ \(locs :: [Location]) (minPop :: Int) ->
                    let filtered = filterPopularLocations locs (abs minPop)
                    in all (\loc -> locPopulation loc >= abs minPop) filtered

            it "sortAttractionsByPopulation preserves length" $
                property $ \(attrs :: [Attraction]) ->
                    length (sortAttractionsByPopulation attrs) == length attrs

            it "sortAttractionsByPopulation is sorted" $
                property $ \(attrs :: [Attraction]) ->
                    let sorted = sortAttractionsByPopulation attrs
                        pops = map (locPopulation . attrLocation) sorted
                    in pops == sortBy (comparing Down) pops

            it "combineSubjects length is sum of inputs" $
                property $ \(artists :: [MusicArtist]) (movies :: [Movie]) ->
                    length (combineSubjects artists movies) == length artists + length movies

            it "validateLocation accepts valid locations" $
                property $ \loc ->
                    let loc' = loc { locId = LocationId "Q1"
                                   , locName = "City"
                                   , locPopulation = abs (locPopulation loc)
                                   }
                    in validateLocation loc' == Right loc'

-- ============================================
-- Arbitrary インスタンス（QuickCheck 用）
-- ============================================

instance Arbitrary LocationId where
    arbitrary = LocationId <$> listOf1 (elements ['a'..'z'])

instance Arbitrary Location where
    arbitrary = Location
        <$> arbitrary
        <*> listOf1 (elements ['A'..'Z'])
        <*> (abs <$> arbitrary)

instance Arbitrary Attraction where
    arbitrary = Attraction
        <$> listOf1 (elements ['A'..'Z'])
        <*> arbitrary
        <*> arbitrary

instance Arbitrary MusicArtist where
    arbitrary = MusicArtist <$> listOf1 (elements ['A'..'Z'])

instance Arbitrary Movie where
    arbitrary = Movie <$> listOf1 (elements ['A'..'Z'])

-- | comparing 関数（Data.Ord にない場合のため）
comparing :: Ord b => (a -> b) -> a -> a -> Ordering
comparing f x y = compare (f x) (f y)
