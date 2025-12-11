{-|
Module      : Ch07.EitherHandlingSpec
Description : 第7章のテスト
-}
module Ch07.EitherHandlingSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Ch07.EitherHandling

spec :: Spec
spec = do
    describe "Ch07.EitherHandling" $ do
        -- ============================================
        -- TV番組のパース（Either版）
        -- ============================================
        describe "extractNameE" $ do
            it "extracts name from valid input" $ do
                extractNameE "Breaking Bad (2008-2013)" `shouldBe` Right "Breaking Bad"

            it "returns Left with error message for invalid input" $ do
                extractNameE "(2008-2013)" `shouldBe` Left "Can't extract name from (2008-2013)"

        describe "extractYearStartE" $ do
            it "extracts start year" $ do
                extractYearStartE "Breaking Bad (2008-2013)" `shouldBe` Right 2008

            it "returns Left for unparseable year" $ do
                case extractYearStartE "Breaking Bad (oops-2013)" of
                    Left msg -> msg `shouldContain` "Can't parse"
                    Right _  -> expectationFailure "Expected Left"

        describe "extractYearEndE" $ do
            it "extracts end year" $ do
                extractYearEndE "Breaking Bad (2008-2013)" `shouldBe` Right 2013

            it "returns Left for invalid format" $ do
                case extractYearEndE "Breaking Bad 2008-2013" of
                    Left _  -> return ()
                    Right _ -> expectationFailure "Expected Left"

        describe "extractSingleYearE" $ do
            it "extracts single year" $ do
                extractSingleYearE "Chernobyl (2019)" `shouldBe` Right 2019

            it "returns Left when dash present" $ do
                case extractSingleYearE "Breaking Bad (2008-2013)" of
                    Left msg -> msg `shouldContain` "Can't extract single year"
                    Right _  -> expectationFailure "Expected Left"

        describe "parseShowE" $ do
            it "parses valid show with range" $ do
                parseShowE "Breaking Bad (2008-2013)"
                    `shouldBe` Right (TvShow "Breaking Bad" 2008 2013)

            it "parses valid show with single year" $ do
                parseShowE "Chernobyl (2019)"
                    `shouldBe` Right (TvShow "Chernobyl" 2019 2019)

            it "returns Left with descriptive error" $ do
                case parseShowE "Invalid" of
                    Left msg -> msg `shouldContain` "Can't extract name"
                    Right _  -> expectationFailure "Expected Left"

        -- ============================================
        -- Either の基本操作
        -- ============================================
        describe "safeDivideE" $ do
            it "divides when divisor is non-zero" $ do
                safeDivideE 10 2 `shouldBe` Right 5

            it "returns Left with message when divisor is zero" $ do
                safeDivideE 10 0 `shouldBe` Left "Division by zero"

        describe "safeHeadE" $ do
            it "returns Right first element" $ do
                safeHeadE [1,2,3 :: Int] `shouldBe` Right 1

            it "returns Left for empty list" $ do
                safeHeadE ([] :: [Int]) `shouldBe` Left "Empty list"

        describe "safeReadIntE" $ do
            it "parses valid integer" $ do
                safeReadIntE "123" `shouldBe` Right 123

            it "returns Left with descriptive error" $ do
                safeReadIntE "abc" `shouldBe` Left "Can't parse 'abc' as Int"

        describe "validateAge" $ do
            it "accepts valid age" $ do
                validateAge 25 `shouldBe` Right 25
                validateAge 0 `shouldBe` Right 0
                validateAge 150 `shouldBe` Right 150

            it "rejects negative age" $ do
                validateAge (-5) `shouldBe` Left "Age cannot be negative"

            it "rejects unrealistic age" $ do
                validateAge 200 `shouldBe` Left "Age cannot be greater than 150"

        describe "validateEmail" $ do
            it "accepts valid email" $ do
                validateEmail "test@example.com" `shouldBe` Right "test@example.com"

            it "rejects email without @" $ do
                validateEmail "invalid" `shouldBe` Left "Email must contain @"

            it "rejects email without domain" $ do
                validateEmail "test@" `shouldBe` Left "Email must contain a domain"

        -- ============================================
        -- 音楽アーティストの例
        -- ============================================
        describe "wasArtistActive" $ do
            let activeArtist = Artist "Metallica" HeavyMetal USA (StillActive 1981)
            let retiredArtist = Artist "Led Zeppelin" HardRock England (ActiveBetween 1968 1980)

            it "returns True for still active artist in range" $ do
                wasArtistActive activeArtist 1990 2000 `shouldBe` True

            it "returns True for retired artist in range" $ do
                wasArtistActive retiredArtist 1970 1985 `shouldBe` True

            it "returns False for retired artist out of range" $ do
                wasArtistActive retiredArtist 1990 2000 `shouldBe` False

        describe "activeLength" $ do
            let activeArtist = Artist "Metallica" HeavyMetal USA (StillActive 2000)
            let retiredArtist = Artist "Led Zeppelin" HardRock England (ActiveBetween 1968 1980)

            it "calculates length for still active artist" $ do
                activeLength activeArtist 2024 `shouldBe` 24

            it "calculates length for retired artist" $ do
                activeLength retiredArtist 2024 `shouldBe` 12

        describe "searchByGenre" $ do
            let artists = [ Artist "Metallica" HeavyMetal USA (StillActive 1981)
                          , Artist "Madonna" Pop USA (StillActive 1982)
                          , Artist "Nirvana" Grunge USA (ActiveBetween 1987 1994)
                          ]

            it "filters by single genre" $ do
                map artistName (searchByGenre [HeavyMetal] artists) `shouldBe` ["Metallica"]

            it "filters by multiple genres" $ do
                map artistName (searchByGenre [HeavyMetal, Grunge] artists)
                    `shouldBe` ["Metallica", "Nirvana"]

        describe "searchByOrigin" $ do
            let artists = [ Artist "Metallica" HeavyMetal USA (StillActive 1981)
                          , Artist "Led Zeppelin" HardRock England (ActiveBetween 1968 1980)
                          ]

            it "filters by origin" $ do
                map artistName (searchByOrigin [England] artists) `shouldBe` ["Led Zeppelin"]

        describe "searchByActiveYears" $ do
            let artists = [ Artist "Metallica" HeavyMetal USA (StillActive 1981)
                          , Artist "Led Zeppelin" HardRock England (ActiveBetween 1968 1980)
                          ]

            it "filters by active years" $ do
                map artistName (searchByActiveYears 1970 1975 artists)
                    `shouldBe` ["Led Zeppelin"]

        -- ============================================
        -- 検索条件
        -- ============================================
        describe "searchArtists" $ do
            let artists = [ Artist "Metallica" HeavyMetal USA (StillActive 1981)
                          , Artist "Madonna" Pop USA (StillActive 1982)
                          , Artist "Led Zeppelin" HardRock England (ActiveBetween 1968 1980)
                          ]

            it "filters by single condition" $ do
                map artistName (searchArtists artists [SearchByGenre [HeavyMetal]])
                    `shouldBe` ["Metallica"]

            it "filters by multiple conditions (AND)" $ do
                map artistName (searchArtists artists
                    [SearchByGenre [HeavyMetal, Pop], SearchByOrigin [USA]])
                    `shouldBe` ["Metallica", "Madonna"]

            it "returns empty when no match" $ do
                searchArtists artists [SearchByOrigin [Australia]] `shouldBe` []

        -- ============================================
        -- ユーティリティ
        -- ============================================
        describe "orElseE" $ do
            it "returns first when Right" $ do
                (Right 5 :: Either String Int) `orElseE` Right 10 `shouldBe` Right 5

            it "returns second when first is Left" $ do
                (Left "error" :: Either String Int) `orElseE` Right 10 `shouldBe` Right 10

        describe "maybeToEither" $ do
            it "converts Just to Right" $ do
                maybeToEither "error" (Just 5) `shouldBe` Right (5 :: Int)

            it "converts Nothing to Left with error" $ do
                maybeToEither "error" (Nothing :: Maybe Int) `shouldBe` Left "error"

        describe "eitherToMaybe" $ do
            it "converts Right to Just" $ do
                eitherToMaybe (Right 5 :: Either String Int) `shouldBe` Just 5

            it "converts Left to Nothing" $ do
                eitherToMaybe (Left "error" :: Either String Int) `shouldBe` Nothing
