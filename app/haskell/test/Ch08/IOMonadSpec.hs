{-|
Module      : Ch08.IOMonadSpec
Description : 第8章のテスト
-}
module Ch08.IOMonadSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Ch08.IOMonad

spec :: Spec
spec = do
    describe "Ch08.IOMonad" $ do
        -- ============================================
        -- サイコロの例
        -- ============================================
        describe "castTheDie" $ do
            it "returns value between 1 and 6" $ do
                results <- sequence $ replicate 100 castTheDie
                all (\x -> x >= 1 && x <= 6) results `shouldBe` True

        describe "castTheDieTwice" $ do
            it "returns value between 2 and 12" $ do
                results <- sequence $ replicate 100 castTheDieTwice
                all (\x -> x >= 2 && x <= 12) results `shouldBe` True

        describe "castTheDieN" $ do
            it "returns n dice rolls" $ do
                results <- castTheDieN 5
                length results `shouldBe` 5

            it "all values are between 1 and 6" $ do
                results <- castTheDieN 100
                all (\x -> x >= 1 && x <= 6) results `shouldBe` True

        -- ============================================
        -- ミーティングスケジューリング
        -- ============================================
        describe "meetingsOverlap" $ do
            it "returns False for adjacent meetings" $ do
                meetingsOverlap (MeetingTime 9 10) (MeetingTime 10 11) `shouldBe` False

            it "returns True for overlapping meetings" $ do
                meetingsOverlap (MeetingTime 9 11) (MeetingTime 10 12) `shouldBe` True

            it "returns True for contained meeting" $ do
                meetingsOverlap (MeetingTime 9 17) (MeetingTime 10 12) `shouldBe` True

            it "returns False for non-overlapping meetings" $ do
                meetingsOverlap (MeetingTime 9 10) (MeetingTime 14 15) `shouldBe` False

        describe "possibleMeetings" $ do
            it "returns available slots" $ do
                let existing = [MeetingTime 9 10, MeetingTime 14 16]
                let possible = possibleMeetings existing 8 17 1
                length possible `shouldSatisfy` (> 0)

            it "excludes overlapping slots" $ do
                let existing = [MeetingTime 10 11]
                let possible = possibleMeetings existing 8 12 1
                MeetingTime 10 11 `notElem` possible `shouldBe` True

            it "returns empty when no slots available" $ do
                let existing = [MeetingTime 8 17]
                let possible = possibleMeetings existing 8 17 1
                possible `shouldBe` []

        -- ============================================
        -- IO の合成
        -- ============================================
        describe "combineIO" $ do
            it "combines two IO actions" $ do
                result <- combineIO (return 1) (return 2) (+)
                result `shouldBe` 3

            it "applies function to results" $ do
                result <- combineIO (return "Hello") (return "World") (\a b -> a ++ " " ++ b)
                result `shouldBe` "Hello World"

        describe "sequenceIO" $ do
            it "sequences list of IO actions" $ do
                result <- sequenceIO [return 1, return 2, return 3]
                result `shouldBe` [1, 2, 3]

            it "handles empty list" $ do
                result <- sequenceIO ([] :: [IO Int])
                result `shouldBe` []

        -- ============================================
        -- エラーハンドリング
        -- ============================================
        describe "catchIO" $ do
            it "returns Right for successful action" $ do
                result <- catchIO (return 42 :: IO Int)
                result `shouldBe` Right 42

            it "returns Left for failed action" $ do
                result <- catchIO (fail "error" :: IO Int)
                case result of
                    Left _  -> return ()
                    Right _ -> expectationFailure "Expected Left"

        describe "retryWithDefault" $ do
            it "returns result on success" $ do
                result <- retryWithDefault 3 0 (return 42 :: IO Int)
                result `shouldBe` 42

        -- ============================================
        -- 純粋関数と IO の分離
        -- ============================================
        describe "parseAndValidate" $ do
            it "parses valid positive number" $ do
                parseAndValidate "42" `shouldBe` Right 42

            it "rejects invalid input" $ do
                parseAndValidate "abc" `shouldBe` Left "Invalid number"

            it "rejects negative number" $ do
                parseAndValidate "-5" `shouldBe` Left "Number must be positive"

            it "rejects zero" $ do
                parseAndValidate "0" `shouldBe` Left "Number must be positive"
