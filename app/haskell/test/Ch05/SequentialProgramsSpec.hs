{-|
Module      : Ch05.SequentialProgramsSpec
Description : 第5章のテスト
-}
module Ch05.SequentialProgramsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Ch05.SequentialPrograms

spec :: Spec
spec = do
    describe "Ch05.SequentialPrograms" $ do
        -- ============================================
        -- Book と Movie の例
        -- ============================================
        describe "bookAdaptations" $ do
            it "returns movies for Tolkien" $ do
                bookAdaptations "Tolkien" `shouldBe`
                    [Movie "An Unexpected Journey", Movie "The Desolation of Smaug"]

            it "returns empty for unknown author" $ do
                bookAdaptations "Unknown" `shouldBe` []

        describe "getAllAuthors" $ do
            let books = [ Book "FP in Scala" ["Chiusano", "Bjarnason"]
                        , Book "The Hobbit" ["Tolkien"]
                        ]

            it "flattens all authors" $ do
                getAllAuthors books `shouldBe` ["Chiusano","Bjarnason","Tolkien"]

        describe "getRecommendations" $ do
            let books = [Book "The Hobbit" ["Tolkien"]]

            it "generates recommendations" $ do
                let recs = getRecommendations books
                length recs `shouldBe` 2
                head recs `shouldBe`
                    "You may like An Unexpected Journey, because you liked Tolkien's The Hobbit"

        -- ============================================
        -- flatten と concatMap
        -- ============================================
        describe "myFlatten" $ do
            it "flattens nested lists" $ do
                myFlatten [[1,2],[3],[4,5,6]] `shouldBe` [1,2,3,4,5,6]

            it "handles empty inner lists" $ do
                myFlatten [[1,2],[],[3]] `shouldBe` [1,2,3]

            it "handles empty outer list" $ do
                myFlatten ([] :: [[Int]]) `shouldBe` []

        describe "myConcatMap" $ do
            it "maps and flattens" $ do
                myConcatMap (\x -> [x, x+10]) [1,2,3] `shouldBe` [1,11,2,12,3,13]

            it "property: equivalent to concatMap" $ property $
                \xs -> myConcatMap (\x -> [x, x*2]) xs ==
                       concatMap (\x -> [x, x*2]) (xs :: [Int])

        -- ============================================
        -- リストサイズの変化
        -- ============================================
        describe "duplicate" $ do
            it "duplicates each element" $ do
                duplicate [1,2,3] `shouldBe` [1,1,2,2,3,3]

            it "property: doubles the length" $ property $
                \xs -> length (duplicate xs) == 2 * length (xs :: [Int])

        describe "evenOnly" $ do
            it "keeps only even numbers" $ do
                evenOnly [1,2,3,4,5,6] `shouldBe` [2,4,6]

            it "returns empty for all odd" $ do
                evenOnly [1,3,5,7] `shouldBe` []

        describe "triplicate" $ do
            it "adds element and its triple" $ do
                triplicate [1,2] `shouldBe` [1,3,2,6]

        -- ============================================
        -- Point と Circle の例
        -- ============================================
        describe "isInside" $ do
            it "returns True for point inside circle" $ do
                isInside (Point 1 1) 2 `shouldBe` True

            it "returns False for point outside circle" $ do
                isInside (Point 5 2) 2 `shouldBe` False

            it "returns True for point on circle" $ do
                isInside (Point 2 0) 2 `shouldBe` True

        describe "allCombinations" $ do
            let points = [Point 1 1, Point 5 2]
            let radiuses = [2, 1]

            it "generates all combinations" $ do
                let result = allCombinations points radiuses
                length result `shouldBe` 4

        describe "pointsInsideRadius" $ do
            let points = [Point 1 1, Point 5 2]

            it "filters points inside radius" $ do
                pointsInsideRadius points 2 `shouldBe` [Point 1 1]

            it "returns empty if no points inside" $ do
                pointsInsideRadius points 1 `shouldBe` []

        -- ============================================
        -- do 記法の例
        -- ============================================
        describe "combinations" $ do
            it "generates all pairs" $ do
                combinations [1,2] [10,20] `shouldBe`
                    [(1,10),(1,20),(2,10),(2,20)]

            it "handles empty list" $ do
                combinations [] [1,2] `shouldBe` ([] :: [(Int, Int)])

        describe "combinationsWithGuard" $ do
            it "filters by sum >= 15" $ do
                combinationsWithGuard [1,2,10] [10,20] `shouldBe`
                    [(1,20),(2,20),(10,10),(10,20)]

        describe "cartesianProduct" $ do
            it "computes sum of all combinations" $ do
                cartesianProduct [1,2] [10,20] [100,200] `shouldBe`
                    [111,211,121,221,112,212,122,222]

            it "property: result length is product of input lengths" $ property $
                \xs ys zs ->
                    let xs' = take 3 (xs :: [Int])
                        ys' = take 3 (ys :: [Int])
                        zs' = take 3 (zs :: [Int])
                    in length (cartesianProduct xs' ys' zs') ==
                       length xs' * length ys' * length zs'
