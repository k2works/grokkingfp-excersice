{-|
Module      : Ch05.SequentialPrograms
Description : 第5章: flatMap とネスト構造
Copyright   : (c) Project Team, 2024
License     : MIT

flatten, concatMap (flatMap), do記法について学ぶ
-}
module Ch05.SequentialPrograms
    ( -- * Book と Movie の例
      Book(..)
    , Movie(..)
    , bookAdaptations
    , getAllAuthors
    , getRecommendations
      -- * flatten と concatMap
    , myFlatten
    , myConcatMap
      -- * リストサイズの変化
    , duplicate
    , evenOnly
    , triplicate
      -- * Point と Circle の例
    , Point(..)
    , isInside
    , allCombinations
    , pointsInsideRadius
      -- * do 記法の例
    , combinations
    , combinationsWithGuard
    , cartesianProduct
    ) where

-- ============================================
-- Book と Movie の例
-- ============================================

-- | 本を表すデータ型
data Book = Book
    { bookTitle   :: String
    , bookAuthors :: [String]
    } deriving (Show, Eq)

-- | 映画を表すデータ型
data Movie = Movie
    { movieTitle :: String
    } deriving (Show, Eq)

-- | 著者の本が映画化されたものを返す
--
-- >>> bookAdaptations "Tolkien"
-- [Movie {movieTitle = "An Unexpected Journey"},Movie {movieTitle = "The Desolation of Smaug"}]
bookAdaptations :: String -> [Movie]
bookAdaptations author
    | author == "Tolkien" = [Movie "An Unexpected Journey", Movie "The Desolation of Smaug"]
    | otherwise           = []

-- | すべての著者を取得（flatMap/concatMap を使用）
--
-- >>> let books = [Book "FP in Scala" ["Chiusano", "Bjarnason"], Book "The Hobbit" ["Tolkien"]]
-- >>> getAllAuthors books
-- ["Chiusano","Bjarnason","Tolkien"]
getAllAuthors :: [Book] -> [String]
getAllAuthors = concatMap bookAuthors

-- | おすすめを生成
--
-- >>> let books = [Book "The Hobbit" ["Tolkien"]]
-- >>> getRecommendations books
-- ["You may like An Unexpected Journey, because you liked Tolkien's The Hobbit","You may like The Desolation of Smaug, because you liked Tolkien's The Hobbit"]
getRecommendations :: [Book] -> [String]
getRecommendations books = do
    book   <- books
    author <- bookAuthors book
    movie  <- bookAdaptations author
    return $ "You may like " ++ movieTitle movie ++
             ", because you liked " ++ author ++ "'s " ++ bookTitle book

-- ============================================
-- flatten と concatMap
-- ============================================

-- | ネストしたリストを平坦化
--
-- >>> myFlatten [[1,2],[3],[4,5,6]]
-- [1,2,3,4,5,6]
myFlatten :: [[a]] -> [a]
myFlatten = concat

-- | map して flatten（= flatMap / concatMap）
--
-- >>> myConcatMap (\x -> [x, x+10]) [1,2,3]
-- [1,11,2,12,3,13]
myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f xs = concat $ map f xs

-- ============================================
-- リストサイズの変化
-- ============================================

-- | 各要素を2つに複製
--
-- >>> duplicate [1,2,3]
-- [1,1,2,2,3,3]
duplicate :: [a] -> [a]
duplicate = concatMap (\x -> [x, x])

-- | 偶数のみを抽出（concatMap でフィルタリング）
--
-- >>> evenOnly [1,2,3,4,5,6]
-- [2,4,6]
evenOnly :: [Int] -> [Int]
evenOnly = concatMap (\x -> if even x then [x] else [])

-- | 各要素を3倍にして追加
--
-- >>> triplicate [1,2]
-- [1,3,2,6]
triplicate :: [Int] -> [Int]
triplicate = concatMap (\x -> [x, x * 3])

-- ============================================
-- Point と Circle の例
-- ============================================

-- | 点を表すデータ型
data Point = Point
    { pointX :: Int
    , pointY :: Int
    } deriving (Show, Eq)

-- | 点が指定半径の円内にあるか判定
--
-- >>> isInside (Point 1 1) 2
-- True
-- >>> isInside (Point 5 2) 2
-- False
isInside :: Point -> Int -> Bool
isInside point radius =
    radius * radius >= pointX point * pointX point + pointY point * pointY point

-- | すべての組み合わせを生成
--
-- >>> let points = [Point 1 1, Point 5 2]
-- >>> let radiuses = [2, 1]
-- >>> allCombinations points radiuses
-- [("Point 1 1",2,True),("Point 5 2",2,False),("Point 1 1",1,False),("Point 5 2",1,False)]
allCombinations :: [Point] -> [Int] -> [(String, Int, Bool)]
allCombinations points radiuses = do
    r     <- radiuses
    point <- points
    let desc = "Point " ++ show (pointX point) ++ " " ++ show (pointY point)
    return (desc, r, isInside point r)

-- | 円内にある点のみを返す（ガード付き）
--
-- >>> let points = [Point 1 1, Point 5 2]
-- >>> pointsInsideRadius points 2
-- [Point {pointX = 1, pointY = 1}]
pointsInsideRadius :: [Point] -> Int -> [Point]
pointsInsideRadius points radius = do
    point <- points
    if isInside point radius
        then return point
        else []

-- ============================================
-- do 記法の例
-- ============================================

-- | 2つのリストの全組み合わせ
--
-- >>> combinations [1,2] [10,20]
-- [(1,10),(1,20),(2,10),(2,20)]
combinations :: [a] -> [b] -> [(a, b)]
combinations xs ys = do
    x <- xs
    y <- ys
    return (x, y)

-- | ガード付きの組み合わせ（和が15以上のもの）
--
-- >>> combinationsWithGuard [1,2,10] [10,20]
-- [(1,20),(2,20),(10,10),(10,20)]
combinationsWithGuard :: [Int] -> [Int] -> [(Int, Int)]
combinationsWithGuard xs ys = do
    x <- xs
    y <- ys
    if x + y >= 15
        then return (x, y)
        else []

-- | 3つのリストのデカルト積
--
-- >>> cartesianProduct [1,2] [10,20] [100,200]
-- [111,211,121,221,112,212,122,222]
cartesianProduct :: [Int] -> [Int] -> [Int] -> [Int]
cartesianProduct xs ys zs = do
    x <- xs
    y <- ys
    z <- zs
    return (x + y + z)
