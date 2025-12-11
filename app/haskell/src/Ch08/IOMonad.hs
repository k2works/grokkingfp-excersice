{-|
Module      : Ch08.IOMonad
Description : 第8章: IO モナドの導入
Copyright   : (c) Project Team, 2024
License     : MIT

IO モナドを使った副作用の管理について学ぶ
-}
module Ch08.IOMonad
    ( -- * サイコロの例
      castTheDie
    , castTheDieTwice
    , castTheDieN
      -- * ミーティングスケジューリング
    , MeetingTime(..)
    , meetingsOverlap
    , possibleMeetings
    , scheduledMeetings
      -- * IO の合成
    , printAndReturn
    , combineIO
    , sequenceIO
      -- * エラーハンドリング
    , retryIO
    , retryWithDefault
    , catchIO
      -- * 純粋関数と IO の分離
    , parseAndValidate
    , processInput
    ) where

import System.Random (randomRIO)
import Control.Exception (try, SomeException)
import Control.Monad (replicateM)

-- ============================================
-- サイコロの例
-- ============================================

-- | サイコロを振る（副作用あり）
--
-- 1から6のランダムな値を返す
castTheDie :: IO Int
castTheDie = randomRIO (1, 6)

-- | サイコロを2回振って合計を返す
--
-- >>> castTheDieTwice >>= print
-- (例: 7)
castTheDieTwice :: IO Int
castTheDieTwice = do
    first  <- castTheDie
    second <- castTheDie
    return (first + second)

-- | サイコロをn回振って結果をリストで返す
--
-- >>> castTheDieN 3 >>= print
-- (例: [4, 2, 6])
castTheDieN :: Int -> IO [Int]
castTheDieN n = replicateM n castTheDie

-- ============================================
-- ミーティングスケジューリング
-- ============================================

-- | ミーティング時間を表すデータ型
data MeetingTime = MeetingTime
    { mtStartHour :: Int
    , mtEndHour   :: Int
    } deriving (Show, Eq)

-- | 2つのミーティングが重なっているかを判定（純粋関数）
--
-- >>> meetingsOverlap (MeetingTime 9 10) (MeetingTime 10 11)
-- False
-- >>> meetingsOverlap (MeetingTime 9 11) (MeetingTime 10 12)
-- True
meetingsOverlap :: MeetingTime -> MeetingTime -> Bool
meetingsOverlap m1 m2 =
    mtStartHour m1 < mtEndHour m2 && mtEndHour m1 > mtStartHour m2

-- | 可能なミーティング時間を計算（純粋関数）
--
-- >>> possibleMeetings [MeetingTime 9 10, MeetingTime 14 16] 8 17 1
-- [MeetingTime {mtStartHour = 10, mtEndHour = 11},MeetingTime {mtStartHour = 11, mtEndHour = 12},MeetingTime {mtStartHour = 12, mtEndHour = 13},MeetingTime {mtStartHour = 13, mtEndHour = 14},MeetingTime {mtStartHour = 16, mtEndHour = 17}]
possibleMeetings :: [MeetingTime] -> Int -> Int -> Int -> [MeetingTime]
possibleMeetings existingMeetings startHour endHour lengthHours =
    let slots = [MeetingTime s (s + lengthHours) | s <- [startHour .. endHour - lengthHours]]
    in filter (\slot -> all (not . meetingsOverlap slot) existingMeetings) slots

-- | 複数人の予定を取得（IO版の例）
-- 実際の実装では API 呼び出しなどを行う
scheduledMeetings :: [String] -> IO [MeetingTime]
scheduledMeetings attendees = do
    -- シミュレーション: 各参加者に対してランダムなミーティングを生成
    meetings <- mapM generateRandomMeeting attendees
    return $ concat meetings
  where
    generateRandomMeeting :: String -> IO [MeetingTime]
    generateRandomMeeting _ = do
        n <- randomRIO (0, 3)
        replicateM n randomMeeting

    randomMeeting :: IO MeetingTime
    randomMeeting = do
        start <- randomRIO (8, 15)
        len   <- randomRIO (1, 2)
        return $ MeetingTime start (start + len)

-- ============================================
-- IO の合成
-- ============================================

-- | メッセージを出力して返す
--
-- >>> printAndReturn "Hello" >>= print
-- Hello
-- "Hello"
printAndReturn :: String -> IO String
printAndReturn message = do
    putStrLn message
    return message

-- | 2つの IO を合成
--
-- >>> combineIO (return 1) (return 2) (+) >>= print
-- 3
combineIO :: IO a -> IO b -> (a -> b -> c) -> IO c
combineIO io1 io2 f = do
    a <- io1
    b <- io2
    return (f a b)

-- | IO のリストを順番に実行
--
-- >>> sequenceIO [return 1, return 2, return 3] >>= print
-- [1,2,3]
sequenceIO :: [IO a] -> IO [a]
sequenceIO []       = return []
sequenceIO (x:xs) = do
    a  <- x
    as <- sequenceIO xs
    return (a : as)

-- ============================================
-- エラーハンドリング
-- ============================================

-- | IO アクションをリトライ
--
-- 失敗した場合、指定回数までリトライする
retryIO :: Int -> IO a -> IO (Maybe a)
retryIO 0 _      = return Nothing
retryIO n action = do
    result <- try action :: IO (Either SomeException a)
    case result of
        Right a -> return (Just a)
        Left _  -> retryIO (n - 1) action

-- | リトライしてデフォルト値を返す
--
-- 全部失敗したらデフォルト値を返す
retryWithDefault :: Int -> a -> IO a -> IO a
retryWithDefault maxRetries defaultVal action = do
    result <- retryIO maxRetries action
    return $ maybe defaultVal id result

-- | 例外をキャッチして Either に変換
--
-- >>> catchIO (return 42) >>= print
-- Right 42
catchIO :: IO a -> IO (Either String a)
catchIO action = do
    result <- try action :: IO (Either SomeException a)
    return $ case result of
        Right a -> Right a
        Left e  -> Left (show e)

-- ============================================
-- 純粋関数と IO の分離
-- ============================================

-- | 入力をパースして検証（純粋関数）
--
-- >>> parseAndValidate "42"
-- Right 42
-- >>> parseAndValidate "abc"
-- Left "Invalid number"
-- >>> parseAndValidate "-5"
-- Left "Number must be positive"
parseAndValidate :: String -> Either String Int
parseAndValidate input =
    case reads input of
        [(n, "")] | n > 0     -> Right n
                  | otherwise -> Left "Number must be positive"
        _                     -> Left "Invalid number"

-- | 入力を処理（IO と純粋関数の分離）
--
-- 入力を読み取り、パース・検証し、結果を出力
processInput :: IO ()
processInput = do
    putStr "Enter a positive number: "
    input <- getLine
    case parseAndValidate input of
        Right n  -> putStrLn $ "Valid: " ++ show n
        Left err -> putStrLn $ "Error: " ++ err
