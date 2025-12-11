namespace Ch04

/// 第4章: 関数を値として扱う
/// 高階関数の概念と使い方を学ぶ
module HigherOrderFunctions =

    // ============================================
    // 基本的な高階関数
    // ============================================

    /// 'a' を除外したワードスコア
    let wordScore (word: string) : int =
        word.Replace("a", "").Length

    /// ボーナススコア（'c' を含む場合 +5）
    let bonus (word: string) : int =
        if word.Contains("c") then 5 else 0

    /// ペナルティスコア（'s' を含む場合 -7）
    let penalty (word: string) : int =
        if word.Contains("s") then 7 else 0

    /// 文字列の長さを返す
    let len (s: string) : int = s.Length

    /// 数値を2倍にする
    let double (i: int) : int = i * 2

    /// 奇数かどうかを判定
    let isOdd (i: int) : bool = i % 2 = 1

    /// 偶数かどうかを判定
    let isEven (i: int) : bool = i % 2 = 0

    // ============================================
    // sortBy - ソート基準を関数で指定
    // ============================================

    /// スコアでソート
    let sortByScore (words: string list) : string list =
        words |> List.sortBy wordScore

    /// スコアの降順でソート
    let sortByScoreDescending (words: string list) : string list =
        words |> List.sortByDescending wordScore

    // ============================================
    // map - 各要素を変換
    // ============================================

    /// 各文字列の長さを取得
    let lengths (strings: string list) : int list =
        strings |> List.map len

    /// 各数値を2倍にする
    let doubles (numbers: int list) : int list =
        numbers |> List.map double

    // ============================================
    // filter - 条件に合う要素を抽出
    // ============================================

    /// 奇数のみを抽出
    let filterOdds (numbers: int list) : int list =
        numbers |> List.filter isOdd

    /// 指定した値より大きい要素を抽出
    let filterLargerThan (n: int) (numbers: int list) : int list =
        numbers |> List.filter (fun i -> i > n)

    // ============================================
    // fold - 畳み込み
    // ============================================

    /// リストの合計を計算
    let sum (numbers: int list) : int =
        numbers |> List.fold (fun acc i -> acc + i) 0

    /// リストの最大値を取得
    let maximum (numbers: int list) : int =
        numbers |> List.fold (fun maxVal i -> if i > maxVal then i else maxVal) System.Int32.MinValue

    /// 条件を満たす要素の数をカウント
    let countWhere (predicate: 'a -> bool) (list: 'a list) : int =
        list |> List.filter predicate |> List.length

    // ============================================
    // 関数を返す関数
    // ============================================

    /// n より大きいかを判定する関数を返す
    let largerThan (n: int) : int -> bool =
        fun i -> i > n

    /// n で割り切れるかを判定する関数を返す
    let divisibleBy (n: int) : int -> bool =
        fun i -> i % n = 0

    /// 指定した文字を含むかを判定する関数を返す
    let containsChar (c: char) : string -> bool =
        fun s -> s.Contains(c)

    // ============================================
    // ワードランキング
    // ============================================

    /// 指定したスコア関数でワードをランキング
    let rankedWords (scoreFn: string -> int) (words: string list) : string list =
        words |> List.sortByDescending scoreFn

    /// 基本スコアでランキング
    let rankByScore = rankedWords wordScore

    /// ボーナス付きスコアでランキング
    let rankByScoreWithBonus (words: string list) : string list =
        rankedWords (fun w -> wordScore w + bonus w) words

    /// ボーナスとペナルティ付きスコアでランキング
    let rankByScoreWithBonusAndPenalty (words: string list) : string list =
        rankedWords (fun w -> wordScore w + bonus w - penalty w) words


/// プログラミング言語のデータ型
module ProgrammingLanguages =

    /// プログラミング言語を表すレコード型
    type ProgrammingLanguage = {
        Name: string
        Year: int
    }

    /// 言語の名前を取得
    let getName (lang: ProgrammingLanguage) : string = lang.Name

    /// 言語の年を取得
    let getYear (lang: ProgrammingLanguage) : int = lang.Year

    /// 指定した年より後に作られた言語をフィルタ
    let filterByYear (year: int) (languages: ProgrammingLanguage list) : ProgrammingLanguage list =
        languages |> List.filter (fun lang -> lang.Year > year)

    /// 言語名のリストを取得
    let getNames (languages: ProgrammingLanguage list) : string list =
        languages |> List.map getName

    /// 年でソート
    let sortByYear (languages: ProgrammingLanguage list) : ProgrammingLanguage list =
        languages |> List.sortBy getYear
