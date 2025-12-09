namespace Ch01

/// 第1章: 関数型プログラミング入門
/// F# の基本構文と関数型プログラミングの概念を学ぶ
module IntroFSharp =

    // ============================================
    // 基本的な関数定義
    // ============================================

    /// 数値をインクリメント
    let increment (x: int) : int =
        x + 1

    /// 文字列の最初の文字を取得
    let getFirstCharacter (s: string) : char =
        s.[0]

    /// 単語のスコアを計算（文字数）
    let wordScore (word: string) : int =
        word.Length

    // ============================================
    // 関数型 vs 命令型の比較
    // ============================================

    /// 命令型スタイル: ループを使用してスコアを計算
    let wordScoreImperative (word: string) : int =
        let mutable score = 0
        for _ in word do
            score <- score + 1
        score

    /// 関数型スタイル: 宣言的にスコアを計算
    let wordScoreFunctional (word: string) : int =
        word.Length

    // ============================================
    // パイプライン演算子の例
    // ============================================

    /// 文字列を大文字に変換してスコアを計算
    let scoreUpperCase (word: string) : int =
        word
        |> fun s -> s.ToUpper()
        |> fun s -> s.Length

    /// 複数の変換をパイプラインで連結
    let processWord (word: string) : string =
        word
        |> fun s -> s.Trim()
        |> fun s -> s.ToLower()
        |> fun s -> s.Replace(" ", "")

    // ============================================
    // 型推論の例
    // ============================================

    // 型注釈なしでも型推論される
    let add a b = a + b

    // 明示的な型注釈
    let addExplicit (a: int) (b: int) : int = a + b

    // ジェネリック関数
    let identity x = x

    // ============================================
    // 部分適用とカリー化
    // ============================================

    /// 2つの数値を加算
    let addTwo x y = x + y

    /// 部分適用: 5を加算する関数
    let addFive = addTwo 5

    /// 3つの数値を加算
    let addThree x y z = x + y + z

    /// 部分適用の例
    let addTenAndTwenty = addThree 10 20
