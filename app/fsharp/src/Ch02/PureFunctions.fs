namespace Ch02

/// 第2章: 純粋関数
/// 純粋関数の概念と副作用のない関数の重要性を学ぶ
module PureFunctions =

    // ============================================
    // 純粋関数の例
    // ============================================

    /// 数値をインクリメント（純粋関数）
    let increment (x: int) : int =
        x + 1

    /// 2つの数値を加算（純粋関数）
    let add (a: int) (b: int) : int =
        a + b

    /// 文字列の最初の文字を取得（純粋関数）
    let getFirstCharacter (s: string) : char =
        s.[0]

    /// 数値を2倍にする（純粋関数）
    let double (x: int) : int =
        x * 2

    /// 数値が偶数かどうかを判定（純粋関数）
    let isEven (n: int) : bool =
        n % 2 = 0

    // ============================================
    // 不純な関数の例（避けるべきパターン）
    // ============================================

    /// 不純な関数 - 毎回異なる値を返す
    let randomPart (x: float) : float =
        x * System.Random().NextDouble()

    /// 不純な関数 - 現在時刻を返す
    let currentTime () : int64 =
        System.DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()

    // ============================================
    // 'a' を除外するワードスコア
    // ============================================

    /// 'a' を除外した文字数をスコアとして返す
    let wordScore (word: string) : int =
        word.Replace("a", "").Length

    /// 大文字小文字を区別せずに 'a' を除外
    let wordScoreIgnoreCase (word: string) : int =
        word
        |> String.filter (fun c -> c <> 'a' && c <> 'A')
        |> String.length

    // ============================================
    // ボーナススコア計算
    // ============================================

    /// ボーナススコアを計算
    /// 7文字以上なら5点ボーナス
    let bonusScore (word: string) : int =
        if word.Length >= 7 then 5 else 0

    /// 総合スコアを計算
    let totalScore (word: string) : int =
        wordScore word + bonusScore word
