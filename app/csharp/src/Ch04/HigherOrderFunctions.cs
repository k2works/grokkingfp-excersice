using LanguageExt;
using static LanguageExt.Prelude;

namespace Ch04;

/// <summary>
/// 第4章: 関数を値として扱う
/// 高階関数の概念と使い方を学ぶ
/// </summary>
public static class HigherOrderFunctions
{
    // ============================================
    // 基本的な高階関数
    // ============================================

    /// <summary>
    /// 'a' を除外したワードスコア
    /// </summary>
    public static int WordScore(string word) =>
        word.Replace("a", "").Length;

    /// <summary>
    /// ボーナススコア（'c' を含む場合 +5）
    /// </summary>
    public static int Bonus(string word) =>
        word.Contains('c') ? 5 : 0;

    /// <summary>
    /// ペナルティスコア（'s' を含む場合 -7）
    /// </summary>
    public static int Penalty(string word) =>
        word.Contains('s') ? 7 : 0;

    /// <summary>
    /// 文字列の長さを返す
    /// </summary>
    public static int Len(string s) => s.Length;

    /// <summary>
    /// 数値を2倍にする
    /// </summary>
    public static int Double(int i) => i * 2;

    /// <summary>
    /// 奇数かどうかを判定
    /// </summary>
    public static bool IsOdd(int i) => i % 2 == 1;

    /// <summary>
    /// 偶数かどうかを判定
    /// </summary>
    public static bool IsEven(int i) => i % 2 == 0;

    // ============================================
    // SortBy - ソート基準を関数で指定
    // ============================================

    /// <summary>
    /// スコアでソート
    /// </summary>
    public static Seq<string> SortByScore(Seq<string> words) =>
        toSeq(words.OrderBy(WordScore));

    /// <summary>
    /// スコアの降順でソート
    /// </summary>
    public static Seq<string> SortByScoreDescending(Seq<string> words) =>
        toSeq(words.OrderByDescending(WordScore));

    // ============================================
    // Map - 各要素を変換
    // ============================================

    /// <summary>
    /// 各文字列の長さを取得
    /// </summary>
    public static Seq<int> Lengths(Seq<string> strings) =>
        strings.Map(Len);

    /// <summary>
    /// 各数値を2倍にする
    /// </summary>
    public static Seq<int> Doubles(Seq<int> numbers) =>
        numbers.Map(Double);

    // ============================================
    // Filter - 条件に合う要素を抽出
    // ============================================

    /// <summary>
    /// 奇数のみを抽出
    /// </summary>
    public static Seq<int> FilterOdds(Seq<int> numbers) =>
        numbers.Filter(IsOdd);

    /// <summary>
    /// 指定した値より大きい要素を抽出
    /// </summary>
    public static Seq<int> FilterLargerThan(Seq<int> numbers, int n) =>
        numbers.Filter(i => i > n);

    // ============================================
    // Fold - 畳み込み
    // ============================================

    /// <summary>
    /// Seq の合計を計算
    /// </summary>
    public static int Sum(Seq<int> numbers) =>
        numbers.Fold(0, (acc, i) => acc + i);

    /// <summary>
    /// Seq の最大値を取得
    /// </summary>
    public static int Maximum(Seq<int> numbers) =>
        numbers.Fold(int.MinValue, (maxVal, i) => i > maxVal ? i : maxVal);

    /// <summary>
    /// 条件を満たす要素の数をカウント
    /// </summary>
    public static int CountWhere<T>(Seq<T> seq, Func<T, bool> predicate) =>
        seq.Filter(predicate).Count;

    // ============================================
    // 関数を返す関数
    // ============================================

    /// <summary>
    /// n より大きいかを判定する関数を返す
    /// </summary>
    public static Func<int, bool> LargerThan(int n) =>
        i => i > n;

    /// <summary>
    /// n で割り切れるかを判定する関数を返す
    /// </summary>
    public static Func<int, bool> DivisibleBy(int n) =>
        i => i % n == 0;

    /// <summary>
    /// 指定した文字を含むかを判定する関数を返す
    /// </summary>
    public static Func<string, bool> ContainsChar(char c) =>
        s => s.Contains(c);

    // ============================================
    // ワードランキング
    // ============================================

    /// <summary>
    /// 指定したスコア関数でワードをランキング
    /// </summary>
    public static Seq<string> RankedWords(Seq<string> words, Func<string, int> scoreFn) =>
        toSeq(words.OrderByDescending(scoreFn));

    /// <summary>
    /// 基本スコアでランキング
    /// </summary>
    public static Seq<string> RankByScore(Seq<string> words) =>
        RankedWords(words, WordScore);

    /// <summary>
    /// ボーナス付きスコアでランキング
    /// </summary>
    public static Seq<string> RankByScoreWithBonus(Seq<string> words) =>
        RankedWords(words, w => WordScore(w) + Bonus(w));

    /// <summary>
    /// ボーナスとペナルティ付きスコアでランキング
    /// </summary>
    public static Seq<string> RankByScoreWithBonusAndPenalty(Seq<string> words) =>
        RankedWords(words, w => WordScore(w) + Bonus(w) - Penalty(w));
}

/// <summary>
/// プログラミング言語のデータ型
/// </summary>
public static class ProgrammingLanguages
{
    /// <summary>
    /// プログラミング言語を表すレコード型
    /// </summary>
    public record ProgrammingLanguage(string Name, int Year);

    /// <summary>
    /// 言語の名前を取得
    /// </summary>
    public static string GetName(ProgrammingLanguage lang) => lang.Name;

    /// <summary>
    /// 言語の年を取得
    /// </summary>
    public static int GetYear(ProgrammingLanguage lang) => lang.Year;

    /// <summary>
    /// 指定した年より後に作られた言語をフィルタ
    /// </summary>
    public static Seq<ProgrammingLanguage> FilterByYear(Seq<ProgrammingLanguage> languages, int year) =>
        languages.Filter(lang => lang.Year > year);

    /// <summary>
    /// 言語名のリストを取得
    /// </summary>
    public static Seq<string> GetNames(Seq<ProgrammingLanguage> languages) =>
        languages.Map(GetName);

    /// <summary>
    /// 年でソート
    /// </summary>
    public static Seq<ProgrammingLanguage> SortByYear(Seq<ProgrammingLanguage> languages) =>
        toSeq(languages.OrderBy(GetYear));
}
