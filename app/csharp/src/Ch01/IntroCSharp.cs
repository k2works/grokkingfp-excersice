using LanguageExt;
using static LanguageExt.Prelude;

namespace Ch01;

/// <summary>
/// 第1章: 関数型プログラミング入門
/// C# + LanguageExt による関数型プログラミングの基礎
/// </summary>
public static class IntroCSharp
{
    // ============================================
    // 1.1 命令型 vs 関数型
    // ============================================

    /// <summary>
    /// 命令型でワードスコアを計算
    /// </summary>
    public static int WordScoreImperative(string word)
    {
        var score = 0;
        foreach (var _ in word)
        {
            score = score + 1;
        }
        return score;
    }

    /// <summary>
    /// 関数型でワードスコアを計算
    /// </summary>
    public static int WordScore(string word) => word.Length;

    // ============================================
    // 1.2 基本的な関数定義
    // ============================================

    /// <summary>
    /// 数値をインクリメント
    /// </summary>
    public static int Increment(int x) => x + 1;

    /// <summary>
    /// 文字列の最初の文字を取得
    /// </summary>
    public static char GetFirstCharacter(string s) => s[0];

    /// <summary>
    /// 2つの数値を加算
    /// </summary>
    public static int Add(int a, int b) => a + b;

    /// <summary>
    /// 数値を2倍にする
    /// </summary>
    public static int Double(int x) => x * 2;

    /// <summary>
    /// 数値が偶数かどうかを判定
    /// </summary>
    public static bool IsEven(int n) => n % 2 == 0;

    // ============================================
    // 1.3 LanguageExt を使った関数合成
    // ============================================

    /// <summary>
    /// 文字列を大文字に変換してスコアを計算
    /// Map を使ったパイプライン的な処理
    /// </summary>
    public static int ScoreUpperCase(string word) =>
        word.ToUpper().Length;

    /// <summary>
    /// 複数の変換をチェーンで連結
    /// </summary>
    public static string ProcessWord(string word) =>
        word.Trim().ToLower().Replace(" ", "");

    // ============================================
    // 1.4 カリー化と部分適用
    // ============================================

    /// <summary>
    /// カリー化された加算関数
    /// LanguageExt の curry を使用
    /// </summary>
    public static Func<int, Func<int, int>> AddCurried =>
        curry<int, int, int>((a, b) => a + b);

    /// <summary>
    /// 部分適用: 5を加算する関数
    /// </summary>
    public static Func<int, int> AddFive => AddCurried(5);

    /// <summary>
    /// カリー化された乗算関数
    /// </summary>
    public static Func<int, Func<int, int>> MultiplyCurried =>
        curry<int, int, int>((a, b) => a * b);

    /// <summary>
    /// 部分適用: 2倍にする関数
    /// </summary>
    public static Func<int, int> DoubleIt => MultiplyCurried(2);

    // ============================================
    // 1.5 LanguageExt の Seq を使った操作
    // ============================================

    /// <summary>
    /// Seq を使った宣言的なリスト操作
    /// </summary>
    public static Seq<int> DoubleAll(Seq<int> numbers) =>
        numbers.Map(x => x * 2);

    /// <summary>
    /// 偶数のみをフィルタリング
    /// </summary>
    public static Seq<int> FilterEvens(Seq<int> numbers) =>
        numbers.Filter(IsEven);

    /// <summary>
    /// 合計を計算
    /// </summary>
    public static int Sum(Seq<int> numbers) =>
        numbers.Fold(0, (acc, x) => acc + x);

    // ============================================
    // 1.6 Option を使った安全な操作
    // ============================================

    /// <summary>
    /// 安全に最初の文字を取得（空文字列の場合は None）
    /// </summary>
    public static Option<char> GetFirstCharacterSafe(string s) =>
        string.IsNullOrEmpty(s)
            ? None
            : Some(s[0]);

    /// <summary>
    /// 安全な除算（ゼロ除算を防ぐ）
    /// </summary>
    public static Option<int> SafeDivide(int a, int b) =>
        b == 0
            ? None
            : Some(a / b);

    // ============================================
    // 1.7 関数の合成
    // ============================================

    /// <summary>
    /// 2つの関数を合成
    /// </summary>
    public static Func<A, C> Compose<A, B, C>(Func<A, B> f, Func<B, C> g) =>
        x => g(f(x));

    /// <summary>
    /// インクリメントして2倍にする合成関数
    /// </summary>
    public static Func<int, int> IncrementThenDouble =>
        Compose<int, int, int>(Increment, Double);

    /// <summary>
    /// 2倍にしてインクリメントする合成関数
    /// </summary>
    public static Func<int, int> DoubleThenIncrement =>
        Compose<int, int, int>(Double, Increment);
}
