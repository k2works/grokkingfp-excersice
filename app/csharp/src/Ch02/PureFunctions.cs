using LanguageExt;
using static LanguageExt.Prelude;

namespace Ch02;

/// <summary>
/// 第2章: 純粋関数とテスト
/// 純粋関数の定義と副作用の排除
/// </summary>
public static class PureFunctions
{
    // ============================================
    // 2.1 純粋関数の例
    // ============================================

    /// <summary>
    /// 数値をインクリメント（純粋関数）
    /// </summary>
    public static int Increment(int x) => x + 1;

    /// <summary>
    /// 2つの数値を加算（純粋関数）
    /// </summary>
    public static int Add(int a, int b) => a + b;

    /// <summary>
    /// 文字列の最初の文字を取得（純粋関数）
    /// </summary>
    public static char GetFirstCharacter(string s) => s[0];

    /// <summary>
    /// 数値を2倍にする（純粋関数）
    /// </summary>
    public static int Double(int x) => x * 2;

    /// <summary>
    /// 数値が偶数かどうかを判定（純粋関数）
    /// </summary>
    public static bool IsEven(int n) => n % 2 == 0;

    /// <summary>
    /// 文字列を大文字に変換（純粋関数）
    /// </summary>
    public static string ToUpperCase(string s) => s.ToUpper();

    /// <summary>
    /// 文字列を反転（純粋関数）
    /// </summary>
    public static string Reverse(string s) =>
        new string(s.Reverse().ToArray());

    // ============================================
    // 2.2 不純な関数の例（避けるべきパターン）
    // ============================================

    /// <summary>
    /// 不純な関数 - 毎回異なる値を返す
    /// </summary>
    public static double RandomPart(double x) =>
        x * Random.Shared.NextDouble();

    /// <summary>
    /// 不純な関数 - 現在時刻を返す
    /// </summary>
    public static long CurrentTime() =>
        DateTimeOffset.UtcNow.ToUnixTimeMilliseconds();

    // ============================================
    // 2.3 参照透過性
    // ============================================

    /// <summary>
    /// 'a' を除外した文字数をスコアとして返す
    /// </summary>
    public static int WordScore(string word) =>
        word.Replace("a", "").Length;

    /// <summary>
    /// 大文字小文字を区別せずに 'a' を除外
    /// </summary>
    public static int WordScoreIgnoreCase(string word) =>
        word.Where(c => c != 'a' && c != 'A').Count();

    /// <summary>
    /// ボーナス文字を含む場合のスコア計算
    /// </summary>
    public static int ScoreWithBonus(string word, char bonusChar) =>
        word.Count(c => c == bonusChar) * 2 + word.Length;

    // ============================================
    // 2.4 Option を使った安全な操作
    // ============================================

    /// <summary>
    /// 安全な除算
    /// </summary>
    public static Option<int> SafeDivide(int a, int b) =>
        b == 0 ? None : Some(a / b);

    /// <summary>
    /// 安全な平方根
    /// </summary>
    public static Option<double> SafeSqrt(double x) =>
        x < 0 ? None : Some(Math.Sqrt(x));

    /// <summary>
    /// 安全な文字列から整数への変換
    /// </summary>
    public static Option<int> ParseInt(string s) =>
        int.TryParse(s, out var result) ? Some(result) : None;

    /// <summary>
    /// 安全な最初の要素の取得
    /// </summary>
    public static Option<T> SafeHead<T>(Seq<T> seq) =>
        seq.IsEmpty ? Option<T>.None : seq.Head;

    /// <summary>
    /// 安全なインデックスアクセス
    /// </summary>
    public static Option<T> SafeGet<T>(Seq<T> seq, int index) =>
        index >= 0 && index < seq.Count
            ? Some(seq[index])
            : None;
}

/// <summary>
/// ショッピングカートの例
/// </summary>
public static class ShoppingCart
{
    // ============================================
    // 純粋関数によるカート操作
    // ============================================

    /// <summary>
    /// カート内の商品リストから割引率を計算
    /// Book が含まれていれば 5%、それ以外は 0%
    /// </summary>
    public static int GetDiscountPercentage(Seq<string> items) =>
        items.Exists(x => x == "Book") ? 5 : 0;

    /// <summary>
    /// 商品を追加した新しいカートを返す
    /// </summary>
    public static Seq<string> AddItem(Seq<string> items, string item) =>
        items.Add(item);

    /// <summary>
    /// 商品を削除した新しいカートを返す
    /// </summary>
    public static Seq<string> RemoveItem(Seq<string> items, string item) =>
        items.Filter(i => i != item);

    /// <summary>
    /// 複数の条件による割引計算
    /// </summary>
    public static int GetAdvancedDiscountPercentage(Seq<string> items)
    {
        var hasBook = items.Exists(x => x == "Book");
        var hasElectronics = items.Exists(item =>
            item == "Laptop" || item == "Phone" || item == "Tablet");
        var itemCount = items.Count;

        return (hasBook, hasElectronics, itemCount) switch
        {
            (true, true, _) => 15,      // Book + 電子機器 → 15%
            (true, _, >= 5) => 10,      // Book + 5個以上 → 10%
            (true, _, _) => 5,          // Book のみ → 5%
            (_, true, >= 3) => 7,       // 電子機器 + 3個以上 → 7%
            (_, _, >= 10) => 5,         // 10個以上 → 5%
            _ => 0                      // それ以外 → 0%
        };
    }

    /// <summary>
    /// カートの合計金額を計算
    /// </summary>
    public static decimal CalculateTotal(Seq<(string Name, decimal Price)> items) =>
        items.Map(item => item.Price).Fold(0m, (acc, x) => acc + x);

    /// <summary>
    /// 割引適用後の合計金額を計算
    /// </summary>
    public static decimal CalculateTotalWithDiscount(
        Seq<(string Name, decimal Price)> items)
    {
        var total = CalculateTotal(items);
        var discountPercentage = GetAdvancedDiscountPercentage(
            items.Map(item => item.Name));
        return total * (100 - discountPercentage) / 100;
    }
}

/// <summary>
/// チップ計算の例
/// </summary>
public static class TipCalculator
{
    /// <summary>
    /// グループサイズに応じたチップ率を計算
    /// 6人以上 → 20%
    /// 1-5人 → 10%
    /// 0人 → 0%
    /// </summary>
    public static int GetTipPercentage(Seq<string> names)
    {
        var size = names.Count;
        if (size > 5) return 20;
        if (size > 0) return 10;
        return 0;
    }

    /// <summary>
    /// パターンマッチングを使用したチップ率計算
    /// </summary>
    public static int GetTipPercentageMatch(Seq<string> names) =>
        names.Count switch
        {
            > 5 => 20,
            > 0 => 10,
            _ => 0
        };

    /// <summary>
    /// チップ金額を計算
    /// </summary>
    public static decimal CalculateTip(decimal billAmount, Seq<string> names) =>
        billAmount * GetTipPercentage(names) / 100;

    /// <summary>
    /// 一人当たりの支払額を計算（チップ込み）
    /// Option を使ってゼロ除算を防ぐ
    /// </summary>
    public static Option<decimal> CalculatePerPerson(
        decimal billAmount,
        Seq<string> names)
    {
        var size = names.Count;
        if (size == 0)
            return None;

        var tip = CalculateTip(billAmount, names);
        var total = billAmount + tip;
        return Some(total / size);
    }

    /// <summary>
    /// チップを含む明細を生成
    /// </summary>
    public static (decimal Bill, decimal Tip, decimal Total, Option<decimal> PerPerson)
        GenerateBillSummary(decimal billAmount, Seq<string> names)
    {
        var tip = CalculateTip(billAmount, names);
        var total = billAmount + tip;
        var perPerson = CalculatePerPerson(billAmount, names);
        return (billAmount, tip, total, perPerson);
    }
}
