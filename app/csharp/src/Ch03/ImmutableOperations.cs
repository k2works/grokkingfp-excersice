using LanguageExt;
using static LanguageExt.Prelude;

namespace Ch03;

/// <summary>
/// 第3章: イミュータブルなデータ操作
/// リストのスライスと結合操作を学ぶ
/// </summary>
public static class ImmutableOperations
{
    // ============================================
    // 基本的なリスト操作
    // ============================================

    /// <summary>
    /// Seq の先頭 n 個の要素を取得
    /// </summary>
    public static Seq<T> FirstN<T>(Seq<T> seq, int n) =>
        seq.Take(n);

    /// <summary>
    /// Seq の末尾 n 個の要素を取得
    /// </summary>
    public static Seq<T> LastN<T>(Seq<T> seq, int n)
    {
        var skipCount = Math.Max(0, seq.Count - n);
        return seq.Skip(skipCount);
    }

    /// <summary>
    /// Seq の最初の 2 要素を取得
    /// </summary>
    public static Seq<T> FirstTwo<T>(Seq<T> seq) =>
        FirstN(seq, 2);

    /// <summary>
    /// Seq の最後の 2 要素を取得
    /// </summary>
    public static Seq<T> LastTwo<T>(Seq<T> seq) =>
        LastN(seq, 2);

    /// <summary>
    /// Seq のスライス（開始インデックスから終了インデックスまで）
    /// </summary>
    public static Seq<T> Slice<T>(Seq<T> seq, int startIndex, int endIndex) =>
        seq.Skip(startIndex).Take(endIndex - startIndex);

    // ============================================
    // 要素の追加
    // ============================================

    /// <summary>
    /// Seq の末尾に要素を追加（新しい Seq を返す）
    /// </summary>
    public static Seq<T> Appended<T>(Seq<T> seq, T element) =>
        seq.Add(element);

    /// <summary>
    /// Seq の末尾に複数の要素を追加
    /// </summary>
    public static Seq<T> AppendedAll<T>(Seq<T> seq, Seq<T> elements) =>
        seq.Concat(elements);

    // ============================================
    // リスト変換パターン
    // ============================================

    /// <summary>
    /// 最初の 2 要素を末尾に移動
    /// </summary>
    public static Seq<T> MoveFirstTwoToEnd<T>(Seq<T> seq)
    {
        var first = FirstTwo(seq);
        var rest = seq.Skip(2);
        return rest.Concat(first);
    }

    /// <summary>
    /// 最後の要素の前に新しい要素を挿入
    /// </summary>
    public static Seq<T> InsertBeforeLast<T>(Seq<T> seq, T element)
    {
        var withoutLast = seq.Take(seq.Count - 1);
        var last = LastN(seq, 1);
        return withoutLast.Add(element).Concat(last);
    }

    /// <summary>
    /// 中央に要素を挿入
    /// </summary>
    public static Seq<T> InsertAtMiddle<T>(Seq<T> seq, T element)
    {
        var middle = seq.Count / 2;
        var before = seq.Take(middle);
        var after = seq.Skip(middle);
        return before.Add(element).Concat(after);
    }
}

/// <summary>
/// 旅程の再計画
/// </summary>
public static class Itinerary
{
    /// <summary>
    /// 指定した都市の前に新しい都市を挿入
    /// </summary>
    public static Seq<string> Replan(Seq<string> plan, string newCity, string beforeCity)
    {
        var index = plan.ToList().FindIndex(c => c == beforeCity);
        if (index < 0) index = plan.Count;

        var citiesBefore = plan.Take(index);
        var citiesAfter = plan.Skip(index);
        return citiesBefore.Add(newCity).Concat(citiesAfter);
    }
}

/// <summary>
/// 文字列操作
/// </summary>
public static class StringOperations
{
    /// <summary>
    /// 名前を省略形に変換（例: "Alonzo Church" -> "A. Church"）
    /// </summary>
    public static string Abbreviate(string name)
    {
        var initial = name.Substring(0, 1);
        var separatorIndex = name.IndexOf(' ');
        if (separatorIndex < 0)
            return name;

        var lastName = name.Substring(separatorIndex + 1);
        return $"{initial}. {lastName}";
    }

    /// <summary>
    /// 文字列の最初の n 文字を取得
    /// </summary>
    public static string FirstNChars(string s, int n) =>
        n >= s.Length ? s : s.Substring(0, n);

    /// <summary>
    /// 文字列の最後の n 文字を取得
    /// </summary>
    public static string LastNChars(string s, int n) =>
        n >= s.Length ? s : s.Substring(s.Length - n);
}
