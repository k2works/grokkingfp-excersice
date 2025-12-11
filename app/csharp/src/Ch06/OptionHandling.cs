using LanguageExt;
using static LanguageExt.Prelude;

namespace Ch06;

/// <summary>
/// 第6章: Option 型による安全なエラーハンドリング
/// Option を使って null や例外を避ける方法を学ぶ
/// </summary>
public static class OptionHandling
{
    // ============================================
    // 基本的なデータ型
    // ============================================

    /// <summary>
    /// TV番組を表すレコード型
    /// </summary>
    public record TvShow(string Title, int Start, int End);

    // ============================================
    // 安全な除算
    // ============================================

    /// <summary>
    /// ゼロ除算を防ぐ安全な除算
    /// </summary>
    public static Option<int> SafeDivide(int a, int b) =>
        b == 0 ? None : Some(a / b);

    // ============================================
    // 文字列パース関数
    // ============================================

    /// <summary>
    /// 文字列を安全に整数に変換
    /// </summary>
    public static Option<int> TryParseInt(string s) =>
        int.TryParse(s, out var result) ? Some(result) : None;

    // ============================================
    // TV番組パーサー
    // ============================================

    /// <summary>
    /// 名前を抽出（例: "Breaking Bad (2008-2013)" → "Breaking Bad"）
    /// </summary>
    public static Option<string> ExtractName(string rawShow)
    {
        var bracketOpen = rawShow.IndexOf('(');
        return bracketOpen > 0
            ? Some(rawShow.Substring(0, bracketOpen).Trim())
            : None;
    }

    /// <summary>
    /// 開始年を抽出（例: "Breaking Bad (2008-2013)" → 2008）
    /// </summary>
    public static Option<int> ExtractYearStart(string rawShow)
    {
        var bracketOpen = rawShow.IndexOf('(');
        var dash = rawShow.IndexOf('-');
        if (bracketOpen != -1 && dash > bracketOpen + 1)
        {
            var yearStr = rawShow.Substring(bracketOpen + 1, dash - bracketOpen - 1);
            return TryParseInt(yearStr);
        }
        return None;
    }

    /// <summary>
    /// 終了年を抽出（例: "Breaking Bad (2008-2013)" → 2013）
    /// </summary>
    public static Option<int> ExtractYearEnd(string rawShow)
    {
        var dash = rawShow.IndexOf('-');
        var bracketClose = rawShow.IndexOf(')');
        if (dash != -1 && bracketClose > dash + 1)
        {
            var yearStr = rawShow.Substring(dash + 1, bracketClose - dash - 1);
            return TryParseInt(yearStr);
        }
        return None;
    }

    /// <summary>
    /// 単年を抽出（例: "Chernobyl (2019)" → 2019）
    /// </summary>
    public static Option<int> ExtractSingleYear(string rawShow)
    {
        var dash = rawShow.IndexOf('-');
        var bracketOpen = rawShow.IndexOf('(');
        var bracketClose = rawShow.IndexOf(')');
        if (dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1)
        {
            var yearStr = rawShow.Substring(bracketOpen + 1, bracketClose - bracketOpen - 1);
            return TryParseInt(yearStr);
        }
        return None;
    }

    /// <summary>
    /// TV番組をパース（Option版）
    /// </summary>
    public static Option<TvShow> ParseShow(string rawShow) =>
        ExtractName(rawShow).Bind(name =>
        {
            var yearStart = ExtractYearStart(rawShow) || ExtractSingleYear(rawShow);
            var yearEnd = ExtractYearEnd(rawShow) || ExtractSingleYear(rawShow);

            return from start in yearStart
                   from endYear in yearEnd
                   select new TvShow(name, start, endYear);
        });

    // ============================================
    // 複数の番組をパースする戦略
    // ============================================

    /// <summary>
    /// Best-effort 戦略: パースできたものだけ返す
    /// </summary>
    public static Seq<TvShow> ParseShowsBestEffort(Seq<string> rawShows) =>
        rawShows.Bind(raw => ParseShow(raw).ToSeq());

    /// <summary>
    /// All-or-nothing 戦略: 全部成功するか、全部失敗
    /// </summary>
    public static Option<Seq<TvShow>> ParseShowsAllOrNothing(Seq<string> rawShows)
    {
        var parsed = rawShows.Map(ParseShow);
        return parsed.ForAll(opt => opt.IsSome)
            ? Some(parsed.Bind(opt => opt.ToSeq()))
            : None;
    }

    // ============================================
    // Option のユーティリティ
    // ============================================

    /// <summary>
    /// 2つの数値文字列を加算
    /// </summary>
    public static Option<int> AddStrings(string a, string b) =>
        from x in TryParseInt(a)
        from y in TryParseInt(b)
        select x + y;

    // ============================================
    // forall と exists
    // ============================================

    /// <summary>
    /// Option.ForAll の例
    /// None なら true、Some なら predicate の結果
    /// </summary>
    public static bool ForAllExample(Option<int> opt, int threshold) =>
        opt.ForAll(x => x < threshold);

    /// <summary>
    /// Option.Exists の例
    /// None なら false、Some なら predicate の結果
    /// </summary>
    public static bool ExistsExample(Option<int> opt, int threshold) =>
        opt.Exists(x => x < threshold);
}

/// <summary>
/// ユーザーモデル（ForAll/Exists の例題用）
/// </summary>
public static class UserModel
{
    /// <summary>
    /// ユーザーを表すレコード型
    /// </summary>
    public record User(string Name, Option<string> Email, int Age);

    /// <summary>
    /// メールアドレスが設定されていないか、指定ドメインのユーザーをフィルタ
    /// ForAll: None → true, Some → predicate の結果
    /// </summary>
    public static Seq<User> FilterByEmailDomain(Seq<User> users, string domain) =>
        users.Filter(user =>
            user.Email.ForAll(email => email.EndsWith(domain)));

    /// <summary>
    /// メールアドレスが設定されていて、指定ドメインのユーザーをフィルタ
    /// Exists: None → false, Some → predicate の結果
    /// </summary>
    public static Seq<User> FilterByEmailDomainExists(Seq<User> users, string domain) =>
        users.Filter(user =>
            user.Email.Exists(email => email.EndsWith(domain)));
}
