using LanguageExt;
using static LanguageExt.Prelude;

namespace Ch07;

/// <summary>
/// 第7章: Either/Result 型と複合的なエラー処理
/// Either を使ってエラーメッセージを保持する方法を学ぶ
/// </summary>
public static class ResultHandling
{
    // ============================================
    // 基本的なデータ型
    // ============================================

    /// <summary>
    /// TV番組を表すレコード型
    /// </summary>
    public record TvShow(string Title, int Start, int End);

    // ============================================
    // 文字列パース関数（Either版）
    // ============================================

    /// <summary>
    /// 文字列を安全に整数に変換（エラーメッセージ付き）
    /// </summary>
    public static Either<string, int> TryParseInt(string s) =>
        int.TryParse(s, out var result)
            ? Right(result)
            : Left($"Can't parse '{s}' as integer");

    // ============================================
    // TV番組パーサー（Either版）
    // ============================================

    /// <summary>
    /// 名前を抽出
    /// </summary>
    public static Either<string, string> ExtractName(string rawShow)
    {
        var bracketOpen = rawShow.IndexOf('(');
        return bracketOpen > 0
            ? Right(rawShow.Substring(0, bracketOpen).Trim())
            : Left($"Can't extract name from '{rawShow}'");
    }

    /// <summary>
    /// 開始年を抽出
    /// </summary>
    public static Either<string, int> ExtractYearStart(string rawShow)
    {
        var bracketOpen = rawShow.IndexOf('(');
        var dash = rawShow.IndexOf('-');
        if (bracketOpen != -1 && dash > bracketOpen + 1)
        {
            var yearStr = rawShow.Substring(bracketOpen + 1, dash - bracketOpen - 1);
            return TryParseInt(yearStr);
        }
        return Left($"Can't extract start year from '{rawShow}'");
    }

    /// <summary>
    /// 終了年を抽出
    /// </summary>
    public static Either<string, int> ExtractYearEnd(string rawShow)
    {
        var dash = rawShow.IndexOf('-');
        var bracketClose = rawShow.IndexOf(')');
        if (dash != -1 && bracketClose > dash + 1)
        {
            var yearStr = rawShow.Substring(dash + 1, bracketClose - dash - 1);
            return TryParseInt(yearStr);
        }
        return Left($"Can't extract end year from '{rawShow}'");
    }

    /// <summary>
    /// 単年を抽出
    /// </summary>
    public static Either<string, int> ExtractSingleYear(string rawShow)
    {
        var dash = rawShow.IndexOf('-');
        var bracketOpen = rawShow.IndexOf('(');
        var bracketClose = rawShow.IndexOf(')');
        if (dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1)
        {
            var yearStr = rawShow.Substring(bracketOpen + 1, bracketClose - bracketOpen - 1);
            return TryParseInt(yearStr);
        }
        return Left($"Can't extract single year from '{rawShow}'");
    }

    /// <summary>
    /// Either の OrElse: 最初が Left なら代替を使用
    /// </summary>
    public static Either<L, R> OrElse<L, R>(this Either<L, R> either, Either<L, R> alternative) =>
        either.IsRight ? either : alternative;

    /// <summary>
    /// TV番組をパース（Either版）
    /// </summary>
    public static Either<string, TvShow> ParseShow(string rawShow) =>
        ExtractName(rawShow).Bind(name =>
        {
            var yearStart = ExtractYearStart(rawShow).OrElse(ExtractSingleYear(rawShow));
            var yearEnd = ExtractYearEnd(rawShow).OrElse(ExtractSingleYear(rawShow));

            return from start in yearStart
                   from endYear in yearEnd
                   select new TvShow(name, start, endYear);
        });

    // ============================================
    // バリデーション
    // ============================================

    /// <summary>
    /// 年齢のバリデーション
    /// </summary>
    public static Either<string, int> ValidateAge(int age) =>
        age < 0 ? Left("Age cannot be negative")
        : age > 150 ? Left("Age cannot be greater than 150")
        : Right(age);

    /// <summary>
    /// 名前のバリデーション
    /// </summary>
    public static Either<string, string> ValidateName(string name) =>
        string.IsNullOrWhiteSpace(name) ? Left("Name cannot be empty")
        : name.Length > 100 ? Left("Name cannot be longer than 100 characters")
        : Right(name);

    /// <summary>
    /// メールアドレスのバリデーション
    /// </summary>
    public static Either<string, string> ValidateEmail(string email) =>
        string.IsNullOrWhiteSpace(email) ? Left("Email cannot be empty")
        : !email.Contains('@') ? Left("Email must contain @")
        : Right(email);
}

/// <summary>
/// 音楽アーティストのモデル
/// </summary>
public static class MusicArtists
{
    // ============================================
    // 代数的データ型（ADT）
    // ============================================

    /// <summary>
    /// 音楽ジャンル（直和型を enum で表現）
    /// </summary>
    public enum MusicGenre
    {
        HeavyMetal,
        Pop,
        HardRock,
        Jazz,
        Classical
    }

    /// <summary>
    /// 活動期間（直和型を抽象クラスで表現）
    /// </summary>
    public abstract record YearsActive;
    public record StillActive(int Since) : YearsActive;
    public record ActiveBetween(int Start, int EndYear) : YearsActive;

    /// <summary>
    /// 地域（直和型を抽象クラスで表現）
    /// </summary>
    public abstract record Location;
    public record US() : Location;
    public record UK() : Location;
    public record England() : Location;
    public record Japan() : Location;
    public record Germany() : Location;
    public record OtherLocation(string Name) : Location;

    /// <summary>
    /// アーティスト（直積型をレコードで表現）
    /// </summary>
    public record Artist(string Name, MusicGenre Genre, Location Origin, YearsActive YearsActive);

    // ============================================
    // パターンマッチング
    // ============================================

    /// <summary>
    /// アーティストが指定期間にアクティブだったか判定
    /// </summary>
    public static bool WasArtistActive(Artist artist, int yearStart, int yearEnd) =>
        artist.YearsActive switch
        {
            StillActive sa => sa.Since <= yearEnd,
            ActiveBetween ab => ab.Start <= yearEnd && ab.EndYear >= yearStart,
            _ => false
        };

    /// <summary>
    /// 活動期間の長さを計算
    /// </summary>
    public static int ActiveLength(Artist artist, int currentYear) =>
        artist.YearsActive switch
        {
            StillActive sa => currentYear - sa.Since,
            ActiveBetween ab => ab.EndYear - ab.Start,
            _ => 0
        };

    /// <summary>
    /// 活動状態の説明を取得
    /// </summary>
    public static string DescribeActivity(YearsActive yearsActive) =>
        yearsActive switch
        {
            StillActive sa => $"Active since {sa.Since}",
            ActiveBetween ab => $"Active from {ab.Start} to {ab.EndYear}",
            _ => "Unknown"
        };

    // ============================================
    // 検索条件のモデリング
    // ============================================

    /// <summary>
    /// 検索条件（直和型を抽象クラスで表現）
    /// </summary>
    public abstract record SearchCondition;
    public record SearchByGenre(Seq<MusicGenre> Genres) : SearchCondition;
    public record SearchByOrigin(Seq<Location> Locations) : SearchCondition;
    public record SearchByActiveYears(int Start, int EndYear) : SearchCondition;

    /// <summary>
    /// 単一の条件でアーティストをチェック
    /// </summary>
    public static bool MatchesCondition(Artist artist, SearchCondition condition) =>
        condition switch
        {
            SearchByGenre sg => sg.Genres.Exists(g => g == artist.Genre),
            SearchByOrigin so => so.Locations.Exists(l => l == artist.Origin),
            SearchByActiveYears say => WasArtistActive(artist, say.Start, say.EndYear),
            _ => false
        };

    /// <summary>
    /// アーティストを検索
    /// </summary>
    public static Seq<Artist> SearchArtists(Seq<Artist> artists, Seq<SearchCondition> conditions) =>
        artists.Filter(artist =>
            conditions.ForAll(condition => MatchesCondition(artist, condition)));
}

/// <summary>
/// 支払い方法の例（パターンマッチングの練習用）
/// </summary>
public static class PaymentMethods
{
    /// <summary>
    /// 支払い方法（直和型を抽象クラスで表現）
    /// </summary>
    public abstract record PaymentMethod;
    public record CreditCard(string Number, string Expiry) : PaymentMethod;
    public record BankTransfer(string AccountNumber) : PaymentMethod;
    public record Cash() : PaymentMethod;

    /// <summary>
    /// 支払い方法の説明を取得
    /// </summary>
    public static string DescribePayment(PaymentMethod method) =>
        method switch
        {
            CreditCard cc => $"Credit card ending in {cc.Number}",
            BankTransfer bt => $"Bank transfer to account {bt.AccountNumber}",
            Cash => "Cash payment",
            _ => "Unknown payment method"
        };

    /// <summary>
    /// 支払い方法が有効か判定
    /// </summary>
    public static bool IsValidPayment(PaymentMethod method) =>
        method switch
        {
            CreditCard cc => cc.Number.Length >= 4 && cc.Expiry.Contains('/'),
            BankTransfer bt => bt.AccountNumber.Length > 0,
            Cash => true,
            _ => false
        };
}
