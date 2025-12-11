using LanguageExt;
using static LanguageExt.Prelude;

namespace Ch05;

/// <summary>
/// 第5章: FlatMap とネスト構造
/// SelectMany（flatMap相当）と LINQ を学ぶ
/// </summary>
public static class FlatMapOperations
{
    // ============================================
    // 基本データ型
    // ============================================

    /// <summary>
    /// 本を表すレコード型
    /// </summary>
    public record Book(string Title, Seq<string> Authors);

    /// <summary>
    /// 映画を表すレコード型
    /// </summary>
    public record Movie(string Title);

    /// <summary>
    /// 点を表すレコード型
    /// </summary>
    public record Point(int X, int Y);

    // ============================================
    // Flatten - ネストした Seq を平坦化
    // ============================================

    /// <summary>
    /// ネストした Seq を平坦化
    /// </summary>
    public static Seq<T> Flatten<T>(Seq<Seq<T>> seqs) =>
        seqs.Bind(x => x);

    // ============================================
    // FlatMap (Bind/SelectMany) - map + flatten
    // ============================================

    /// <summary>
    /// 本のリストから著者のリストを取得
    /// </summary>
    public static Seq<string> GetAllAuthors(Seq<Book> books) =>
        books.Bind(book => book.Authors);

    // ============================================
    // FlatMap によるリストサイズの変化
    // ============================================

    /// <summary>
    /// 各要素を複製（サイズ増加）
    /// </summary>
    public static Seq<int> Duplicate(Seq<int> numbers) =>
        numbers.Bind(i => Seq(i, i + 10));

    /// <summary>
    /// 各要素を2倍にして Seq に（サイズ維持）
    /// </summary>
    public static Seq<int> MapToSeq(Seq<int> numbers) =>
        numbers.Bind(i => Seq(i * 2));

    /// <summary>
    /// 偶数のみをフィルタ（サイズ減少）
    /// </summary>
    public static Seq<int> FilterEven(Seq<int> numbers) =>
        numbers.Bind(i => i % 2 == 0 ? Seq(i) : Empty);

    // ============================================
    // 映画のレコメンデーション
    // ============================================

    /// <summary>
    /// 著者から映画化作品を取得
    /// </summary>
    public static Seq<Movie> BookAdaptations(string author) =>
        author == "Tolkien"
            ? Seq(new Movie("An Unexpected Journey"), new Movie("The Desolation of Smaug"))
            : Empty;

    /// <summary>
    /// 本のリストからレコメンデーションを生成（Bind チェーン）
    /// </summary>
    public static Seq<string> GetRecommendations(Seq<Book> books) =>
        books.Bind(book =>
            book.Authors.Bind(author =>
                BookAdaptations(author).Map(movie =>
                    $"You may like {movie.Title}, because you liked {author}'s {book.Title}")));

    /// <summary>
    /// LINQ を使ったレコメンデーション生成
    /// </summary>
    public static Seq<string> GetRecommendationsLinq(Seq<Book> books) =>
        (from book in books
         from author in book.Authors
         from movie in BookAdaptations(author)
         select $"You may like {movie.Title}, because you liked {author}'s {book.Title}").ToSeq();

    // ============================================
    // 円内の点の判定
    // ============================================

    /// <summary>
    /// 点が指定した半径の円内にあるかを判定
    /// </summary>
    public static bool IsInside(Point point, int radius) =>
        radius * radius >= point.X * point.X + point.Y * point.Y;

    /// <summary>
    /// 全組み合わせを生成（LINQ）
    /// </summary>
    public static Seq<string> AllCombinations(Seq<int> radiuses, Seq<Point> points) =>
        (from r in radiuses
         from point in points
         select $"Point({point.X},{point.Y}) is within a radius of {r}: {IsInside(point, r)}").ToSeq();

    /// <summary>
    /// 条件を満たす組み合わせのみを生成（LINQ + where）
    /// </summary>
    public static Seq<string> InsidePointsOnly(Seq<int> radiuses, Seq<Point> points) =>
        (from r in radiuses
         from point in points
         where IsInside(point, r)
         select $"Point({point.X},{point.Y}) is within a radius of {r}").ToSeq();

    // ============================================
    // LINQ クエリ式
    // ============================================

    /// <summary>
    /// 2つの Seq の全組み合わせを生成
    /// </summary>
    public static Seq<int> ListComprehension(Seq<int> xs, Seq<int> ys) =>
        (from x in xs
         from y in ys
         select x * y).ToSeq();

    /// <summary>
    /// HashSet から Seq への変換を含む全組み合わせ
    /// </summary>
    public static Seq<int> SetToSeqComprehension(System.Collections.Generic.HashSet<int> xs, Seq<int> ys) =>
        toSeq(from x in xs
              from y in ys
              select x * y);

    // ============================================
    // 追加のユーティリティ
    // ============================================

    /// <summary>
    /// 3つの Seq の全組み合わせを生成
    /// </summary>
    public static Seq<int> AllTripleCombinations(Seq<int> xs, Seq<int> ys, Seq<int> zs) =>
        (from x in xs
         from y in ys
         from z in zs
         select x + y + z).ToSeq();

    /// <summary>
    /// Bind を使った偶数フィルタ
    /// </summary>
    public static Seq<int> FilterEvenWithBind(Seq<int> numbers) =>
        numbers.Bind(n => n % 2 == 0 ? Seq(n) : Empty);

    /// <summary>
    /// Option をフィルタ的に使う
    /// </summary>
    public static Seq<T> FilterWithOption<T>(Seq<T> seq, Func<T, bool> predicate) =>
        seq.Bind(x => predicate(x) ? Seq(x) : Empty);
}
