using LanguageExt;
using static LanguageExt.Prelude;

namespace Ch09;

/// <summary>
/// 第9章: ストリーム処理
/// C# の IEnumerable と LINQ を使った遅延評価のストリーム処理を学ぶ
/// </summary>
public static class StreamProcessing
{
    // ============================================
    // 基本的な Seq（純粋なストリーム）
    // ============================================

    /// <summary>
    /// 有限シーケンス
    /// </summary>
    public static Seq<int> Numbers => Seq(1, 2, 3);

    /// <summary>
    /// フィルタリング
    /// </summary>
    public static Seq<int> OddNumbers(Seq<int> nums) =>
        nums.Filter(n => n % 2 != 0);

    /// <summary>
    /// マッピング
    /// </summary>
    public static Seq<int> Doubled(Seq<int> nums) =>
        nums.Map(n => n * 2);

    // ============================================
    // 無限シーケンス
    // ============================================

    /// <summary>
    /// 無限に繰り返すシーケンス
    /// </summary>
    public static IEnumerable<T> Repeat<T>(IEnumerable<T> elements)
    {
        while (true)
        {
            foreach (var element in elements)
            {
                yield return element;
            }
        }
    }

    /// <summary>
    /// 無限に 1, 2, 3 を繰り返す
    /// </summary>
    public static IEnumerable<int> Infinite123s => Repeat(new[] { 1, 2, 3 });

    /// <summary>
    /// 自然数の無限シーケンス
    /// </summary>
    public static IEnumerable<int> Naturals
    {
        get
        {
            var i = 1;
            while (true)
            {
                yield return i++;
            }
        }
    }

    /// <summary>
    /// 指定値から始まる無限シーケンス
    /// </summary>
    public static IEnumerable<int> From(int start)
    {
        var i = start;
        while (true)
        {
            yield return i++;
        }
    }

    // ============================================
    // サイコロを振るストリーム
    // ============================================

    private static readonly Random _random = new();

    /// <summary>
    /// サイコロを振る（副作用あり）
    /// </summary>
    private static int CastTheDieImpure() =>
        _random.Next(1, 7);

    /// <summary>
    /// サイコロを無限に振るストリーム
    /// </summary>
    public static IEnumerable<int> InfiniteDieCasts
    {
        get
        {
            while (true)
            {
                yield return CastTheDieImpure();
            }
        }
    }

    /// <summary>
    /// 指定した値が出るまで振り続ける（target を含む）
    /// </summary>
    public static IEnumerable<int> CastUntil(int target)
    {
        while (true)
        {
            var n = CastTheDieImpure();
            yield return n;
            if (n == target) yield break;
        }
    }

    // ============================================
    // ストリームの主要操作
    // ============================================

    /// <summary>
    /// 最初の n 要素を取得
    /// </summary>
    public static IEnumerable<T> Take<T>(int n, IEnumerable<T> stream) =>
        stream.Take(n);

    /// <summary>
    /// 条件を満たす要素のみ
    /// </summary>
    public static IEnumerable<T> Filter<T>(Func<T, bool> predicate, IEnumerable<T> stream) =>
        stream.Where(predicate);

    /// <summary>
    /// 各要素を変換
    /// </summary>
    public static IEnumerable<TResult> Map<T, TResult>(Func<T, TResult> f, IEnumerable<T> stream) =>
        stream.Select(f);

    /// <summary>
    /// 2つのストリームを結合
    /// </summary>
    public static IEnumerable<T> Append<T>(IEnumerable<T> stream1, IEnumerable<T> stream2) =>
        stream1.Concat(stream2);

    // ============================================
    // スライディングウィンドウ
    // ============================================

    /// <summary>
    /// スライディングウィンドウを作成
    /// </summary>
    public static IEnumerable<Seq<T>> Sliding<T>(int windowSize, IEnumerable<T> stream)
    {
        var window = new Queue<T>();
        foreach (var item in stream)
        {
            window.Enqueue(item);
            if (window.Count == windowSize)
            {
                yield return toSeq(window);
                window.Dequeue();
            }
        }
    }

    /// <summary>
    /// 連続する要素のペア
    /// </summary>
    public static IEnumerable<(T, T)> Pairwise<T>(IEnumerable<T> stream)
    {
        using var enumerator = stream.GetEnumerator();
        if (!enumerator.MoveNext()) yield break;
        var prev = enumerator.Current;
        while (enumerator.MoveNext())
        {
            var current = enumerator.Current;
            yield return (prev, current);
            prev = current;
        }
    }

    // ============================================
    // 通貨交換レートの例
    // ============================================

    /// <summary>
    /// 通貨を表す列挙型
    /// </summary>
    public enum Currency { USD, EUR, GBP, JPY }

    /// <summary>
    /// 交換レートのシミュレーション
    /// </summary>
    private static decimal GetExchangeRate(Currency from, Currency to) =>
        (from, to) switch
        {
            (Currency.USD, Currency.EUR) => 0.85m + (decimal)(_random.NextDouble() * 0.02 - 0.01),
            (Currency.USD, Currency.GBP) => 0.73m + (decimal)(_random.NextDouble() * 0.02 - 0.01),
            (Currency.USD, Currency.JPY) => 110.0m + (decimal)(_random.NextDouble() * 2.0 - 1.0),
            (Currency.EUR, Currency.USD) => 1.18m + (decimal)(_random.NextDouble() * 0.02 - 0.01),
            _ => 1.0m
        };

    /// <summary>
    /// トレンド判定（上昇傾向）
    /// </summary>
    public static bool Trending(Seq<decimal> rates) =>
        rates.Count > 1 &&
        Pairwise(rates).All(pair => pair.Item2 > pair.Item1);

    /// <summary>
    /// 下降トレンド判定
    /// </summary>
    public static bool Declining(Seq<decimal> rates) =>
        rates.Count > 1 &&
        Pairwise(rates).All(pair => pair.Item2 < pair.Item1);

    /// <summary>
    /// 安定（変動なし）判定
    /// </summary>
    public static bool Stable(Seq<decimal> rates) =>
        rates.Count >= 3 &&
        Enumerable.Count(rates.Distinct()) == 1;

    /// <summary>
    /// レートのストリームを生成
    /// </summary>
    public static IEnumerable<decimal> Rates(Currency from, Currency to)
    {
        while (true)
        {
            yield return GetExchangeRate(from, to);
        }
    }

    /// <summary>
    /// トレンドを検出して交換
    /// </summary>
    public static Option<decimal> ExchangeIfTrending(
        decimal amount,
        Currency from,
        Currency to,
        int windowSize)
    {
        var rateWindow = Sliding(windowSize, Rates(from, to))
            .Where(window => Trending(window))
            .Take(1)
            .FirstOrDefault();

        if (rateWindow.IsEmpty)
            return None;

        var lastRate = rateWindow[rateWindow.Count - 1];
        return Some(amount * lastRate);
    }

    // ============================================
    // ストリームの結合
    // ============================================

    /// <summary>
    /// 2つのストリームを zip
    /// </summary>
    public static IEnumerable<(T1, T2)> Zip<T1, T2>(IEnumerable<T1> stream1, IEnumerable<T2> stream2) =>
        stream1.Zip(stream2);

    /// <summary>
    /// 左側の値を返しつつ、両方のストリームを進める
    /// </summary>
    public static IEnumerable<T1> ZipLeft<T1, T2>(IEnumerable<T1> stream1, IEnumerable<T2> stream2) =>
        Zip(stream1, stream2).Select(pair => pair.Item1);

    /// <summary>
    /// 右側の値を返しつつ、両方のストリームを進める
    /// </summary>
    public static IEnumerable<T2> ZipRight<T1, T2>(IEnumerable<T1> stream1, IEnumerable<T2> stream2) =>
        Zip(stream1, stream2).Select(pair => pair.Item2);

    // ============================================
    // 実用的なストリーム操作
    // ============================================

    /// <summary>
    /// 累積和
    /// </summary>
    public static IEnumerable<int> RunningSum(IEnumerable<int> stream)
    {
        var sum = 0;
        foreach (var item in stream)
        {
            sum += item;
            yield return sum;
        }
    }

    /// <summary>
    /// 移動平均
    /// </summary>
    public static IEnumerable<decimal> MovingAverage(int windowSize, IEnumerable<decimal> stream) =>
        Sliding(windowSize, stream)
            .Select(window => Enumerable.Sum(window) / windowSize);

    /// <summary>
    /// 重複を除去（連続する重複のみ）
    /// </summary>
    public static IEnumerable<T> DistinctConsecutive<T>(IEnumerable<T> stream)
    {
        var first = true;
        T? prev = default;
        foreach (var item in stream)
        {
            if (first || !EqualityComparer<T>.Default.Equals(prev, item))
            {
                yield return item;
                prev = item;
                first = false;
            }
        }
    }

    /// <summary>
    /// チャンク化
    /// </summary>
    public static IEnumerable<Seq<T>> Chunk<T>(int size, IEnumerable<T> stream)
    {
        var chunk = new List<T>();
        foreach (var item in stream)
        {
            chunk.Add(item);
            if (chunk.Count == size)
            {
                yield return toSeq(chunk);
                chunk = new List<T>();
            }
        }
        if (chunk.Count > 0)
        {
            yield return toSeq(chunk);
        }
    }

    // ============================================
    // 遅延評価の実証
    // ============================================

    /// <summary>
    /// 副作用付きの数値生成（遅延評価を確認）
    /// </summary>
    public static IEnumerable<int> NumbersWithSideEffect()
    {
        for (var i = 1; i <= 10; i++)
        {
            Console.WriteLine($"Generating {i}");
            yield return i;
        }
    }

    /// <summary>
    /// 無限ストリームから条件を満たす最初の要素を見つける
    /// </summary>
    public static Option<T> FindFirst<T>(Func<T, bool> predicate, IEnumerable<T> stream)
    {
        foreach (var item in stream)
        {
            if (predicate(item))
                return Some(item);
        }
        return None;
    }

    /// <summary>
    /// 条件を満たす間取得
    /// </summary>
    public static IEnumerable<T> TakeWhile<T>(Func<T, bool> predicate, IEnumerable<T> stream)
    {
        foreach (var item in stream)
        {
            if (!predicate(item)) yield break;
            yield return item;
        }
    }

    /// <summary>
    /// 条件を満たすまでスキップ
    /// </summary>
    public static IEnumerable<T> DropWhile<T>(Func<T, bool> predicate, IEnumerable<T> stream)
    {
        var dropping = true;
        foreach (var item in stream)
        {
            if (dropping && predicate(item))
                continue;
            dropping = false;
            yield return item;
        }
    }
}

/// <summary>
/// 非同期ストリーム（IAsyncEnumerable を使用）
/// </summary>
public static class AsyncStream
{
    /// <summary>
    /// 遅延付きで要素を生成
    /// </summary>
    public static async IAsyncEnumerable<T> RepeatWithDelay<T>(int delayMs, T value)
    {
        while (true)
        {
            await Task.Delay(delayMs);
            yield return value;
        }
    }

    /// <summary>
    /// 非同期シーケンスから最初の n 要素を取得
    /// </summary>
    public static async IAsyncEnumerable<T> Take<T>(int n, IAsyncEnumerable<T> stream)
    {
        var count = 0;
        await foreach (var item in stream)
        {
            if (count >= n) yield break;
            yield return item;
            count++;
        }
    }

    /// <summary>
    /// 非同期シーケンスをリストに変換
    /// </summary>
    public static async Task<Seq<T>> ToSeqAsync<T>(IAsyncEnumerable<T> stream)
    {
        var results = new List<T>();
        await foreach (var item in stream)
        {
            results.Add(item);
        }
        return toSeq(results);
    }

    /// <summary>
    /// 非同期でフィルタリング
    /// </summary>
    public static async IAsyncEnumerable<T> Filter<T>(Func<T, bool> predicate, IAsyncEnumerable<T> stream)
    {
        await foreach (var item in stream)
        {
            if (predicate(item))
                yield return item;
        }
    }

    /// <summary>
    /// 非同期でマッピング
    /// </summary>
    public static async IAsyncEnumerable<TResult> Map<T, TResult>(Func<T, TResult> f, IAsyncEnumerable<T> stream)
    {
        await foreach (var item in stream)
        {
            yield return f(item);
        }
    }
}
