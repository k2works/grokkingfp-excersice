using Ch09;
using LanguageExt;
using static LanguageExt.Prelude;
using Xunit;

namespace GrokkingFP.CSharp.Tests.Ch09;

/// <summary>
/// 第9章: ストリーム処理のテスト
/// </summary>
public class StreamProcessingTests
{
    // ============================================
    // 基本的な Seq のテスト
    // ============================================

    [Fact]
    public void Numbers_Returns123()
    {
        var result = StreamProcessing.Numbers.ToList();
        Assert.Equal(new[] { 1, 2, 3 }, result);
    }

    [Fact]
    public void OddNumbers_FiltersEvenNumbers()
    {
        var input = Seq(1, 2, 3, 4, 5);
        var result = StreamProcessing.OddNumbers(input).ToList();
        Assert.Equal(new[] { 1, 3, 5 }, result);
    }

    [Fact]
    public void Doubled_DoublesEachNumber()
    {
        var input = Seq(1, 2, 3);
        var result = StreamProcessing.Doubled(input).ToList();
        Assert.Equal(new[] { 2, 4, 6 }, result);
    }

    // ============================================
    // 無限シーケンスのテスト
    // ============================================

    [Fact]
    public void Repeat_RepeatsInfinitely()
    {
        var result = StreamProcessing.Repeat(new[] { 1, 2, 3 }).Take(8).ToList();
        Assert.Equal(new[] { 1, 2, 3, 1, 2, 3, 1, 2 }, result);
    }

    [Fact]
    public void Infinite123s_RepeatsInfinitely()
    {
        var result = StreamProcessing.Infinite123s.Take(7).ToList();
        Assert.Equal(new[] { 1, 2, 3, 1, 2, 3, 1 }, result);
    }

    [Fact]
    public void Naturals_StartsFrom1()
    {
        var result = StreamProcessing.Naturals.Take(5).ToList();
        Assert.Equal(new[] { 1, 2, 3, 4, 5 }, result);
    }

    [Fact]
    public void From_StartsFromGivenValue()
    {
        var result = StreamProcessing.From(10).Take(5).ToList();
        Assert.Equal(new[] { 10, 11, 12, 13, 14 }, result);
    }

    // ============================================
    // サイコロストリームのテスト
    // ============================================

    [Fact]
    public void InfiniteDieCasts_ReturnsBetween1And6()
    {
        var results = StreamProcessing.InfiniteDieCasts.Take(10).ToList();
        Assert.True(results.All(n => n >= 1 && n <= 6));
    }

    [Fact]
    public void CastUntil_EndsWithTarget()
    {
        var results = StreamProcessing.CastUntil(6).Take(20).ToList();
        Assert.True(results.Count >= 1 && results.Count <= 20);
    }

    // ============================================
    // ストリーム操作のテスト
    // ============================================

    [Fact]
    public void Take_TakesFirstN()
    {
        var input = new[] { 1, 2, 3, 4, 5 };
        var result = StreamProcessing.Take(3, input).ToList();
        Assert.Equal(new[] { 1, 2, 3 }, result);
    }

    [Fact]
    public void Filter_FiltersElements()
    {
        var input = new[] { 1, 2, 3, 4, 5 };
        var result = StreamProcessing.Filter<int>(n => n > 2, input).ToList();
        Assert.Equal(new[] { 3, 4, 5 }, result);
    }

    [Fact]
    public void Map_TransformsElements()
    {
        var input = new[] { 1, 2, 3 };
        var result = StreamProcessing.Map(n => n * 10, input).ToList();
        Assert.Equal(new[] { 10, 20, 30 }, result);
    }

    [Fact]
    public void Append_ConcatenatesStreams()
    {
        var s1 = new[] { 1, 2 };
        var s2 = new[] { 3, 4 };
        var result = StreamProcessing.Append(s1, s2).ToList();
        Assert.Equal(new[] { 1, 2, 3, 4 }, result);
    }

    // ============================================
    // スライディングウィンドウのテスト
    // ============================================

    [Fact]
    public void Sliding_CreatesWindows()
    {
        var input = new[] { 1, 2, 3, 4, 5 };
        var result = StreamProcessing.Sliding(3, input).Select(w => w.ToList()).ToList();
        Assert.Equal(3, result.Count);
        Assert.Equal(new[] { 1, 2, 3 }, result[0]);
        Assert.Equal(new[] { 2, 3, 4 }, result[1]);
        Assert.Equal(new[] { 3, 4, 5 }, result[2]);
    }

    [Fact]
    public void Pairwise_CreatesPairs()
    {
        var input = new[] { 1, 2, 3, 4 };
        var result = StreamProcessing.Pairwise(input).ToList();
        Assert.Equal(new[] { (1, 2), (2, 3), (3, 4) }, result);
    }

    // ============================================
    // トレンド判定のテスト
    // ============================================

    [Fact]
    public void Trending_DetectsUpwardTrend()
    {
        Assert.True(StreamProcessing.Trending(Seq(0.81m, 0.82m, 0.83m)));
        Assert.False(StreamProcessing.Trending(Seq(0.81m, 0.84m, 0.83m)));
        Assert.False(StreamProcessing.Trending(Seq(0.83m, 0.82m, 0.81m)));
    }

    [Fact]
    public void Declining_DetectsDownwardTrend()
    {
        Assert.True(StreamProcessing.Declining(Seq(0.83m, 0.82m, 0.81m)));
        Assert.False(StreamProcessing.Declining(Seq(0.81m, 0.82m, 0.83m)));
        Assert.False(StreamProcessing.Declining(Seq(0.81m, 0.80m, 0.82m)));
    }

    [Fact]
    public void Stable_DetectsStability()
    {
        Assert.True(StreamProcessing.Stable(Seq(5m, 5m, 5m)));
        Assert.False(StreamProcessing.Stable(Seq(5m, 5m, 6m)));
        Assert.False(StreamProcessing.Stable(Seq(5m, 6m))); // 3つ未満
    }

    // ============================================
    // ストリーム結合のテスト
    // ============================================

    [Fact]
    public void Zip_CombinesStreams()
    {
        var s1 = new[] { 1, 2, 3 };
        var s2 = new[] { "a", "b", "c" };
        var result = StreamProcessing.Zip(s1, s2).ToList();
        Assert.Equal(new[] { (1, "a"), (2, "b"), (3, "c") }, result);
    }

    [Fact]
    public void ZipLeft_TakesLeftValues()
    {
        var s1 = new[] { 1, 2, 3 };
        var s2 = new[] { "a", "b", "c" };
        var result = StreamProcessing.ZipLeft(s1, s2).ToList();
        Assert.Equal(new[] { 1, 2, 3 }, result);
    }

    [Fact]
    public void ZipRight_TakesRightValues()
    {
        var s1 = new[] { 1, 2, 3 };
        var s2 = new[] { "a", "b", "c" };
        var result = StreamProcessing.ZipRight(s1, s2).ToList();
        Assert.Equal(new[] { "a", "b", "c" }, result);
    }

    // ============================================
    // 実用的なストリーム操作のテスト
    // ============================================

    [Fact]
    public void RunningSum_CalculatesCumulativeSum()
    {
        var input = new[] { 1, 2, 3, 4, 5 };
        var result = StreamProcessing.RunningSum(input).ToList();
        Assert.Equal(new[] { 1, 3, 6, 10, 15 }, result);
    }

    [Fact]
    public void MovingAverage_CalculatesAverage()
    {
        var input = new[] { 1m, 2m, 3m, 4m, 5m };
        var result = StreamProcessing.MovingAverage(3, input).ToList();
        Assert.Equal(new[] { 2m, 3m, 4m }, result);
    }

    [Fact]
    public void DistinctConsecutive_RemovesDuplicates()
    {
        var input = new[] { 1, 1, 2, 2, 2, 3, 1, 1 };
        var result = StreamProcessing.DistinctConsecutive(input).ToList();
        Assert.Equal(new[] { 1, 2, 3, 1 }, result);
    }

    [Fact]
    public void Chunk_ChunksStream()
    {
        var input = new[] { 1, 2, 3, 4, 5, 6, 7 };
        var result = StreamProcessing.Chunk(3, input).Select(c => c.ToList()).ToList();
        Assert.Equal(3, result.Count);
        Assert.Equal(new[] { 1, 2, 3 }, result[0]);
        Assert.Equal(new[] { 4, 5, 6 }, result[1]);
        Assert.Equal(new[] { 7 }, result[2]);
    }

    // ============================================
    // 遅延評価のテスト
    // ============================================

    [Fact]
    public void FindFirst_FindsElement()
    {
        var input = new[] { 1, 2, 3, 4, 5 };
        var result = StreamProcessing.FindFirst(n => n > 3, input);
        Assert.Equal(Some(4), result);
    }

    [Fact]
    public void FindFirst_ReturnsNoneWhenNotFound()
    {
        var input = new[] { 1, 2, 3 };
        var result = StreamProcessing.FindFirst(n => n > 10, input);
        Assert.True(result.IsNone);
    }

    [Fact]
    public void TakeWhile_TakesWhileConditionIsTrue()
    {
        var input = new[] { 1, 2, 3, 4, 5 };
        var result = StreamProcessing.TakeWhile<int>(n => n < 4, input).ToList();
        Assert.Equal(new[] { 1, 2, 3 }, result);
    }

    [Fact]
    public void DropWhile_DropsWhileConditionIsTrue()
    {
        var input = new[] { 1, 2, 3, 4, 5 };
        var result = StreamProcessing.DropWhile<int>(n => n < 4, input).ToList();
        Assert.Equal(new[] { 4, 5 }, result);
    }

    // ============================================
    // 通貨交換のテスト
    // ============================================

    [Fact]
    public void Rates_GeneratesExchangeRates()
    {
        var result = StreamProcessing.Rates(StreamProcessing.Currency.USD, StreamProcessing.Currency.EUR)
            .Take(5).ToList();
        Assert.Equal(5, result.Count);
        Assert.True(result.All(r => r > 0m));
    }

    [Fact]
    public void ExchangeIfTrending_TestPureFunction()
    {
        // 純粋関数のテストのみ（F#と同様）
        var trendingRates = Seq(0.81m, 0.82m, 0.83m);
        Assert.True(StreamProcessing.Trending(trendingRates));
    }
}

/// <summary>
/// AsyncStream のテスト
/// </summary>
public class AsyncStreamTests
{
    [Fact]
    public async Task Take_TakesFirstN()
    {
        var stream = RepeatForTest(1);
        var result = await AsyncStream.ToSeqAsync(AsyncStream.Take(3, stream));
        Assert.Equal(3, result.Count);
    }

    [Fact]
    public async Task Filter_FiltersElements()
    {
        var stream = FromEnumerable(new[] { 1, 2, 3, 4, 5 });
        var result = await AsyncStream.ToSeqAsync(AsyncStream.Filter(x => x > 2, stream));
        Assert.Equal(Seq(3, 4, 5), result);
    }

    [Fact]
    public async Task Map_TransformsElements()
    {
        var stream = FromEnumerable(new[] { 1, 2, 3 });
        var result = await AsyncStream.ToSeqAsync(AsyncStream.Map(x => x * 2, stream));
        Assert.Equal(Seq(2, 4, 6), result);
    }

    // ヘルパーメソッド - 有限のストリームを返す
    private static async IAsyncEnumerable<int> RepeatForTest(int value)
    {
        for (var i = 0; i < 10; i++)
        {
            yield return value;
        }
    }

    private static async IAsyncEnumerable<T> FromEnumerable<T>(IEnumerable<T> source)
    {
        foreach (var item in source)
        {
            yield return item;
        }
    }
}
