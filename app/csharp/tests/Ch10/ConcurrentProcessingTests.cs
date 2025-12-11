using Ch10;
using LanguageExt;
using static LanguageExt.Prelude;
using Xunit;

namespace GrokkingFP.CSharp.Tests.Ch10;

/// <summary>
/// 第10章: 並行処理のテスト
/// </summary>
public class RefTests
{
    [Fact]
    public void Ref_Of_CreatesRefWithInitialValue()
    {
        var r = ConcurrentProcessing.Ref<int>.Of(42);
        Assert.Equal(42, r.Get());
    }

    [Fact]
    public void Ref_Set_SetsValue()
    {
        var r = ConcurrentProcessing.Ref<int>.Of(0);
        r.Set(100);
        Assert.Equal(100, r.Get());
    }

    [Fact]
    public void Ref_Update_UpdatesAtomically()
    {
        var r = ConcurrentProcessing.Ref<int>.Of(10);
        r.Update(n => n + 5);
        Assert.Equal(15, r.Get());
    }

    [Fact]
    public void Ref_GetAndUpdate_ReturnsOldValueAndUpdates()
    {
        var r = ConcurrentProcessing.Ref<int>.Of(10);
        var oldValue = r.GetAndUpdate(n => n + 5);
        Assert.Equal(10, oldValue);
        Assert.Equal(15, r.Get());
    }

    [Fact]
    public void Ref_UpdateAndGet_ReturnsNewValue()
    {
        var r = ConcurrentProcessing.Ref<int>.Of(10);
        var newValue = r.UpdateAndGet(n => n + 5);
        Assert.Equal(15, newValue);
        Assert.Equal(15, r.Get());
    }

    [Fact]
    public void Ref_Modify_UpdatesAndReturnsResult()
    {
        var r = ConcurrentProcessing.Ref<int>.Of(10);
        var result = r.Modify(n => (n + 5, n * 2));
        Assert.Equal(20, result); // 古い値 * 2
        Assert.Equal(15, r.Get()); // 更新後
    }

    [Fact]
    public void Ref_IsSafeUnderConcurrentAccess()
    {
        var counter = ConcurrentProcessing.Ref<int>.Of(0);
        const int iterations = 1000;

        // 並行して increment
        var tasks = Enumerable.Range(0, iterations)
            .Select(_ => Task.Run(() => counter.Update(c => c + 1)))
            .ToArray();

        Task.WhenAll(tasks).Wait();

        Assert.Equal(iterations, counter.Get());
    }
}

public class ParallelExecutionTests
{
    [Fact]
    public async Task ParSequence_ExecutesTasksInParallel()
    {
        var tasks = Seq(
            Task.FromResult(1),
            Task.FromResult(2),
            Task.FromResult(3)
        );
        var result = await ConcurrentProcessing.ParSequence(tasks);
        Assert.Equal(Seq(1, 2, 3), result);
    }

    [Fact]
    public async Task ParTuple2_ExecutesTwoTasksInParallel()
    {
        var task1 = Task.FromResult(1);
        var task2 = Task.FromResult("hello");
        var result = await ConcurrentProcessing.ParTuple2(task1, task2);
        Assert.Equal((1, "hello"), result);
    }

    [Fact]
    public async Task ParTuple3_ExecutesThreeTasksInParallel()
    {
        var task1 = Task.FromResult(1);
        var task2 = Task.FromResult("hello");
        var task3 = Task.FromResult(true);
        var result = await ConcurrentProcessing.ParTuple3(task1, task2, task3);
        Assert.Equal((1, "hello", true), result);
    }
}

public class FiberTests
{
    [Fact]
    public async Task Start_CreatesFiberThatReturnsResult()
    {
        var fiber = ConcurrentProcessing.Start(async ct => 42);
        var result = await fiber.Join;
        Assert.Equal(42, result);
    }

    [Fact]
    public async Task Fiber_Cancel_StopsExecution()
    {
        var counter = ConcurrentProcessing.Ref<int>.Of(0);
        var fiber = ConcurrentProcessing.Start(async ct =>
        {
            while (!ct.IsCancellationRequested)
            {
                counter.Update(c => c + 1);
                await Task.Delay(10, ct);
            }
            return counter.Get();
        });

        // 少し待ってからキャンセル
        await Task.Delay(30);
        fiber.Cancel();

        var countAtCancel = counter.Get();
        await Task.Delay(30);
        var countAfterWait = counter.Get();

        // キャンセル後はカウンタが増えていないことを確認
        Assert.True(countAtCancel >= 1);
        Assert.True(countAfterWait - countAtCancel <= 1);
    }

    [Fact]
    public async Task RepeatN_RepeatsNTimes()
    {
        var result = await ConcurrentProcessing.RepeatN(5, () => Task.FromResult(1));
        Assert.Equal(Seq(1, 1, 1, 1, 1), result);
    }
}

public class CheckInProcessingTests
{
    [Fact]
    public void TopCities_ReturnsTopNCities()
    {
        var checkIns = new Dictionary<ConcurrentProcessing.City, int>
        {
            { new ConcurrentProcessing.City("Tokyo"), 100 },
            { new ConcurrentProcessing.City("Sydney"), 50 },
            { new ConcurrentProcessing.City("London"), 75 },
            { new ConcurrentProcessing.City("Paris"), 25 }
        };

        var top3 = ConcurrentProcessing.TopCities(3, checkIns);

        Assert.Equal(3, top3.Count);
        Assert.Equal("Tokyo", top3[0].City.Name);
        Assert.Equal("London", top3[1].City.Name);
        Assert.Equal("Sydney", top3[2].City.Name);
    }

    [Fact]
    public void StoreCheckIn_StoresCheckIns()
    {
        var r = ConcurrentProcessing.Ref<Dictionary<ConcurrentProcessing.City, int>>.Of(
            new Dictionary<ConcurrentProcessing.City, int>());
        var tokyo = new ConcurrentProcessing.City("Tokyo");

        ConcurrentProcessing.StoreCheckIn(r, tokyo);
        ConcurrentProcessing.StoreCheckIn(r, tokyo);
        ConcurrentProcessing.StoreCheckIn(r, new ConcurrentProcessing.City("Sydney"));

        var checkIns = r.Get();
        Assert.Equal(2, checkIns[tokyo]);
        Assert.Equal(1, checkIns[new ConcurrentProcessing.City("Sydney")]);
    }

    [Fact]
    public async Task StartCheckInProcessing_StartsProcessingAndReturnsControl()
    {
        var cities = new[]
        {
            new ConcurrentProcessing.City("Tokyo"),
            new ConcurrentProcessing.City("Sydney"),
            new ConcurrentProcessing.City("Tokyo"),
            new ConcurrentProcessing.City("London"),
            new ConcurrentProcessing.City("Tokyo")
        };

        var processing = ConcurrentProcessing.StartCheckInProcessing(cities);

        // 少し待ってからランキングを取得
        await Task.Delay(30);
        var ranking = processing.CurrentRanking();

        // 処理を停止
        processing.Stop();

        Assert.True(ranking.Count <= 3);
    }
}

public class AgentTests
{
    [Fact]
    public void CreateCounter_CreatesWorkingCounter()
    {
        using var counter = AgentBasedConcurrency.CreateCounter(0);

        Assert.Equal(0, AgentBasedConcurrency.GetCounterValue(counter));

        AgentBasedConcurrency.IncrementCounter(counter);
        AgentBasedConcurrency.IncrementCounter(counter);
        Thread.Sleep(20); // メッセージ処理のため待機
        Assert.Equal(2, AgentBasedConcurrency.GetCounterValue(counter));

        AgentBasedConcurrency.DecrementCounter(counter);
        Thread.Sleep(20);
        Assert.Equal(1, AgentBasedConcurrency.GetCounterValue(counter));
    }

    [Fact]
    public void CreateCheckInAgent_AggregatesCheckIns()
    {
        using var agent = AgentBasedConcurrency.CreateCheckInAgent();

        AgentBasedConcurrency.AddCheckIn(agent, new ConcurrentProcessing.City("Tokyo"));
        AgentBasedConcurrency.AddCheckIn(agent, new ConcurrentProcessing.City("Tokyo"));
        AgentBasedConcurrency.AddCheckIn(agent, new ConcurrentProcessing.City("Sydney"));

        Thread.Sleep(20); // メッセージ処理のため待機

        var total = AgentBasedConcurrency.GetTotal(agent);
        Assert.Equal(3, total);

        var stats = AgentBasedConcurrency.GetStats(agent);
        Assert.Equal(2, stats.Count);
        Assert.Equal("Tokyo", stats[0].City.Name);
        Assert.Equal(2, stats[0].CheckIns);
    }

    [Fact]
    public void Agent_IsSafeUnderConcurrentAccess()
    {
        using var counter = AgentBasedConcurrency.CreateCounter(0);
        const int iterations = 1000;

        // 並行して increment
        var tasks = Enumerable.Range(0, iterations)
            .Select(_ => Task.Run(() => AgentBasedConcurrency.IncrementCounter(counter)))
            .ToArray();

        Task.WhenAll(tasks).Wait();

        // メッセージ処理のため待機
        Thread.Sleep(50);

        Assert.Equal(iterations, AgentBasedConcurrency.GetCounterValue(counter));
    }
}

public class ParallelUtilsTests
{
    [Fact]
    public void ParMap_AppliesFunctionInParallel()
    {
        var result = ParallelUtils.ParMap(x => x * 2, Seq(1, 2, 3, 4, 5));
        Assert.Equal(Seq(2, 4, 6, 8, 10), result);
    }

    [Fact]
    public async Task ParMapAsync_AppliesAsyncFunctionInParallel()
    {
        var result = await ParallelUtils.ParMapAsync(
            x => Task.FromResult(x * 2),
            Seq(1, 2, 3));
        Assert.Equal(Seq(2, 4, 6), result);
    }

    [Fact]
    public void ParFilter_FiltersInParallel()
    {
        var result = ParallelUtils.ParFilter(x => x % 2 == 0, Seq(1, 2, 3, 4, 5, 6));
        Assert.Equal(Seq(2, 4, 6), result);
    }

    [Fact]
    public void ParReduce_ReducesInParallel()
    {
        var result = ParallelUtils.ParReduce((a, b) => a + b, Seq(1, 2, 3, 4, 5));
        Assert.Equal(Some(15), result);
    }

    [Fact]
    public void ParReduce_ReturnsNoneForEmptyList()
    {
        var result = ParallelUtils.ParReduce((a, b) => a + b, Seq<int>());
        Assert.True(result.IsNone);
    }

    [Fact]
    public async Task WithTimeout_ReturnsSomeWhenCompletedInTime()
    {
        var result = await ParallelUtils.WithTimeout(1000, Task.FromResult(42));
        Assert.Equal(Some(42), result);
    }

    [Fact]
    public async Task CastDiceConcurrently_ReturnsNDiceResults()
    {
        var results = await ParallelUtils.CastDiceConcurrently(5);
        Assert.Equal(5, results.Count);
        Assert.True(results.ForAll(n => n >= 1 && n <= 6));
    }

    [Fact]
    public async Task CastDiceAndSum_ReturnsSumOfDice()
    {
        var sum = await ParallelUtils.CastDiceAndSum(3);
        Assert.True(sum >= 3 && sum <= 18);
    }

    [Fact]
    public async Task CountEvens_CountsEvenNumbers()
    {
        var asyncInts = Seq(
            Task.FromResult(2),
            Task.FromResult(3),
            Task.FromResult(4),
            Task.FromResult(5),
            Task.FromResult(6)
        );
        var count = await ParallelUtils.CountEvens(asyncInts);
        Assert.Equal(3, count); // 2, 4, 6
    }
}
