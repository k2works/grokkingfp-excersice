using Ch08;
using LanguageExt;
using static LanguageExt.Prelude;
using Xunit;
using static Ch08.AsyncIO;

namespace GrokkingFP.CSharp.Tests.Ch08;

/// <summary>
/// 第8章: 非同期処理のテスト
/// </summary>
public class AsyncIOTests
{
    // ============================================
    // 基本的な Task のテスト
    // ============================================

    [Fact]
    public async Task PureAsync_ReturnsValue()
    {
        var result = await PureAsync(42);
        Assert.Equal(42, result);
    }

    [Fact]
    public async Task UnitAsync_ReturnsUnit()
    {
        var result = await UnitAsync;
        Assert.Equal(unit, result);
    }

    [Fact]
    public async Task DelayEffect_ExecutesEffect()
    {
        var executed = false;
        var result = await DelayEffect(() =>
        {
            executed = true;
            return 42;
        });
        Assert.True(executed);
        Assert.Equal(42, result);
    }

    // ============================================
    // サイコロのテスト
    // ============================================

    [Fact]
    public void CastTheDieImpure_ReturnsBetween1And6()
    {
        for (var i = 0; i < 100; i++)
        {
            var result = CastTheDieImpure();
            Assert.InRange(result, 1, 6);
        }
    }

    [Fact]
    public async Task CastTheDie_ReturnsBetween1And6()
    {
        for (var i = 0; i < 10; i++)
        {
            var result = await CastTheDie();
            Assert.InRange(result, 1, 6);
        }
    }

    [Fact]
    public async Task CastTheDieTwice_ReturnsBetween2And12()
    {
        for (var i = 0; i < 10; i++)
        {
            var result = await CastTheDieTwice();
            Assert.InRange(result, 2, 12);
        }
    }

    // ============================================
    // ミーティングスケジューリングのテスト
    // ============================================

    [Fact]
    public void MeetingsOverlap_DetectsOverlap()
    {
        var meeting1 = new MeetingTime(9, 10);
        var meeting2 = new MeetingTime(9, 11);
        Assert.True(MeetingsOverlap(meeting1, meeting2));
    }

    [Fact]
    public void MeetingsOverlap_DetectsNoOverlap()
    {
        var meeting1 = new MeetingTime(9, 10);
        var meeting2 = new MeetingTime(10, 11);
        Assert.False(MeetingsOverlap(meeting1, meeting2));
    }

    [Fact]
    public async Task CalendarEntries_ReturnsEntriesForAlice()
    {
        var entries = await CalendarEntries("Alice");
        Assert.Equal(2, entries.Count);
    }

    [Fact]
    public async Task CalendarEntries_ReturnsEmptyForUnknown()
    {
        var entries = await CalendarEntries("Unknown");
        Assert.True(entries.IsEmpty);
    }

    [Fact]
    public async Task ScheduledMeetings_CombinesTwoPersonsEntries()
    {
        var meetings = await ScheduledMeetings("Alice", "Bob");
        Assert.Equal(4, meetings.Count);
    }

    [Fact]
    public void PossibleMeetings_FindsAvailableSlots()
    {
        var existing = Seq(new MeetingTime(9, 10), new MeetingTime(14, 15));
        var possible = PossibleMeetings(existing, 8, 16, 1);
        Assert.Contains(possible, m => m.StartHour == 8);
        Assert.DoesNotContain(possible, m => m.StartHour == 9);
        Assert.Contains(possible, m => m.StartHour == 10);
    }

    [Fact]
    public async Task Schedule_FindsAvailableSlots()
    {
        var slots = await Schedule("Alice", "Bob", 1);
        Assert.True(slots.Count > 0);
    }

    // ============================================
    // OrElse のテスト
    // ============================================

    [Fact]
    public async Task OrElse_ReturnsFirstWhenSucceeds()
    {
        var result = await SucceedingAsync(42).OrElse(SucceedingAsync(100));
        Assert.Equal(42, result);
    }

    [Fact]
    public async Task OrElse_ReturnsFallbackWhenFails()
    {
        var result = await FailingAsync("error").OrElse(SucceedingAsync(100));
        Assert.Equal(100, result);
    }

    // ============================================
    // リトライのテスト
    // ============================================

    [Fact]
    public async Task Retry_ReturnsValueOnSuccess()
    {
        var result = await Retry(3, () => Task.FromResult(42));
        Assert.Equal(42, result);
    }

    [Fact]
    public async Task Retry_RetriesOnFailure()
    {
        var attempts = 0;
        var result = await Retry(3, async () =>
        {
            attempts++;
            if (attempts < 3)
                throw new Exception("fail");
            return 42;
        });
        Assert.Equal(42, result);
        Assert.Equal(3, attempts);
    }

    [Fact]
    public async Task RetryWithDefault_ReturnsDefaultAfterMaxRetries()
    {
        var result = await RetryWithDefault(3, 0, () => Task.FromException<int>(new Exception("fail")));
        Assert.Equal(0, result);
    }

    // ============================================
    // Sequence のテスト
    // ============================================

    [Fact]
    public async Task Sequence_CombinesTasks()
    {
        var tasks = Seq(
            Task.FromResult(1),
            Task.FromResult(2),
            Task.FromResult(3)
        );
        var result = await Sequence(tasks);
        Assert.Equal(Seq(1, 2, 3), result);
    }

    [Fact]
    public async Task ScheduledMeetingsMultiple_CombinesMultipleAttendees()
    {
        var meetings = await ScheduledMeetingsMultiple(Seq("Alice", "Bob", "Charlie"));
        Assert.Equal(5, meetings.Count); // 2 + 2 + 1
    }

    // ============================================
    // 並列実行のテスト
    // ============================================

    [Fact]
    public async Task Parallel2_RunsBothTasks()
    {
        var result = await Parallel2(
            Task.FromResult(1),
            Task.FromResult("hello")
        );
        Assert.Equal((1, "hello"), result);
    }

    [Fact]
    public async Task ParallelAll_RunsAllTasks()
    {
        var tasks = Seq(
            Task.FromResult(1),
            Task.FromResult(2),
            Task.FromResult(3)
        );
        var result = await ParallelAll(tasks);
        Assert.Equal(Seq(1, 2, 3), result);
    }

    // ============================================
    // RunSync のテスト
    // ============================================

    [Fact]
    public void RunSync_ExecutesSynchronously()
    {
        var result = RunSync(Task.FromResult(42));
        Assert.Equal(42, result);
    }

    [Fact]
    public async Task RunWithTimeout_ReturnsValueBeforeTimeout()
    {
        var result = await RunWithTimeout(1000, Task.FromResult(42));
        Assert.True(result.IsSome);
        Assert.Equal(42, result.Match(Some: x => x, None: () => 0));
    }
}

/// <summary>
/// MeetingScheduler のテスト
/// </summary>
public class MeetingSchedulerTests
{
    [Fact]
    public async Task ScheduleAndCreate_ReturnsSlotWhenAvailable()
    {
        var result = await MeetingScheduler.ScheduleAndCreate("Alice", "Charlie", 1);
        Assert.True(result.IsSome);
    }
}
