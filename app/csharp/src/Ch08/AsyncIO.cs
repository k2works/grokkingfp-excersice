using LanguageExt;
using static LanguageExt.Prelude;

namespace Ch08;

/// <summary>
/// 第8章: 非同期処理と副作用の管理
/// C# の Task と LanguageExt の Aff/Eff を使って副作用を管理する方法を学ぶ
/// </summary>
public static class AsyncIO
{
    // ============================================
    // 基本的な Task の作成
    // ============================================

    /// <summary>
    /// 副作用のある計算を Task でラップ（IO.delay 相当）
    /// </summary>
    public static Task<T> DelayEffect<T>(Func<T> effect) =>
        Task.Run(effect);

    /// <summary>
    /// 純粋な値を Task でラップ（IO.pure 相当）
    /// </summary>
    public static Task<T> PureAsync<T>(T value) =>
        Task.FromResult(value);

    /// <summary>
    /// 何もしない Task（IO.unit 相当）
    /// </summary>
    public static Task<Unit> UnitAsync =>
        Task.FromResult(unit);

    // ============================================
    // サイコロを振る例
    // ============================================

    private static readonly Random _random = new();

    /// <summary>
    /// 不純な関数（副作用あり）
    /// </summary>
    public static int CastTheDieImpure() =>
        _random.Next(1, 7);

    /// <summary>
    /// Task を使った純粋な記述
    /// </summary>
    public static Task<int> CastTheDie() =>
        Task.Run(CastTheDieImpure);

    /// <summary>
    /// サイコロを2回振って合計を返す
    /// </summary>
    public static async Task<int> CastTheDieTwice()
    {
        var first = await CastTheDie();
        var second = await CastTheDie();
        return first + second;
    }

    // ============================================
    // ミーティングスケジューリング
    // ============================================

    /// <summary>
    /// ミーティング時間を表すレコード型
    /// </summary>
    public record MeetingTime(int StartHour, int EndHour);

    /// <summary>
    /// ミーティングが重なっているか判定
    /// </summary>
    public static bool MeetingsOverlap(MeetingTime meeting1, MeetingTime meeting2) =>
        meeting1.StartHour < meeting2.EndHour && meeting2.StartHour < meeting1.EndHour;

    /// <summary>
    /// API呼び出しをシミュレート（副作用）
    /// </summary>
    private static Seq<MeetingTime> CalendarEntriesApiCall(string name) =>
        name switch
        {
            "Alice" => Seq(new MeetingTime(9, 10), new MeetingTime(14, 15)),
            "Bob" => Seq(new MeetingTime(10, 11), new MeetingTime(15, 16)),
            "Charlie" => Seq(new MeetingTime(11, 12)),
            _ => Seq<MeetingTime>()
        };

    /// <summary>
    /// カレンダーエントリを取得（Task版）
    /// </summary>
    public static Task<Seq<MeetingTime>> CalendarEntries(string name) =>
        Task.Run(() => CalendarEntriesApiCall(name));

    /// <summary>
    /// 2人の予定を取得
    /// </summary>
    public static async Task<Seq<MeetingTime>> ScheduledMeetings(string person1, string person2)
    {
        var entries1 = await CalendarEntries(person1);
        var entries2 = await CalendarEntries(person2);
        return entries1.Concat(entries2);
    }

    /// <summary>
    /// 可能なミーティング時間を計算（純粋関数）
    /// </summary>
    public static Seq<MeetingTime> PossibleMeetings(
        Seq<MeetingTime> existingMeetings,
        int startHour,
        int endHour,
        int lengthHours)
    {
        return toSeq(Enumerable.Range(startHour, endHour - lengthHours - startHour + 1)
            .Select(start => new MeetingTime(start, start + lengthHours))
            .Where(slot => existingMeetings.ForAll(meeting => !MeetingsOverlap(meeting, slot))));
    }

    /// <summary>
    /// ミーティングをスケジュール
    /// </summary>
    public static async Task<Seq<MeetingTime>> Schedule(string person1, string person2, int lengthHours)
    {
        var meetings = await ScheduledMeetings(person1, person2);
        return PossibleMeetings(meetings, 8, 16, lengthHours);
    }

    // ============================================
    // エラーハンドリングと OrElse
    // ============================================

    /// <summary>
    /// 失敗時のフォールバックを指定（orElse 相当）
    /// </summary>
    public static async Task<T> OrElse<T>(this Task<T> primary, Task<T> fallback)
    {
        try
        {
            return await primary;
        }
        catch
        {
            return await fallback;
        }
    }

    /// <summary>
    /// 失敗時のフォールバックを指定（Func版）
    /// </summary>
    public static async Task<T> OrElse<T>(this Task<T> primary, Func<Task<T>> fallback)
    {
        try
        {
            return await primary;
        }
        catch
        {
            return await fallback();
        }
    }

    /// <summary>
    /// 失敗する可能性のある Task
    /// </summary>
    public static Task<int> FailingAsync(string message) =>
        Task.FromException<int>(new Exception(message));

    /// <summary>
    /// 成功する Task
    /// </summary>
    public static Task<int> SucceedingAsync(int value) =>
        Task.FromResult(value);

    // ============================================
    // リトライ戦略
    // ============================================

    /// <summary>
    /// 指定回数リトライする
    /// </summary>
    public static async Task<T> Retry<T>(int maxRetries, Func<Task<T>> action)
    {
        var remaining = maxRetries;
        while (true)
        {
            try
            {
                return await action();
            }
            catch when (remaining > 0)
            {
                remaining--;
            }
        }
    }

    /// <summary>
    /// リトライ後、失敗したらデフォルト値を返す
    /// </summary>
    public static async Task<T> RetryWithDefault<T>(int maxRetries, T defaultValue, Func<Task<T>> action)
    {
        try
        {
            return await Retry(maxRetries, action);
        }
        catch
        {
            return defaultValue;
        }
    }

    // ============================================
    // Sequence: Task<T>[] -> Task<T[]>
    // ============================================

    /// <summary>
    /// Task のリストを List の Task に変換（順次実行）
    /// </summary>
    public static async Task<Seq<T>> Sequence<T>(Seq<Task<T>> tasks)
    {
        var results = new List<T>();
        foreach (var task in tasks)
        {
            results.Add(await task);
        }
        return toSeq(results);
    }

    /// <summary>
    /// 複数人の予定を取得
    /// </summary>
    public static async Task<Seq<MeetingTime>> ScheduledMeetingsMultiple(Seq<string> attendees)
    {
        var tasks = attendees.Map(CalendarEntries);
        var allMeetings = await Sequence(tasks);
        return allMeetings.Bind(x => x);
    }

    // ============================================
    // 並列実行
    // ============================================

    /// <summary>
    /// 2つの Task を並列実行
    /// </summary>
    public static async Task<(T1, T2)> Parallel2<T1, T2>(Task<T1> task1, Task<T2> task2)
    {
        await Task.WhenAll(task1, task2);
        return (task1.Result, task2.Result);
    }

    /// <summary>
    /// Task のリストを並列実行
    /// </summary>
    public static async Task<Seq<T>> ParallelAll<T>(Seq<Task<T>> tasks)
    {
        var results = await Task.WhenAll(tasks);
        return toSeq(results);
    }

    // ============================================
    // 実行ヘルパー
    // ============================================

    /// <summary>
    /// Task を同期的に実行（unsafeRunSync 相当）
    /// </summary>
    public static T RunSync<T>(Task<T> task) =>
        task.GetAwaiter().GetResult();

    /// <summary>
    /// タイムアウト付きで実行
    /// </summary>
    public static async Task<Option<T>> RunWithTimeout<T>(int timeoutMs, Task<T> task)
    {
        var timeoutTask = Task.Delay(timeoutMs);
        var completedTask = await Task.WhenAny(task, timeoutTask);
        if (completedTask == task)
        {
            return Some(await task);
        }
        return None;
    }
}

/// <summary>
/// ミーティングスケジューラーの完全な例
/// </summary>
public static class MeetingScheduler
{
    /// <summary>
    /// ミーティング作成APIをシミュレート
    /// </summary>
    private static void CreateMeetingApiCall(Seq<string> names, AsyncIO.MeetingTime meeting)
    {
        Console.WriteLine($"Created meeting for [{string.Join(", ", names)}] at {meeting.StartHour}:00-{meeting.EndHour}:00");
    }

    /// <summary>
    /// ミーティングを作成（Task版）
    /// </summary>
    public static Task<Unit> CreateMeeting(Seq<string> names, AsyncIO.MeetingTime meeting) =>
        Task.Run(() =>
        {
            CreateMeetingApiCall(names, meeting);
            return unit;
        });

    /// <summary>
    /// 完全なスケジューリングワークフロー
    /// </summary>
    public static async Task<Option<AsyncIO.MeetingTime>> ScheduleAndCreate(
        string person1,
        string person2,
        int lengthHours)
    {
        var possibleSlots = await AsyncIO.Schedule(person1, person2, lengthHours);
        if (possibleSlots.IsEmpty)
        {
            return None;
        }
        var slot = possibleSlots[0];
        await CreateMeeting(Seq(person1, person2), slot);
        return Some(slot);
    }
}
