using System.Threading.Channels;
using LanguageExt;
using static LanguageExt.Prelude;

namespace Ch10;

/// <summary>
/// 第10章: 並行処理
/// C# の Task と lock を使ったスレッドセーフな共有状態管理を学ぶ
/// </summary>
public static class ConcurrentProcessing
{
    // ============================================
    // Ref - スレッドセーフな共有状態
    // ============================================

    /// <summary>
    /// Scala の Ref[IO, A] に相当するスレッドセーフな参照
    /// </summary>
    public class Ref<T>
    {
        private T _value;
        private readonly object _lock = new();

        private Ref(T initialValue)
        {
            _value = initialValue;
        }

        /// <summary>
        /// 初期値で Ref を作成
        /// </summary>
        public static Ref<T> Of(T initialValue) => new(initialValue);

        /// <summary>
        /// 現在の値を取得
        /// </summary>
        public T Get()
        {
            lock (_lock)
            {
                return _value;
            }
        }

        /// <summary>
        /// 値を設定
        /// </summary>
        public void Set(T newValue)
        {
            lock (_lock)
            {
                _value = newValue;
            }
        }

        /// <summary>
        /// アトミックに更新
        /// </summary>
        public void Update(Func<T, T> f)
        {
            lock (_lock)
            {
                _value = f(_value);
            }
        }

        /// <summary>
        /// 更新して古い値を返す
        /// </summary>
        public T GetAndUpdate(Func<T, T> f)
        {
            lock (_lock)
            {
                var oldValue = _value;
                _value = f(_value);
                return oldValue;
            }
        }

        /// <summary>
        /// 更新して新しい値を返す
        /// </summary>
        public T UpdateAndGet(Func<T, T> f)
        {
            lock (_lock)
            {
                _value = f(_value);
                return _value;
            }
        }

        /// <summary>
        /// 更新して結果を返す
        /// </summary>
        public TResult Modify<TResult>(Func<T, (T NewValue, TResult Result)> f)
        {
            lock (_lock)
            {
                var (newValue, result) = f(_value);
                _value = newValue;
                return result;
            }
        }
    }

    // ============================================
    // 並列実行
    // ============================================

    /// <summary>
    /// Task のリストを順次実行
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
    /// Task のリストを並列実行
    /// </summary>
    public static async Task<Seq<T>> ParSequence<T>(Seq<Task<T>> tasks)
    {
        var results = await Task.WhenAll(tasks);
        return toSeq(results);
    }

    /// <summary>
    /// 2つの Task を並列実行
    /// </summary>
    public static async Task<(T1, T2)> ParTuple2<T1, T2>(Task<T1> task1, Task<T2> task2)
    {
        await Task.WhenAll(task1, task2);
        return (task1.Result, task2.Result);
    }

    /// <summary>
    /// 3つの Task を並列実行
    /// </summary>
    public static async Task<(T1, T2, T3)> ParTuple3<T1, T2, T3>(Task<T1> task1, Task<T2> task2, Task<T3> task3)
    {
        await Task.WhenAll(task1, task2, task3);
        return (task1.Result, task2.Result, task3.Result);
    }

    // ============================================
    // Fiber - キャンセル可能な非同期タスク
    // ============================================

    /// <summary>
    /// Scala の Fiber に相当するキャンセル可能な非同期タスク
    /// </summary>
    public class Fiber<T>
    {
        private readonly Task<T> _task;
        private readonly CancellationTokenSource _cts;

        public Fiber(Task<T> task, CancellationTokenSource cts)
        {
            _task = task;
            _cts = cts;
        }

        /// <summary>
        /// タスクの結果を待機
        /// </summary>
        public Task<T> Join => _task;

        /// <summary>
        /// タスクをキャンセル
        /// </summary>
        public void Cancel() => _cts.Cancel();
    }

    /// <summary>
    /// Task を Fiber として起動（バックグラウンドで実行）
    /// </summary>
    public static Fiber<T> Start<T>(Func<CancellationToken, Task<T>> computation)
    {
        var cts = new CancellationTokenSource();
        var task = computation(cts.Token);
        return new Fiber<T>(task, cts);
    }

    /// <summary>
    /// 永遠に繰り返す
    /// </summary>
    public static async Task ForeverM(Func<Task> action, CancellationToken ct = default)
    {
        while (!ct.IsCancellationRequested)
        {
            await action();
        }
    }

    /// <summary>
    /// 指定回数繰り返す
    /// </summary>
    public static async Task<Seq<T>> RepeatN<T>(int n, Func<Task<T>> action)
    {
        var results = new List<T>();
        for (var i = 0; i < n; i++)
        {
            results.Add(await action());
        }
        return toSeq(results);
    }

    // ============================================
    // チェックイン処理の例
    // ============================================

    public record City(string Name);
    public record CityStats(City City, int CheckIns);

    /// <summary>
    /// トップ N 都市を計算（純粋関数）
    /// </summary>
    public static Seq<CityStats> TopCities(int n, Dictionary<City, int> cityCheckIns)
    {
        return toSeq(cityCheckIns
            .Select(kv => new CityStats(kv.Key, kv.Value))
            .OrderByDescending(cs => cs.CheckIns)
            .Take(n));
    }

    /// <summary>
    /// チェックインを保存
    /// </summary>
    public static void StoreCheckIn(Ref<Dictionary<City, int>> storedCheckIns, City city)
    {
        storedCheckIns.Update(dict =>
        {
            var newDict = new Dictionary<City, int>(dict);
            if (newDict.TryGetValue(city, out var count))
            {
                newDict[city] = count + 1;
            }
            else
            {
                newDict[city] = 1;
            }
            return newDict;
        });
    }

    /// <summary>
    /// ランキングを更新
    /// </summary>
    public static void UpdateRanking(
        Ref<Dictionary<City, int>> storedCheckIns,
        Ref<Seq<CityStats>> storedRanking)
    {
        var checkIns = storedCheckIns.Get();
        var ranking = TopCities(3, checkIns);
        storedRanking.Set(ranking);
    }

    // ============================================
    // 処理結果を返すパターン
    // ============================================

    public class ProcessingCheckIns
    {
        private readonly Ref<Seq<CityStats>> _storedRanking;
        private readonly CancellationTokenSource _cts;

        public ProcessingCheckIns(Ref<Seq<CityStats>> storedRanking, CancellationTokenSource cts)
        {
            _storedRanking = storedRanking;
            _cts = cts;
        }

        /// <summary>
        /// 現在のランキングを取得
        /// </summary>
        public Seq<CityStats> CurrentRanking() => _storedRanking.Get();

        /// <summary>
        /// 処理を停止
        /// </summary>
        public void Stop() => _cts.Cancel();
    }

    /// <summary>
    /// チェックイン処理を開始し、制御オブジェクトを返す
    /// </summary>
    public static ProcessingCheckIns StartCheckInProcessing(IEnumerable<City> checkIns)
    {
        var storedCheckIns = Ref<Dictionary<City, int>>.Of(new Dictionary<City, int>());
        var storedRanking = Ref<Seq<CityStats>>.Of(Seq<CityStats>());
        var cts = new CancellationTokenSource();

        // チェックイン処理を開始
        Task.Run(async () =>
        {
            foreach (var city in checkIns)
            {
                if (cts.Token.IsCancellationRequested) break;
                StoreCheckIn(storedCheckIns, city);
            }
        }, cts.Token);

        // ランキング更新を開始
        Task.Run(async () =>
        {
            while (!cts.Token.IsCancellationRequested)
            {
                UpdateRanking(storedCheckIns, storedRanking);
                await Task.Delay(10, cts.Token).ConfigureAwait(false);
            }
        }, cts.Token);

        return new ProcessingCheckIns(storedRanking, cts);
    }
}

/// <summary>
/// Channel を使ったエージェント（Agent）ベースの並行処理
/// F# の MailboxProcessor に相当
/// </summary>
public static class AgentBasedConcurrency
{
    // ============================================
    // カウンターエージェント
    // ============================================

    public abstract record CounterMessage;
    public record Increment : CounterMessage;
    public record Decrement : CounterMessage;
    public record GetValue(TaskCompletionSource<int> Reply) : CounterMessage;
    public record Reset : CounterMessage;

    /// <summary>
    /// スレッドセーフなカウンター（Channel ベース）
    /// </summary>
    public class CounterAgent : IDisposable
    {
        private readonly Channel<CounterMessage> _channel;
        private readonly CancellationTokenSource _cts;
        private readonly int _initialValue;

        public CounterAgent(int initialValue)
        {
            _initialValue = initialValue;
            _channel = Channel.CreateUnbounded<CounterMessage>();
            _cts = new CancellationTokenSource();

            Task.Run(async () => await ProcessMessages(initialValue));
        }

        private async Task ProcessMessages(int initialCount)
        {
            var count = initialCount;
            await foreach (var msg in _channel.Reader.ReadAllAsync(_cts.Token))
            {
                switch (msg)
                {
                    case Increment:
                        count++;
                        break;
                    case Decrement:
                        count--;
                        break;
                    case GetValue gv:
                        gv.Reply.SetResult(count);
                        break;
                    case Reset:
                        count = _initialValue;
                        break;
                }
            }
        }

        public void Post(CounterMessage msg) => _channel.Writer.TryWrite(msg);

        public int PostAndReply()
        {
            var tcs = new TaskCompletionSource<int>();
            _channel.Writer.TryWrite(new GetValue(tcs));
            return tcs.Task.Result;
        }

        public void Dispose()
        {
            _channel.Writer.Complete();
            _cts.Cancel();
            _cts.Dispose();
        }
    }

    /// <summary>
    /// カウンターを作成
    /// </summary>
    public static CounterAgent CreateCounter(int initialValue) => new(initialValue);

    /// <summary>
    /// カウンターの値を取得
    /// </summary>
    public static int GetCounterValue(CounterAgent counter) => counter.PostAndReply();

    /// <summary>
    /// カウンターをインクリメント
    /// </summary>
    public static void IncrementCounter(CounterAgent counter) => counter.Post(new Increment());

    /// <summary>
    /// カウンターをデクリメント
    /// </summary>
    public static void DecrementCounter(CounterAgent counter) => counter.Post(new Decrement());

    // ============================================
    // チェックインエージェント
    // ============================================

    public abstract record CheckInMessage;
    public record AddCheckInMsg(ConcurrentProcessing.City City) : CheckInMessage;
    public record GetStatsMsg(TaskCompletionSource<Seq<ConcurrentProcessing.CityStats>> Reply) : CheckInMessage;
    public record GetTotalMsg(TaskCompletionSource<int> Reply) : CheckInMessage;

    /// <summary>
    /// チェックイン集計エージェント
    /// </summary>
    public class CheckInAgent : IDisposable
    {
        private readonly Channel<CheckInMessage> _channel;
        private readonly CancellationTokenSource _cts;

        public CheckInAgent()
        {
            _channel = Channel.CreateUnbounded<CheckInMessage>();
            _cts = new CancellationTokenSource();

            Task.Run(async () => await ProcessMessages());
        }

        private async Task ProcessMessages()
        {
            var checkIns = new Dictionary<ConcurrentProcessing.City, int>();
            await foreach (var msg in _channel.Reader.ReadAllAsync(_cts.Token))
            {
                switch (msg)
                {
                    case AddCheckInMsg aci:
                        if (checkIns.TryGetValue(aci.City, out var count))
                        {
                            checkIns[aci.City] = count + 1;
                        }
                        else
                        {
                            checkIns[aci.City] = 1;
                        }
                        break;
                    case GetStatsMsg gs:
                        var stats = ConcurrentProcessing.TopCities(3, checkIns);
                        gs.Reply.SetResult(stats);
                        break;
                    case GetTotalMsg gt:
                        var total = checkIns.Values.Sum();
                        gt.Reply.SetResult(total);
                        break;
                }
            }
        }

        public void Post(CheckInMessage msg) => _channel.Writer.TryWrite(msg);

        public Seq<ConcurrentProcessing.CityStats> GetStats()
        {
            var tcs = new TaskCompletionSource<Seq<ConcurrentProcessing.CityStats>>();
            _channel.Writer.TryWrite(new GetStatsMsg(tcs));
            return tcs.Task.Result;
        }

        public int GetTotal()
        {
            var tcs = new TaskCompletionSource<int>();
            _channel.Writer.TryWrite(new GetTotalMsg(tcs));
            return tcs.Task.Result;
        }

        public void Dispose()
        {
            _channel.Writer.Complete();
            _cts.Cancel();
            _cts.Dispose();
        }
    }

    /// <summary>
    /// チェックインエージェントを作成
    /// </summary>
    public static CheckInAgent CreateCheckInAgent() => new();

    /// <summary>
    /// チェックインを追加
    /// </summary>
    public static void AddCheckIn(CheckInAgent agent, ConcurrentProcessing.City city) =>
        agent.Post(new AddCheckInMsg(city));

    /// <summary>
    /// 統計を取得
    /// </summary>
    public static Seq<ConcurrentProcessing.CityStats> GetStats(CheckInAgent agent) =>
        agent.GetStats();

    /// <summary>
    /// 合計を取得
    /// </summary>
    public static int GetTotal(CheckInAgent agent) =>
        agent.GetTotal();
}

/// <summary>
/// 並列処理のユーティリティ
/// </summary>
public static class ParallelUtils
{
    private static readonly Random _random = new();

    // ============================================
    // 並列 map
    // ============================================

    /// <summary>
    /// リストの各要素に関数を並列適用
    /// </summary>
    public static Seq<TResult> ParMap<T, TResult>(Func<T, TResult> f, Seq<T> list)
    {
        var tasks = list.Map(x => Task.Run(() => f(x)));
        return toSeq(Task.WhenAll(tasks).Result);
    }

    /// <summary>
    /// リストの各要素に Task 関数を並列適用
    /// </summary>
    public static async Task<Seq<TResult>> ParMapAsync<T, TResult>(Func<T, Task<TResult>> f, Seq<T> list)
    {
        var tasks = list.Map(f);
        var results = await Task.WhenAll(tasks);
        return toSeq(results);
    }

    // ============================================
    // 並列 filter
    // ============================================

    /// <summary>
    /// 条件を満たす要素を並列でフィルタ
    /// </summary>
    public static Seq<T> ParFilter<T>(Func<T, bool> predicate, Seq<T> list)
    {
        var tasks = list.Map(x => Task.Run(() => (x, predicate(x))));
        var results = Task.WhenAll(tasks).Result;
        return toSeq(results.Where(r => r.Item2).Select(r => r.x));
    }

    // ============================================
    // 並列 fold（reduce）
    // ============================================

    /// <summary>
    /// 並列で集約（結合が結合的な場合のみ正確）
    /// </summary>
    public static Option<T> ParReduce<T>(Func<T, T, T> combine, Seq<T> list)
    {
        if (list.IsEmpty) return None;
        if (list.Count == 1) return Some(list[0]);

        Seq<T> ReduceLevel(Seq<T> items)
        {
            if (items.Count <= 1) return items;

            var chunks = toSeq(items.Chunk(2));
            var tasks = chunks.Map(chunk =>
            {
                var arr = chunk.ToArray();
                return Task.Run(() => arr.Length == 2 ? combine(arr[0], arr[1]) : arr[0]);
            });

            return toSeq(Task.WhenAll(tasks).Result);
        }

        var current = list;
        while (current.Count > 1)
        {
            current = ReduceLevel(current);
        }

        return Some(current[0]);
    }

    // ============================================
    // タイムアウト付き実行
    // ============================================

    /// <summary>
    /// タイムアウト付きで Task を実行
    /// </summary>
    public static async Task<Option<T>> WithTimeout<T>(int timeoutMs, Task<T> task)
    {
        var timeoutTask = Task.Delay(timeoutMs);
        var completedTask = await Task.WhenAny(task, timeoutTask);
        if (completedTask == task)
        {
            return Some(await task);
        }
        return None;
    }

    // ============================================
    // サイコロを並行して振る
    // ============================================

    /// <summary>
    /// サイコロを振る
    /// </summary>
    public static Task<int> CastTheDie() =>
        Task.Run(() => _random.Next(1, 7));

    /// <summary>
    /// N 個のサイコロを並行して振る
    /// </summary>
    public static async Task<Seq<int>> CastDiceConcurrently(int n)
    {
        var tasks = Enumerable.Range(0, n).Select(_ => CastTheDie());
        var results = await Task.WhenAll(tasks);
        return toSeq(results);
    }

    /// <summary>
    /// N 個のサイコロを並行して振り、合計を返す
    /// </summary>
    public static async Task<int> CastDiceAndSum(int n)
    {
        var results = await CastDiceConcurrently(n);
        return Enumerable.Sum(results);
    }

    // ============================================
    // 偶数カウント（並行版）
    // ============================================

    /// <summary>
    /// 並行して実行し、偶数の数をカウント
    /// </summary>
    public static async Task<int> CountEvens(Seq<Task<int>> asyncInts)
    {
        var counter = ConcurrentProcessing.Ref<int>.Of(0);
        var tasks = asyncInts.Map(async asyncInt =>
        {
            var n = await asyncInt;
            if (n % 2 == 0)
            {
                counter.Update(c => c + 1);
            }
        });
        await Task.WhenAll(tasks);
        return counter.Get();
    }
}
