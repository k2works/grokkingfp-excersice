module Ch10.ConcurrentProcessingTests

open Xunit
open Ch10.ConcurrentProcessing
open Ch10.AgentBasedConcurrency
open Ch10.ParallelUtils

// ============================================
// Ref のテスト
// ============================================

[<Fact>]
let ``Ref.Of は初期値で Ref を作成する`` () =
    let ref = Ref.Of(42)
    Assert.Equal(42, ref.Get())

[<Fact>]
let ``Ref.Set は値を設定する`` () =
    let ref = Ref.Of(0)
    ref.Set(100)
    Assert.Equal(100, ref.Get())

[<Fact>]
let ``Ref.Update はアトミックに更新する`` () =
    let ref = Ref.Of(10)
    ref.Update((+) 5)
    Assert.Equal(15, ref.Get())

[<Fact>]
let ``Ref.GetAndUpdate は更新して古い値を返す`` () =
    let ref = Ref.Of(10)
    let oldValue = ref.GetAndUpdate((+) 5)
    Assert.Equal(10, oldValue)
    Assert.Equal(15, ref.Get())

[<Fact>]
let ``Ref.UpdateAndGet は更新して新しい値を返す`` () =
    let ref = Ref.Of(10)
    let newValue = ref.UpdateAndGet((+) 5)
    Assert.Equal(15, newValue)
    Assert.Equal(15, ref.Get())

[<Fact>]
let ``Ref.Modify は更新して結果を返す`` () =
    let ref = Ref.Of(10)
    let result = ref.Modify(fun n -> (n + 5, n * 2))
    Assert.Equal(20, result)  // 古い値 * 2
    Assert.Equal(15, ref.Get())  // 更新後

// ============================================
// 並列実行のテスト
// ============================================

[<Fact>]
let ``parSequence は Async のリストを並列実行する`` () =
    let asyncList = [
        async { return 1 }
        async { return 2 }
        async { return 3 }
    ]
    let result = parSequence asyncList |> Async.RunSynchronously
    Assert.Equal<int list>([1; 2; 3], result)

[<Fact>]
let ``parTuple2 は2つの Async を並列実行する`` () =
    let async1 = async { return 1 }
    let async2 = async { return "hello" }
    let result = parTuple2 async1 async2 |> Async.RunSynchronously
    Assert.Equal((1, "hello"), result)

[<Fact>]
let ``parTuple3 は3つの Async を並列実行する`` () =
    let async1 = async { return 1 }
    let async2 = async { return "hello" }
    let async3 = async { return true }
    let result = parTuple3 async1 async2 async3 |> Async.RunSynchronously
    Assert.Equal((1, "hello", true), result)

// ============================================
// Fiber のテスト
// ============================================

[<Fact>]
let ``start は Async を Fiber として起動する`` () =
    let fiber = start (async { return 42 })
    let result = fiber.Join |> Async.RunSynchronously
    Assert.Equal(42, result)

[<Fact>]
let ``Fiber.Cancel はタスクをキャンセルする`` () =
    let counter = Ref.Of(0)
    let fiber = start (
        async {
            while true do
                counter.Update((+) 1)
                do! Async.Sleep 10
        })

    // 少し待ってからキャンセル
    System.Threading.Thread.Sleep(20)
    fiber.Cancel()

    let countAtCancel = counter.Get()
    System.Threading.Thread.Sleep(20)
    let countAfterWait = counter.Get()

    // キャンセル後はカウンタが増えていないことを確認
    Assert.True(countAtCancel >= 1)
    Assert.True(countAfterWait - countAtCancel <= 1)  // キャンセルまでに1回程度の誤差

[<Fact>]
let ``foreverM は永遠に繰り返す`` () =
    let counter = Ref.Of(0)
    let cts = new System.Threading.CancellationTokenSource()

    Async.Start(
        foreverM (async {
            counter.Update((+) 1)
            do! Async.Sleep 10
        }),
        cts.Token)

    System.Threading.Thread.Sleep(20)
    cts.Cancel()

    Assert.True(counter.Get() >= 1)

[<Fact>]
let ``repeatN は指定回数繰り返す`` () =
    let result = repeatN 5 (async { return 1 }) |> Async.RunSynchronously
    Assert.Equal<int list>([1; 1; 1; 1; 1], result)

// ============================================
// チェックイン処理のテスト
// ============================================

[<Fact>]
let ``topCities はトップ N 都市を返す`` () =
    let checkIns = Map.ofList [
        ({ Name = "Tokyo" }, 100)
        ({ Name = "Sydney" }, 50)
        ({ Name = "London" }, 75)
        ({ Name = "Paris" }, 25)
    ]

    let top3 = topCities 3 checkIns

    Assert.Equal(3, List.length top3)
    Assert.Equal("Tokyo", top3.[0].City.Name)
    Assert.Equal("London", top3.[1].City.Name)
    Assert.Equal("Sydney", top3.[2].City.Name)

[<Fact>]
let ``storeCheckIn はチェックインを保存する`` () =
    let ref = Ref.Of(Map.empty<City, int>)
    let tokyo = { Name = "Tokyo" }

    storeCheckIn ref tokyo
    storeCheckIn ref tokyo
    storeCheckIn ref { Name = "Sydney" }

    let checkIns = ref.Get()
    Assert.Equal(2, checkIns.[tokyo])
    Assert.Equal(1, checkIns.[{ Name = "Sydney" }])

[<Fact>]
let ``startCheckInProcessing は処理を開始して制御を返す`` () =
    let cities = [
        { Name = "Tokyo" }
        { Name = "Sydney" }
        { Name = "Tokyo" }
        { Name = "London" }
        { Name = "Tokyo" }
    ]

    let processing = startCheckInProcessing cities

    // 少し待ってからランキングを取得
    System.Threading.Thread.Sleep(20)
    let ranking = processing.CurrentRanking()

    // 処理を停止
    processing.Stop()

    Assert.True(ranking.Length <= 3)

// ============================================
// Agent (MailboxProcessor) のテスト
// ============================================

[<Fact>]
let ``createCounter はカウンターを作成する`` () =
    let counter = createCounter 0

    Assert.Equal(0, getCounterValue counter)

    incrementCounter counter
    incrementCounter counter
    Assert.Equal(2, getCounterValue counter)

    decrementCounter counter
    Assert.Equal(1, getCounterValue counter)

[<Fact>]
let ``createCheckInAgent はチェックインを集計する`` () =
    let agent = createCheckInAgent ()

    addCheckIn agent { Name = "Tokyo" }
    addCheckIn agent { Name = "Tokyo" }
    addCheckIn agent { Name = "Sydney" }

    // 少し待つ（メッセージ処理のため）
    System.Threading.Thread.Sleep(10)

    let total = getTotal agent
    Assert.Equal(3, total)

    let stats = getStats agent
    Assert.Equal(2, List.length stats)
    Assert.Equal("Tokyo", stats.[0].City.Name)
    Assert.Equal(2, stats.[0].CheckIns)

// ============================================
// ParallelUtils のテスト
// ============================================

[<Fact>]
let ``parMap は並列で map する`` () =
    let result = parMap (fun x -> x * 2) [1; 2; 3; 4; 5]
    Assert.Equal<int list>([2; 4; 6; 8; 10], result)

[<Fact>]
let ``parMapAsync は Async 関数を並列適用する`` () =
    let result =
        parMapAsync (fun x -> async { return x * 2 }) [1; 2; 3]
        |> Async.RunSynchronously
    Assert.Equal<int list>([2; 4; 6], result)

[<Fact>]
let ``parFilter は並列でフィルタする`` () =
    let result = parFilter (fun x -> x % 2 = 0) [1; 2; 3; 4; 5; 6]
    Assert.Equal<int list>([2; 4; 6], result)

[<Fact>]
let ``parReduce は並列で集約する`` () =
    let result = parReduce (+) [1; 2; 3; 4; 5]
    Assert.Equal(Some 15, result)

[<Fact>]
let ``parReduce は空リストで None を返す`` () =
    let result = parReduce (+) ([] : int list)
    Assert.Equal(None, result)

[<Fact>]
let ``withTimeout は時間内に完了したら Some を返す`` () =
    let result =
        withTimeout 1000 (async { return 42 })
        |> Async.RunSynchronously
    Assert.Equal(Some 42, result)

[<Fact>]
let ``castDiceConcurrently は N 個のサイコロを並行して振る`` () =
    let results = castDiceConcurrently 5 |> Async.RunSynchronously
    Assert.Equal(5, List.length results)
    Assert.True(results |> List.forall (fun n -> n >= 1 && n <= 6))

[<Fact>]
let ``castDiceAndSum は合計を返す`` () =
    let sum = castDiceAndSum 3 |> Async.RunSynchronously
    Assert.True(sum >= 3 && sum <= 18)

[<Fact>]
let ``countEvens は偶数の数をカウントする`` () =
    let asyncInts = [
        async { return 2 }
        async { return 3 }
        async { return 4 }
        async { return 5 }
        async { return 6 }
    ]
    let count = countEvens asyncInts |> Async.RunSynchronously
    Assert.Equal(3, count)  // 2, 4, 6

// ============================================
// 並行アクセスの安全性テスト
// ============================================

[<Fact>]
let ``Ref は並行アクセスでも安全`` () =
    let counter = Ref.Of(0)
    let iterations = 1000

    // 並行して increment
    [ for _ in 1..iterations -> async { counter.Update((+) 1) } ]
    |> parSequence
    |> Async.RunSynchronously
    |> ignore

    Assert.Equal(iterations, counter.Get())

[<Fact>]
let ``Agent は並行アクセスでも安全`` () =
    let counter = createCounter 0
    let iterations = 1000

    // 並行して increment
    [ for _ in 1..iterations -> async { incrementCounter counter } ]
    |> parSequence
    |> Async.RunSynchronously
    |> ignore

    // メッセージ処理のため少し待つ
    System.Threading.Thread.Sleep(20)

    Assert.Equal(iterations, getCounterValue counter)
