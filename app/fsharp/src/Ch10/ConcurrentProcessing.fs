namespace Ch10

/// F# での並行処理
/// Scala の Ref と Fiber に相当する機能を F# の MailboxProcessor と Async で実現
module ConcurrentProcessing =

    open System
    open System.Threading

    // ============================================
    // Ref - スレッドセーフな共有状態
    // ============================================

    /// Scala の Ref[IO, A] に相当するスレッドセーフな参照
    type Ref<'a> private (initialValue: 'a) =
        let mutable value = initialValue
        let lockObj = obj()

        /// 現在の値を取得
        member _.Get() : 'a =
            lock lockObj (fun () -> value)

        /// 値を設定
        member _.Set(newValue: 'a) : unit =
            lock lockObj (fun () -> value <- newValue)

        /// アトミックに更新
        member _.Update(f: 'a -> 'a) : unit =
            lock lockObj (fun () -> value <- f value)

        /// 更新して古い値を返す
        member _.GetAndUpdate(f: 'a -> 'a) : 'a =
            lock lockObj (fun () ->
                let oldValue = value
                value <- f value
                oldValue)

        /// 更新して新しい値を返す
        member _.UpdateAndGet(f: 'a -> 'a) : 'a =
            lock lockObj (fun () ->
                value <- f value
                value)

        /// 更新して結果を返す
        member _.Modify(f: 'a -> 'a * 'b) : 'b =
            lock lockObj (fun () ->
                let (newValue, result) = f value
                value <- newValue
                result)

        /// 初期値で Ref を作成
        static member Of(initialValue: 'a) : Ref<'a> =
            Ref(initialValue)

    /// Async 版の Ref 操作
    module RefAsync =

        let get (ref: Ref<'a>) : Async<'a> =
            async { return ref.Get() }

        let set (newValue: 'a) (ref: Ref<'a>) : Async<unit> =
            async { return ref.Set(newValue) }

        let update (f: 'a -> 'a) (ref: Ref<'a>) : Async<unit> =
            async { return ref.Update(f) }

        let modify (f: 'a -> 'a * 'b) (ref: Ref<'a>) : Async<'b> =
            async { return ref.Modify(f) }

    // ============================================
    // 並列実行
    // ============================================

    /// Async のリストを順次実行（Scala の sequence 相当）
    let sequence (asyncList: Async<'a> list) : Async<'a list> =
        async {
            let! results = asyncList |> List.map Async.StartAsTask |> Array.ofList |> System.Threading.Tasks.Task.WhenAll |> Async.AwaitTask
            return results |> Array.toList
        }

    /// Async のリストを並列実行（Scala の parSequence 相当）
    let parSequence (asyncList: Async<'a> list) : Async<'a list> =
        async {
            let! results = Async.Parallel asyncList
            return results |> Array.toList
        }

    /// 2つの Async を並列実行
    let parTuple2 (async1: Async<'a>) (async2: Async<'b>) : Async<'a * 'b> =
        async {
            let! results = Async.Parallel [ async { let! a = async1 in return box a }
                                            async { let! b = async2 in return box b } ]
            return (unbox results.[0], unbox results.[1])
        }

    /// 3つの Async を並列実行
    let parTuple3 (async1: Async<'a>) (async2: Async<'b>) (async3: Async<'c>) : Async<'a * 'b * 'c> =
        async {
            let! results = Async.Parallel [ async { let! a = async1 in return box a }
                                            async { let! b = async2 in return box b }
                                            async { let! c = async3 in return box c } ]
            return (unbox results.[0], unbox results.[1], unbox results.[2])
        }

    // ============================================
    // Fiber - 軽量スレッド（キャンセル可能な非同期タスク）
    // ============================================

    /// Scala の Fiber に相当するキャンセル可能な非同期タスク
    type Fiber<'a> = {
        /// タスクの結果を待機
        Join: Async<'a>
        /// タスクをキャンセル
        Cancel: unit -> unit
    }

    /// Async を Fiber として起動（バックグラウンドで実行）
    let start (computation: Async<'a>) : Fiber<'a> =
        let cts = new CancellationTokenSource()
        let tcs = new System.Threading.Tasks.TaskCompletionSource<'a>()

        Async.Start(
            async {
                try
                    let! result = computation
                    tcs.SetResult(result)
                with
                | :? OperationCanceledException ->
                    tcs.SetCanceled()
                | ex ->
                    tcs.SetException(ex)
            },
            cts.Token)

        {
            Join = Async.AwaitTask tcs.Task
            Cancel = fun () -> cts.Cancel()
        }

    /// 永遠に繰り返す（Scala の foreverM 相当）
    let foreverM (action: Async<unit>) : Async<unit> =
        let rec loop () =
            async {
                do! action
                return! loop ()
            }
        loop ()

    /// 指定回数繰り返す
    let repeatN (n: int) (action: Async<'a>) : Async<'a list> =
        async {
            let! results =
                [ for _ in 1..n -> action ]
                |> Async.Sequential
            return results |> Array.toList
        }

    // ============================================
    // チェックイン処理の例
    // ============================================

    type City = { Name: string }
    type CityStats = { City: City; CheckIns: int }

    /// トップ N 都市を計算（純粋関数）
    let topCities (n: int) (cityCheckIns: Map<City, int>) : CityStats list =
        cityCheckIns
        |> Map.toList
        |> List.map (fun (city, checkIns) -> { City = city; CheckIns = checkIns })
        |> List.sortByDescending (fun stats -> stats.CheckIns)
        |> List.truncate n

    /// チェックインを保存
    let storeCheckIn (storedCheckIns: Ref<Map<City, int>>) (city: City) : unit =
        storedCheckIns.Update(fun map ->
            match Map.tryFind city map with
            | Some count -> Map.add city (count + 1) map
            | None -> Map.add city 1 map)

    /// チェックインを保存（Async版）
    let storeCheckInAsync (storedCheckIns: Ref<Map<City, int>>) (city: City) : Async<unit> =
        async { return storeCheckIn storedCheckIns city }

    /// ランキングを更新
    let updateRanking
        (storedCheckIns: Ref<Map<City, int>>)
        (storedRanking: Ref<CityStats list>)
        : Async<unit> =
        async {
            let checkIns = storedCheckIns.Get()
            let ranking = topCities 3 checkIns
            storedRanking.Set(ranking)
        }

    /// ランキングを継続的に更新
    let updateRankingForever
        (storedCheckIns: Ref<Map<City, int>>)
        (storedRanking: Ref<CityStats list>)
        : Async<unit> =
        foreverM (updateRanking storedCheckIns storedRanking)

    // ============================================
    // 処理結果を返すパターン
    // ============================================

    type ProcessingCheckIns = {
        /// 現在のランキングを取得
        CurrentRanking: unit -> CityStats list
        /// 処理を停止
        Stop: unit -> unit
    }

    /// チェックイン処理を開始し、制御オブジェクトを返す
    let startCheckInProcessing
        (checkIns: City seq)
        : ProcessingCheckIns =
        let storedCheckIns = Ref.Of(Map.empty<City, int>)
        let storedRanking = Ref.Of([] : CityStats list)
        let cts = new CancellationTokenSource()

        // チェックイン処理を開始
        Async.Start(
            async {
                for city in checkIns do
                    storeCheckIn storedCheckIns city
            },
            cts.Token)

        // ランキング更新を開始
        Async.Start(
            async {
                while not cts.Token.IsCancellationRequested do
                    do! updateRanking storedCheckIns storedRanking
                    do! Async.Sleep 10
            },
            cts.Token)

        {
            CurrentRanking = fun () -> storedRanking.Get()
            Stop = fun () -> cts.Cancel()
        }


/// MailboxProcessor（Agent）を使った並行処理
module AgentBasedConcurrency =

    open System

    // ============================================
    // カウンターエージェント
    // ============================================

    type CounterMessage =
        | Increment
        | Decrement
        | GetValue of AsyncReplyChannel<int>
        | Reset

    /// スレッドセーフなカウンター
    let createCounter (initialValue: int) : MailboxProcessor<CounterMessage> =
        MailboxProcessor.Start(fun inbox ->
            let rec loop count =
                async {
                    let! msg = inbox.Receive()
                    match msg with
                    | Increment ->
                        return! loop (count + 1)
                    | Decrement ->
                        return! loop (count - 1)
                    | GetValue replyChannel ->
                        replyChannel.Reply(count)
                        return! loop count
                    | Reset ->
                        return! loop initialValue
                }
            loop initialValue)

    /// カウンターの値を取得
    let getCounterValue (counter: MailboxProcessor<CounterMessage>) : int =
        counter.PostAndReply(GetValue)

    /// カウンターをインクリメント
    let incrementCounter (counter: MailboxProcessor<CounterMessage>) : unit =
        counter.Post(Increment)

    /// カウンターをデクリメント
    let decrementCounter (counter: MailboxProcessor<CounterMessage>) : unit =
        counter.Post(Decrement)

    // ============================================
    // 汎用ステートエージェント
    // ============================================

    type StateMessage<'state, 'result> =
        | Get of AsyncReplyChannel<'state>
        | Update of ('state -> 'state)
        | Modify of ('state -> 'state * 'result) * AsyncReplyChannel<'result>

    /// 汎用的な状態管理エージェント（Ref の Agent 版）
    let createStateAgent<'state> (initialState: 'state) : MailboxProcessor<StateMessage<'state, obj>> =
        MailboxProcessor.Start(fun inbox ->
            let rec loop state =
                async {
                    let! msg = inbox.Receive()
                    match msg with
                    | Get replyChannel ->
                        replyChannel.Reply(state)
                        return! loop state
                    | Update f ->
                        return! loop (f state)
                    | Modify (f, replyChannel) ->
                        let (newState, result) = f state
                        replyChannel.Reply(result)
                        return! loop newState
                }
            loop initialState)

    // ============================================
    // チェックインエージェント
    // ============================================

    open ConcurrentProcessing

    type CheckInMessage =
        | AddCheckIn of City
        | GetStats of AsyncReplyChannel<CityStats list>
        | GetTotal of AsyncReplyChannel<int>

    /// チェックイン集計エージェント
    let createCheckInAgent () : MailboxProcessor<CheckInMessage> =
        MailboxProcessor.Start(fun inbox ->
            let rec loop (checkIns: Map<City, int>) =
                async {
                    let! msg = inbox.Receive()
                    match msg with
                    | AddCheckIn city ->
                        let newCheckIns =
                            match Map.tryFind city checkIns with
                            | Some count -> Map.add city (count + 1) checkIns
                            | None -> Map.add city 1 checkIns
                        return! loop newCheckIns
                    | GetStats replyChannel ->
                        let stats = topCities 3 checkIns
                        replyChannel.Reply(stats)
                        return! loop checkIns
                    | GetTotal replyChannel ->
                        let total = checkIns |> Map.toList |> List.sumBy snd
                        replyChannel.Reply(total)
                        return! loop checkIns
                }
            loop Map.empty)

    /// チェックインを追加
    let addCheckIn (agent: MailboxProcessor<CheckInMessage>) (city: City) : unit =
        agent.Post(AddCheckIn city)

    /// 統計を取得
    let getStats (agent: MailboxProcessor<CheckInMessage>) : CityStats list =
        agent.PostAndReply(GetStats)

    /// 合計を取得
    let getTotal (agent: MailboxProcessor<CheckInMessage>) : int =
        agent.PostAndReply(GetTotal)


/// 並列処理のユーティリティ
module ParallelUtils =

    open System
    open ConcurrentProcessing

    // ============================================
    // 並列 map
    // ============================================

    /// リストの各要素に関数を並列適用
    let parMap (f: 'a -> 'b) (list: 'a list) : 'b list =
        list
        |> List.map (fun x -> async { return f x })
        |> parSequence
        |> Async.RunSynchronously

    /// リストの各要素に Async 関数を並列適用
    let parMapAsync (f: 'a -> Async<'b>) (list: 'a list) : Async<'b list> =
        list
        |> List.map f
        |> parSequence

    // ============================================
    // 並列 filter
    // ============================================

    /// 条件を満たす要素を並列でフィルタ
    let parFilter (predicate: 'a -> bool) (list: 'a list) : 'a list =
        list
        |> List.map (fun x -> async { return (x, predicate x) })
        |> parSequence
        |> Async.RunSynchronously
        |> List.filter snd
        |> List.map fst

    // ============================================
    // 並列 fold（reduce）
    // ============================================

    /// 並列で集約（結合が結合的な場合のみ正確）
    let parReduce (combine: 'a -> 'a -> 'a) (list: 'a list) : 'a option =
        match list with
        | [] -> None
        | [x] -> Some x
        | _ ->
            let rec reduceLevel (items: 'a list) =
                match items with
                | [] -> failwith "Unexpected empty list"
                | [x] -> x
                | _ ->
                    items
                    |> List.chunkBySize 2
                    |> List.map (fun chunk ->
                        match chunk with
                        | [a; b] -> async { return combine a b }
                        | [a] -> async { return a }
                        | _ -> failwith "Unexpected chunk size")
                    |> parSequence
                    |> Async.RunSynchronously
                    |> reduceLevel
            Some (reduceLevel list)

    // ============================================
    // タイムアウト付き実行
    // ============================================

    /// タイムアウト付きで Async を実行
    let withTimeout (timeoutMs: int) (computation: Async<'a>) : Async<'a option> =
        async {
            let! child = Async.StartChild(computation, timeoutMs)
            try
                let! result = child
                return Some result
            with :? TimeoutException ->
                return None
        }

    // ============================================
    // サイコロを並行して振る
    // ============================================

    let private random = Random()

    let castTheDie () : Async<int> =
        async { return random.Next(1, 7) }

    /// N 個のサイコロを並行して振る
    let castDiceConcurrently (n: int) : Async<int list> =
        [ for _ in 1..n -> castTheDie () ]
        |> parSequence

    /// N 個のサイコロを並行して振り、合計を返す
    let castDiceAndSum (n: int) : Async<int> =
        async {
            let! results = castDiceConcurrently n
            return List.sum results
        }

    // ============================================
    // 偶数カウント（並行版）
    // ============================================

    /// 並行して実行し、偶数の数をカウント
    let countEvens (asyncInts: Async<int> list) : Async<int> =
        async {
            let counter = Ref.Of(0)
            let tasks =
                asyncInts
                |> List.map (fun asyncInt ->
                    async {
                        let! n = asyncInt
                        if n % 2 = 0 then
                            counter.Update((+) 1)
                    })
            do! parSequence tasks |> Async.Ignore
            return counter.Get()
        }
