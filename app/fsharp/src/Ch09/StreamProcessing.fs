namespace Ch09

/// F# でのストリーム処理
/// Scala の fs2 Stream に相当する機能を F# の Seq と遅延評価で実現
module StreamProcessing =

    open System

    // ============================================
    // 基本的な Seq（純粋なストリーム）
    // ============================================

    /// 有限シーケンス
    let numbers : int seq = seq { 1; 2; 3 }

    /// フィルタリング
    let oddNumbers (nums: int seq) : int seq =
        nums |> Seq.filter (fun n -> n % 2 <> 0)

    /// マッピング
    let doubled (nums: int seq) : int seq =
        nums |> Seq.map (fun n -> n * 2)

    // ============================================
    // 無限シーケンス
    // ============================================

    /// 無限に繰り返すシーケンス
    let repeat (elements: 'a seq) : 'a seq =
        seq {
            while true do
                yield! elements
        }

    /// 無限に 1, 2, 3 を繰り返す
    let infinite123s : int seq = repeat (seq { 1; 2; 3 })

    /// 自然数の無限シーケンス
    let naturals : int seq =
        Seq.initInfinite (fun i -> i + 1)

    /// 指定値から始まる無限シーケンス
    let from (start: int) : int seq =
        Seq.initInfinite (fun i -> start + i)

    // ============================================
    // サイコロを振るストリーム
    // ============================================

    /// 乱数生成器
    let private random = Random()

    /// サイコロを振る（副作用あり）
    let private castTheDieImpure () : int =
        random.Next(1, 7)

    /// サイコロを無限に振るストリーム
    let infiniteDieCasts : int seq =
        seq {
            while true do
                yield castTheDieImpure ()
        }

    /// 指定した値が出るまで振り続ける（target を含む）
    let castUntil (target: int) : int seq =
        seq {
            let mutable found = false
            while not found do
                let n = castTheDieImpure ()
                yield n
                if n = target then
                    found <- true
        }

    // ============================================
    // ストリームの主要操作
    // ============================================

    /// 最初の n 要素を取得
    let take (n: int) (stream: 'a seq) : 'a seq =
        stream |> Seq.take n

    /// 条件を満たす要素のみ
    let filter (predicate: 'a -> bool) (stream: 'a seq) : 'a seq =
        stream |> Seq.filter predicate

    /// 各要素を変換
    let map (f: 'a -> 'b) (stream: 'a seq) : 'b seq =
        stream |> Seq.map f

    /// 2つのストリームを結合
    let append (stream1: 'a seq) (stream2: 'a seq) : 'a seq =
        Seq.append stream1 stream2

    // ============================================
    // スライディングウィンドウ
    // ============================================

    /// スライディングウィンドウを作成
    let sliding (windowSize: int) (stream: 'a seq) : 'a list seq =
        stream
        |> Seq.windowed windowSize
        |> Seq.map Array.toList

    /// 連続する要素のペア
    let pairwise (stream: 'a seq) : ('a * 'a) seq =
        stream |> Seq.pairwise

    // ============================================
    // 通貨交換レートの例
    // ============================================

    type Currency = USD | EUR | GBP | JPY

    /// 交換レートのシミュレーション
    let private getExchangeRate (from: Currency) (toC: Currency) : decimal =
        match from, toC with
        | USD, EUR -> 0.85m + decimal (random.NextDouble() * 0.02 - 0.01)
        | USD, GBP -> 0.73m + decimal (random.NextDouble() * 0.02 - 0.01)
        | USD, JPY -> 110.0m + decimal (random.NextDouble() * 2.0 - 1.0)
        | EUR, USD -> 1.18m + decimal (random.NextDouble() * 0.02 - 0.01)
        | _ -> 1.0m

    /// トレンド判定（上昇傾向）
    let trending (rates: decimal list) : bool =
        rates.Length > 1 &&
        rates
        |> List.pairwise
        |> List.forall (fun (prev, curr) -> curr > prev)

    /// 下降トレンド判定
    let declining (rates: decimal list) : bool =
        rates.Length > 1 &&
        rates
        |> List.pairwise
        |> List.forall (fun (prev, curr) -> curr < prev)

    /// 安定（変動なし）判定
    let stable (rates: decimal list) : bool =
        rates.Length >= 3 &&
        rates |> List.distinct |> List.length = 1

    /// レートのストリームを生成
    let rates (from: Currency) (toC: Currency) : decimal seq =
        seq {
            while true do
                yield getExchangeRate from toC
        }

    /// トレンドを検出して交換
    let exchangeIfTrending
        (amount: decimal)
        (from: Currency)
        (toC: Currency)
        (windowSize: int)
        : decimal option =
        rates from toC
        |> sliding windowSize
        |> Seq.tryFind trending
        |> Option.map (fun rateWindow ->
            let lastRate = rateWindow |> List.last
            amount * lastRate)

    // ============================================
    // ストリームの結合
    // ============================================

    /// 2つのストリームを zip
    let zip (stream1: 'a seq) (stream2: 'b seq) : ('a * 'b) seq =
        Seq.zip stream1 stream2

    /// 左側の値を返しつつ、両方のストリームを進める
    let zipLeft (stream1: 'a seq) (stream2: 'b seq) : 'a seq =
        Seq.zip stream1 stream2
        |> Seq.map fst

    /// 右側の値を返しつつ、両方のストリームを進める
    let zipRight (stream1: 'a seq) (stream2: 'b seq) : 'b seq =
        Seq.zip stream1 stream2
        |> Seq.map snd

    // ============================================
    // 実用的なストリーム操作
    // ============================================

    /// 累積和
    let runningSum (stream: int seq) : int seq =
        stream |> Seq.scan (+) 0 |> Seq.skip 1

    /// 移動平均
    let movingAverage (windowSize: int) (stream: decimal seq) : decimal seq =
        stream
        |> sliding windowSize
        |> Seq.map (fun window ->
            window |> List.sum |> fun s -> s / decimal windowSize)

    /// 重複を除去（連続する重複のみ）
    let distinctConsecutive (stream: 'a seq) : 'a seq =
        seq {
            let mutable prev = None
            for item in stream do
                match prev with
                | Some p when p = item -> ()
                | _ ->
                    yield item
                    prev <- Some item
        }

    /// チャンク化
    let chunk (size: int) (stream: 'a seq) : 'a list seq =
        stream
        |> Seq.chunkBySize size
        |> Seq.map Array.toList

    // ============================================
    // 遅延評価の実証
    // ============================================

    /// 副作用付きの数値生成（遅延評価を確認）
    let numbersWithSideEffect () : int seq =
        seq {
            for i in 1 .. 10 do
                printfn "Generating %d" i
                yield i
        }

    /// 無限ストリームから条件を満たす最初の要素を見つける
    let findFirst (predicate: 'a -> bool) (stream: 'a seq) : 'a option =
        stream |> Seq.tryFind predicate


/// 非同期ストリーム（AsyncSeq相当の簡易実装）
module AsyncStream =

    open System

    /// 非同期で要素を生成するストリーム
    type AsyncStream<'a> = Async<AsyncStreamNode<'a>>
    and AsyncStreamNode<'a> =
        | Nil
        | Cons of 'a * AsyncStream<'a>

    /// 空のストリーム
    let empty<'a> : AsyncStream<'a> =
        async { return Nil }

    /// 単一要素のストリーム
    let singleton (value: 'a) : AsyncStream<'a> =
        async { return Cons(value, empty) }

    /// Async から AsyncStream を作成
    let fromAsync (computation: Async<'a>) : AsyncStream<'a> =
        async {
            let! value = computation
            return Cons(value, empty)
        }

    /// AsyncStream を List に変換
    let toListAsync (stream: AsyncStream<'a>) : Async<'a list> =
        let rec loop acc s =
            async {
                let! node = s
                match node with
                | Nil -> return List.rev acc
                | Cons(head, tail) -> return! loop (head :: acc) tail
            }
        loop [] stream

    /// 最初の n 要素を取得
    let rec take (n: int) (stream: AsyncStream<'a>) : AsyncStream<'a> =
        async {
            if n <= 0 then
                return Nil
            else
                let! node = stream
                match node with
                | Nil -> return Nil
                | Cons(head, tail) -> return Cons(head, take (n - 1) tail)
        }

    /// 無限に繰り返す
    let rec repeat (value: 'a) : AsyncStream<'a> =
        async { return Cons(value, repeat value) }

    /// 遅延付きで要素を生成
    let rec repeatWithDelay (delayMs: int) (value: 'a) : AsyncStream<'a> =
        async {
            do! Async.Sleep delayMs
            return Cons(value, repeatWithDelay delayMs value)
        }
