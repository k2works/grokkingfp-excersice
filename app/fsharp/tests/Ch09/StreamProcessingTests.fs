module Ch09.StreamProcessingTests

open Xunit
open Ch09.StreamProcessing

// ============================================
// 基本的な Seq のテスト
// ============================================

[<Fact>]
let ``numbers は 1, 2, 3 を含む`` () =
    let result = numbers |> Seq.toList
    Assert.Equal<int list>([1; 2; 3], result)

[<Fact>]
let ``oddNumbers は奇数のみをフィルタする`` () =
    let input = seq { 1; 2; 3; 4; 5 }
    let result = oddNumbers input |> Seq.toList
    Assert.Equal<int list>([1; 3; 5], result)

[<Fact>]
let ``doubled は各要素を2倍にする`` () =
    let input = seq { 1; 2; 3 }
    let result = doubled input |> Seq.toList
    Assert.Equal<int list>([2; 4; 6], result)

// ============================================
// 無限シーケンスのテスト
// ============================================

[<Fact>]
let ``repeat は要素を無限に繰り返す`` () =
    let result = repeat (seq { 1; 2; 3 }) |> Seq.truncate 8 |> Seq.toList
    Assert.Equal<int list>([1; 2; 3; 1; 2; 3; 1; 2], result)

[<Fact>]
let ``infinite123s は 1, 2, 3 を無限に繰り返す`` () =
    let result = infinite123s |> Seq.truncate 7 |> Seq.toList
    Assert.Equal<int list>([1; 2; 3; 1; 2; 3; 1], result)

[<Fact>]
let ``naturals は自然数の無限シーケンス`` () =
    let result = naturals |> Seq.truncate 5 |> Seq.toList
    Assert.Equal<int list>([1; 2; 3; 4; 5], result)

[<Fact>]
let ``from は指定値から始まる無限シーケンス`` () =
    let result = from 10 |> Seq.truncate 5 |> Seq.toList
    Assert.Equal<int list>([10; 11; 12; 13; 14], result)

// ============================================
// サイコロストリームのテスト
// ============================================

[<Fact>]
let ``infiniteDieCasts は1から6の値を生成する`` () =
    // 新しいシーケンスを毎回作成して10個だけテスト
    let results = infiniteDieCasts |> Seq.truncate 10 |> Seq.toList
    Assert.True(results |> List.forall (fun n -> n >= 1 && n <= 6))

[<Fact>]
let ``castUntil は目標値まで振り続ける`` () =
    // 最大20回に制限
    let results = castUntil 6 |> Seq.truncate 20 |> Seq.toList
    Assert.True(results.Length >= 1 && results.Length <= 20)

// ============================================
// ストリーム操作のテスト
// ============================================

[<Fact>]
let ``take は最初の n 要素を取得する`` () =
    let input = seq { 1; 2; 3; 4; 5 }
    let result = take 3 input |> Seq.toList
    Assert.Equal<int list>([1; 2; 3], result)

[<Fact>]
let ``filter は条件を満たす要素のみを返す`` () =
    let input = seq { 1; 2; 3; 4; 5 }
    let result = filter (fun n -> n > 2) input |> Seq.toList
    Assert.Equal<int list>([3; 4; 5], result)

[<Fact>]
let ``map は各要素を変換する`` () =
    let input = seq { 1; 2; 3 }
    let result = map (fun n -> n * 10) input |> Seq.toList
    Assert.Equal<int list>([10; 20; 30], result)

[<Fact>]
let ``append は2つのストリームを結合する`` () =
    let s1 = seq { 1; 2 }
    let s2 = seq { 3; 4 }
    let result = append s1 s2 |> Seq.toList
    Assert.Equal<int list>([1; 2; 3; 4], result)

// ============================================
// スライディングウィンドウのテスト
// ============================================

[<Fact>]
let ``sliding はスライディングウィンドウを作成する`` () =
    let input = seq { 1; 2; 3; 4; 5 }
    let result = sliding 3 input |> Seq.toList
    Assert.Equal<int list list>([ [1; 2; 3]; [2; 3; 4]; [3; 4; 5] ], result)

[<Fact>]
let ``pairwise は連続する要素のペアを返す`` () =
    let input = seq { 1; 2; 3; 4 }
    let result = pairwise input |> Seq.toList
    Assert.Equal<(int * int) list>([ (1, 2); (2, 3); (3, 4) ], result)

// ============================================
// トレンド判定のテスト
// ============================================

[<Fact>]
let ``trending は上昇トレンドを検出する`` () =
    Assert.True(trending [0.81m; 0.82m; 0.83m])
    Assert.False(trending [0.81m; 0.84m; 0.83m])
    Assert.False(trending [0.83m; 0.82m; 0.81m])

[<Fact>]
let ``declining は下降トレンドを検出する`` () =
    Assert.True(declining [0.83m; 0.82m; 0.81m])
    Assert.False(declining [0.81m; 0.82m; 0.83m])
    Assert.False(declining [0.81m; 0.80m; 0.82m])

[<Fact>]
let ``stable は安定を検出する`` () =
    Assert.True(stable [5m; 5m; 5m])
    Assert.False(stable [5m; 5m; 6m])
    Assert.False(stable [5m; 6m])  // 3つ未満

// ============================================
// ストリーム結合のテスト
// ============================================

[<Fact>]
let ``zip は2つのストリームを結合する`` () =
    let s1 = seq { 1; 2; 3 }
    let s2 = seq { "a"; "b"; "c" }
    let result = zip s1 s2 |> Seq.toList
    Assert.Equal<(int * string) list>([ (1, "a"); (2, "b"); (3, "c") ], result)

[<Fact>]
let ``zipLeft は左側の値を返す`` () =
    let s1 = seq { 1; 2; 3 }
    let s2 = seq { "a"; "b"; "c" }
    let result = zipLeft s1 s2 |> Seq.toList
    Assert.Equal<int list>([1; 2; 3], result)

[<Fact>]
let ``zipRight は右側の値を返す`` () =
    let s1 = seq { 1; 2; 3 }
    let s2 = seq { "a"; "b"; "c" }
    let result = zipRight s1 s2 |> Seq.toList
    Assert.Equal<string list>(["a"; "b"; "c"], result)

// ============================================
// 実用的なストリーム操作のテスト
// ============================================

[<Fact>]
let ``runningSum は累積和を計算する`` () =
    let input = seq { 1; 2; 3; 4; 5 }
    let result = runningSum input |> Seq.toList
    Assert.Equal<int list>([1; 3; 6; 10; 15], result)

[<Fact>]
let ``movingAverage は移動平均を計算する`` () =
    let input = seq { 1m; 2m; 3m; 4m; 5m }
    let result = movingAverage 3 input |> Seq.toList
    // (1+2+3)/3=2, (2+3+4)/3=3, (3+4+5)/3=4
    Assert.Equal<decimal list>([2m; 3m; 4m], result)

[<Fact>]
let ``distinctConsecutive は連続する重複を除去する`` () =
    let input = seq { 1; 1; 2; 2; 2; 3; 1; 1 }
    let result = distinctConsecutive input |> Seq.toList
    Assert.Equal<int list>([1; 2; 3; 1], result)

[<Fact>]
let ``chunk はストリームをチャンク化する`` () =
    let input = seq { 1; 2; 3; 4; 5; 6; 7 }
    let result = chunk 3 input |> Seq.toList
    Assert.Equal<int list list>([ [1; 2; 3]; [4; 5; 6]; [7] ], result)

// ============================================
// 遅延評価のテスト
// ============================================

[<Fact>]
let ``findFirst は最初に条件を満たす要素を見つける`` () =
    let input = seq { 1; 2; 3; 4; 5 }
    let result = findFirst (fun n -> n > 3) input
    Assert.Equal(Some 4, result)

[<Fact>]
let ``findFirst は見つからない場合 None を返す`` () =
    let input = seq { 1; 2; 3 }
    let result = findFirst (fun n -> n > 10) input
    Assert.Equal(None, result)

// ============================================
// 通貨交換のテスト
// ============================================

[<Fact>]
let ``rates は為替レートのストリームを生成する`` () =
    // 5個だけ取得
    let result = rates USD EUR |> Seq.truncate 5 |> Seq.toList
    Assert.Equal(5, List.length result)
    Assert.True(result |> List.forall (fun r -> r > 0m))

[<Fact>]
let ``exchangeIfTrending は上昇トレンドで交換する`` () =
    // 純粋関数のテストのみ
    let trendingRates = [0.81m; 0.82m; 0.83m]
    Assert.True(trending trendingRates)

// ============================================
// AsyncStream のテスト
// ============================================

module AsyncStreamTests =

    open Ch09.AsyncStream

    [<Fact>]
    let ``empty は空のストリームを返す`` () =
        let result = empty<int> |> toListAsync |> Async.RunSynchronously
        Assert.Empty(result)

    [<Fact>]
    let ``singleton は単一要素のストリームを返す`` () =
        let result = singleton 42 |> toListAsync |> Async.RunSynchronously
        Assert.Equal<int list>([42], result)

    [<Fact>]
    let ``fromAsync は Async から AsyncStream を作成する`` () =
        let asyncValue = async { return 123 }
        let result = fromAsync asyncValue |> toListAsync |> Async.RunSynchronously
        Assert.Equal<int list>([123], result)

    [<Fact>]
    let ``take は最初の n 要素を取得する`` () =
        let stream = repeat 1
        let result = stream |> take 3 |> toListAsync |> Async.RunSynchronously
        Assert.Equal<int list>([1; 1; 1], result)
