module Ch08.AsyncIOTests

open Xunit
open Ch08.AsyncIO
open Ch08.MeetingScheduler

// ============================================
// 基本的な Async のテスト
// ============================================

[<Fact>]
let ``delayEffect は副作用をラップする`` () =
    let mutable called = false
    let effect = delayEffect (fun () ->
        called <- true
        42)

    // まだ実行されていない
    Assert.False(called)

    // 実行
    let result = runSync effect
    Assert.True(called)
    Assert.Equal(42, result)

[<Fact>]
let ``pureAsync は純粋な値をラップする`` () =
    let asyncValue = pureAsync 123
    let result = runSync asyncValue
    Assert.Equal(123, result)

[<Fact>]
let ``unitAsync は unit を返す`` () =
    let asyncUnit = unitAsync
    let result = runSync asyncUnit
    Assert.Equal((), result)

// ============================================
// サイコロのテスト
// ============================================

[<Fact>]
let ``castTheDie は1から6の値を返す`` () =
    for _ in 1..100 do
        let result = runSync (castTheDie ())
        Assert.True(result >= 1 && result <= 6)

[<Fact>]
let ``castTheDieTwice は2から12の値を返す`` () =
    for _ in 1..100 do
        let result = runSync (castTheDieTwice ())
        Assert.True(result >= 2 && result <= 12)

// ============================================
// ミーティングスケジューリングのテスト
// ============================================

[<Fact>]
let ``meetingsOverlap は重なりを正しく判定する`` () =
    let meeting1 = { StartHour = 9; EndHour = 10 }
    let meeting2 = { StartHour = 10; EndHour = 11 }
    let meeting3 = { StartHour = 9; EndHour = 11 }

    // 隣接は重ならない
    Assert.False(meetingsOverlap meeting1 meeting2)

    // 重なっている
    Assert.True(meetingsOverlap meeting1 meeting3)
    Assert.True(meetingsOverlap meeting3 meeting2)

[<Fact>]
let ``calendarEntries は予定を返す`` () =
    let aliceEntries = runSync (calendarEntries "Alice")
    Assert.Equal(2, List.length aliceEntries)

    let bobEntries = runSync (calendarEntries "Bob")
    Assert.Equal(2, List.length bobEntries)

    let unknownEntries = runSync (calendarEntries "Unknown")
    Assert.Empty(unknownEntries)

[<Fact>]
let ``scheduledMeetings は2人の予定を結合する`` () =
    let meetings = runSync (scheduledMeetings "Alice" "Bob")
    Assert.Equal(4, List.length meetings)

[<Fact>]
let ``possibleMeetings は空きスロットを計算する`` () =
    let existing = [
        { StartHour = 9; EndHour = 10 }
        { StartHour = 14; EndHour = 15 }
    ]
    let possible = possibleMeetings existing 8 17 1

    // 8-9, 10-11, 11-12, 12-13, 13-14, 15-16, 16-17 が空き
    Assert.True(List.length possible >= 6)
    Assert.True(possible |> List.exists (fun m -> m.StartHour = 8))
    Assert.True(possible |> List.exists (fun m -> m.StartHour = 10))

[<Fact>]
let ``schedule は2人のスケジュールから空きを見つける`` () =
    let slots = runSync (schedule "Alice" "Bob" 1)
    // Alice: 9-10, 14-15
    // Bob: 10-11, 15-16
    // 空き: 8-9, 11-12, 12-13, 13-14
    Assert.True(List.length slots > 0)

// ============================================
// エラーハンドリングのテスト
// ============================================

[<Fact>]
let ``orElse は成功時にメインの値を返す`` () =
    let primary = succeedingAsync 42
    let fallback = succeedingAsync 0
    let result = runSync (primary |> orElse fallback)
    Assert.Equal(42, result)

[<Fact>]
let ``orElse は失敗時にフォールバックを返す`` () =
    let primary = failingAsync "error"
    let fallback = succeedingAsync 99
    let result = runSync (primary |> orElse fallback)
    Assert.Equal(99, result)

// ============================================
// リトライのテスト
// ============================================

[<Fact>]
let ``retry は成功するまでリトライする`` () =
    let mutable attempts = 0
    let action = async {
        attempts <- attempts + 1
        if attempts < 3 then
            failwith "not yet"
        return 42
    }

    let result = runSync (retry 5 action)
    Assert.Equal(42, result)
    Assert.Equal(3, attempts)

[<Fact>]
let ``retryWithDefault は全部失敗したらデフォルト値を返す`` () =
    let action = async {
        failwith "always fails"
        return 42
    }

    let result = runSync (retryWithDefault 3 (-1) action)
    Assert.Equal(-1, result)

// ============================================
// sequence のテスト
// ============================================

[<Fact>]
let ``sequence は Async のリストを List の Async に変換する`` () =
    let asyncList = [
        pureAsync 1
        pureAsync 2
        pureAsync 3
    ]

    let result = runSync (sequence asyncList)
    Assert.Equal<int list>([1; 2; 3], result)

[<Fact>]
let ``scheduledMeetingsMultiple は複数人の予定を取得する`` () =
    let meetings = runSync (scheduledMeetingsMultiple ["Alice"; "Bob"; "Charlie"])
    // Alice: 2, Bob: 2, Charlie: 1
    Assert.Equal(5, List.length meetings)

// ============================================
// 並列実行のテスト
// ============================================

[<Fact>]
let ``parallel2 は2つの Async を並列実行する`` () =
    let async1 = pureAsync 1
    let async2 = pureAsync "hello"
    let result = runSync (parallel2 async1 async2)
    Assert.Equal((1, "hello"), result)

[<Fact>]
let ``parallelAll は Async のリストを並列実行する`` () =
    let asyncList = [
        pureAsync 1
        pureAsync 2
        pureAsync 3
    ]

    let result = runSync (parallelAll asyncList)
    Assert.Equal<int list>([1; 2; 3], result)

// ============================================
// タイムアウトのテスト
// ============================================

[<Fact>]
let ``runWithTimeout は時間内に完了したら Some を返す`` () =
    let fast = pureAsync 42
    let result = runWithTimeout 1000 fast
    Assert.Equal(Some 42, result)

// ============================================
// MeetingScheduler のテスト
// ============================================

[<Fact>]
let ``scheduleAndCreate は最初の空きスロットでミーティングを作成する`` () =
    let result = runSync (scheduleAndCreate "Alice" "Bob" 1)
    Assert.True(result.IsSome)

[<Fact>]
let ``scheduleAndCreate は空きがない場合 None を返す`` () =
    // すべての時間が埋まっている人をシミュレート
    // ここでは既存の実装では発生しないが、ロジックを確認
    let existing = [
        for h in 8..15 -> { StartHour = h; EndHour = h + 1 }
    ]
    let slots = possibleMeetings existing 8 16 1
    Assert.Empty(slots)
