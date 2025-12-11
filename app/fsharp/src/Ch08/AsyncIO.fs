namespace Ch08

/// F# での非同期処理と副作用の管理
/// Scala の IO モナドに相当する機能を F# の Async で実現
module AsyncIO =

    open System

    // ============================================
    // 基本的な Async の作成
    // ============================================

    /// 副作用のある計算を Async でラップ（IO.delay 相当）
    let delayEffect (effect: unit -> 'a) : Async<'a> =
        async { return effect () }

    /// 純粋な値を Async でラップ（IO.pure 相当）
    let pureAsync (value: 'a) : Async<'a> =
        async { return value }

    /// 何もしない Async（IO.unit 相当）
    let unitAsync : Async<unit> =
        async { return () }

    // ============================================
    // サイコロを振る例
    // ============================================

    /// 不純な関数（副作用あり）
    let castTheDieImpure () : int =
        let random = Random()
        random.Next(1, 7)

    /// Async を使った純粋な記述
    let castTheDie () : Async<int> =
        async { return castTheDieImpure () }

    /// サイコロを2回振って合計を返す
    let castTheDieTwice () : Async<int> =
        async {
            let! first = castTheDie ()
            let! second = castTheDie ()
            return first + second
        }

    // ============================================
    // ミーティングスケジューリング
    // ============================================

    type MeetingTime = { StartHour: int; EndHour: int }

    /// ミーティングが重なっているか判定
    let meetingsOverlap (meeting1: MeetingTime) (meeting2: MeetingTime) : bool =
        meeting1.StartHour < meeting2.EndHour && meeting2.StartHour < meeting1.EndHour

    /// API呼び出しをシミュレート（副作用）
    let private calendarEntriesApiCall (name: string) : MeetingTime list =
        // 実際の実装ではAPIを呼び出す
        match name with
        | "Alice" -> [ { StartHour = 9; EndHour = 10 }; { StartHour = 14; EndHour = 15 } ]
        | "Bob" -> [ { StartHour = 10; EndHour = 11 }; { StartHour = 15; EndHour = 16 } ]
        | "Charlie" -> [ { StartHour = 11; EndHour = 12 } ]
        | _ -> []

    /// カレンダーエントリを取得（Async版）
    let calendarEntries (name: string) : Async<MeetingTime list> =
        async { return calendarEntriesApiCall name }

    /// 2人の予定を取得
    let scheduledMeetings (person1: string) (person2: string) : Async<MeetingTime list> =
        async {
            let! entries1 = calendarEntries person1
            let! entries2 = calendarEntries person2
            return entries1 @ entries2
        }

    /// 可能なミーティング時間を計算（純粋関数）
    let possibleMeetings
        (existingMeetings: MeetingTime list)
        (startHour: int)
        (endHour: int)
        (lengthHours: int)
        : MeetingTime list =
        [ startHour .. (endHour - lengthHours) ]
        |> List.map (fun start -> { StartHour = start; EndHour = start + lengthHours })
        |> List.filter (fun slot ->
            existingMeetings |> List.forall (fun meeting -> not (meetingsOverlap meeting slot)))

    /// ミーティングをスケジュール
    let schedule (person1: string) (person2: string) (lengthHours: int) : Async<MeetingTime list> =
        async {
            let! meetings = scheduledMeetings person1 person2
            return possibleMeetings meetings 8 16 lengthHours
        }

    // ============================================
    // エラーハンドリングと orElse
    // ============================================

    /// 失敗時のフォールバックを指定（orElse 相当）
    let orElse (fallback: Async<'a>) (primary: Async<'a>) : Async<'a> =
        async {
            try
                return! primary
            with _ ->
                return! fallback
        }

    /// 失敗する可能性のある Async
    let failingAsync (message: string) : Async<int> =
        async {
            failwith message
            return 0
        }

    /// 成功する Async
    let succeedingAsync (value: int) : Async<int> =
        async { return value }

    // ============================================
    // リトライ戦略
    // ============================================

    /// 指定回数リトライする
    let retry (maxRetries: int) (action: Async<'a>) : Async<'a> =
        let rec loop remaining =
            async {
                try
                    return! action
                with ex ->
                    if remaining > 0 then
                        return! loop (remaining - 1)
                    else
                        return raise ex
            }
        loop maxRetries

    /// リトライ後、失敗したらデフォルト値を返す
    let retryWithDefault (maxRetries: int) (defaultValue: 'a) (action: Async<'a>) : Async<'a> =
        async {
            try
                return! retry maxRetries action
            with _ ->
                return defaultValue
        }

    // ============================================
    // sequence: List<Async<'a>> -> Async<List<'a>>
    // ============================================

    /// Async のリストを List の Async に変換
    let sequence (asyncList: Async<'a> list) : Async<'a list> =
        async {
            let! results =
                asyncList
                |> List.map (fun a -> async {
                    let! result = a
                    return result
                })
                |> Async.Sequential
            return results |> Array.toList
        }

    /// 複数人の予定を取得
    let scheduledMeetingsMultiple (attendees: string list) : Async<MeetingTime list> =
        async {
            let! allMeetings =
                attendees
                |> List.map calendarEntries
                |> sequence
            return allMeetings |> List.concat
        }

    // ============================================
    // 並列実行
    // ============================================

    /// 2つの Async を並列実行
    let parallel2 (async1: Async<'a>) (async2: Async<'b>) : Async<'a * 'b> =
        async {
            let! results = Async.Parallel [ async { let! a = async1 in return box a }
                                            async { let! b = async2 in return box b } ]
            return (unbox results.[0], unbox results.[1])
        }

    /// Async のリストを並列実行
    let parallelAll (asyncList: Async<'a> list) : Async<'a list> =
        async {
            let! results = Async.Parallel asyncList
            return results |> Array.toList
        }

    // ============================================
    // 実行ヘルパー
    // ============================================

    /// Async を同期的に実行（unsafeRunSync 相当）
    let runSync (computation: Async<'a>) : 'a =
        Async.RunSynchronously computation

    /// タイムアウト付きで実行
    let runWithTimeout (timeout: int) (computation: Async<'a>) : 'a option =
        try
            Some (Async.RunSynchronously(computation, timeout))
        with :? TimeoutException ->
            None


/// ミーティングスケジューラーの完全な例
module MeetingScheduler =

    open AsyncIO

    /// ミーティング作成APIをシミュレート
    let createMeetingApiCall (names: string list) (meeting: MeetingTime) : unit =
        printfn "Created meeting for %A at %d:00-%d:00" names meeting.StartHour meeting.EndHour

    /// ミーティングを作成（Async版）
    let createMeeting (names: string list) (meeting: MeetingTime) : Async<unit> =
        async { return createMeetingApiCall names meeting }

    /// 完全なスケジューリングワークフロー
    let scheduleAndCreate (person1: string) (person2: string) (lengthHours: int) : Async<MeetingTime option> =
        async {
            let! possibleSlots = schedule person1 person2 lengthHours
            match possibleSlots with
            | slot :: _ ->
                do! createMeeting [ person1; person2 ] slot
                return Some slot
            | [] ->
                return None
        }
