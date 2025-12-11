namespace Ch07

/// 第7章: Result 型と複合的なエラー処理
/// Result を使ってエラーメッセージを保持する方法を学ぶ
module ResultHandling =

    // ============================================
    // 基本的なデータ型
    // ============================================

    /// TV番組を表すレコード型
    type TvShow = {
        Title: string
        Start: int
        End: int
    }

    // ============================================
    // 文字列パース関数（Result版）
    // ============================================

    /// 文字列を安全に整数に変換（エラーメッセージ付き）
    let tryParseInt (s: string) : Result<int, string> =
        match System.Int32.TryParse(s) with
        | true, value -> Ok value
        | false, _ -> Error $"Can't parse '{s}' as integer"

    // ============================================
    // TV番組パーサー（Result版）
    // ============================================

    /// 名前を抽出
    let extractName (rawShow: string) : Result<string, string> =
        let bracketOpen = rawShow.IndexOf('(')
        if bracketOpen > 0 then
            Ok (rawShow.Substring(0, bracketOpen).Trim())
        else
            Error $"Can't extract name from '{rawShow}'"

    /// 開始年を抽出
    let extractYearStart (rawShow: string) : Result<int, string> =
        let bracketOpen = rawShow.IndexOf('(')
        let dash = rawShow.IndexOf('-')
        if bracketOpen <> -1 && dash > bracketOpen + 1 then
            let yearStr = rawShow.Substring(bracketOpen + 1, dash - bracketOpen - 1)
            tryParseInt yearStr
        else
            Error $"Can't extract start year from '{rawShow}'"

    /// 終了年を抽出
    let extractYearEnd (rawShow: string) : Result<int, string> =
        let dash = rawShow.IndexOf('-')
        let bracketClose = rawShow.IndexOf(')')
        if dash <> -1 && bracketClose > dash + 1 then
            let yearStr = rawShow.Substring(dash + 1, bracketClose - dash - 1)
            tryParseInt yearStr
        else
            Error $"Can't extract end year from '{rawShow}'"

    /// 単年を抽出
    let extractSingleYear (rawShow: string) : Result<int, string> =
        let dash = rawShow.IndexOf('-')
        let bracketOpen = rawShow.IndexOf('(')
        let bracketClose = rawShow.IndexOf(')')
        if dash = -1 && bracketOpen <> -1 && bracketClose > bracketOpen + 1 then
            let yearStr = rawShow.Substring(bracketOpen + 1, bracketClose - bracketOpen - 1)
            tryParseInt yearStr
        else
            Error $"Can't extract single year from '{rawShow}'"

    /// Result の orElse（F# では標準にないので実装）
    let orElse (alternative: Result<'a, 'e>) (result: Result<'a, 'e>) : Result<'a, 'e> =
        match result with
        | Ok _ -> result
        | Error _ -> alternative

    /// TV番組をパース（Result版）
    let parseShow (rawShow: string) : Result<TvShow, string> =
        match extractName rawShow with
        | Error e -> Error e
        | Ok name ->
            let yearStart =
                extractYearStart rawShow
                |> orElse (extractSingleYear rawShow)
            let yearEnd =
                extractYearEnd rawShow
                |> orElse (extractSingleYear rawShow)
            match yearStart, yearEnd with
            | Ok start, Ok endYear ->
                Ok { Title = name; Start = start; End = endYear }
            | Error e, _ -> Error e
            | _, Error e -> Error e

    // ============================================
    // バリデーション
    // ============================================

    /// 年齢のバリデーション
    let validateAge (age: int) : Result<int, string> =
        if age < 0 then Error "Age cannot be negative"
        elif age > 150 then Error "Age cannot be greater than 150"
        else Ok age

    /// 名前のバリデーション
    let validateName (name: string) : Result<string, string> =
        if System.String.IsNullOrWhiteSpace(name) then
            Error "Name cannot be empty"
        elif name.Length > 100 then
            Error "Name cannot be longer than 100 characters"
        else
            Ok name

    /// メールアドレスのバリデーション
    let validateEmail (email: string) : Result<string, string> =
        if System.String.IsNullOrWhiteSpace(email) then
            Error "Email cannot be empty"
        elif not (email.Contains("@")) then
            Error "Email must contain @"
        else
            Ok email


/// 音楽アーティストのモデル
module MusicArtists =

    // ============================================
    // 代数的データ型（ADT）
    // ============================================

    /// 音楽ジャンル（直和型）
    type MusicGenre =
        | HeavyMetal
        | Pop
        | HardRock
        | Jazz
        | Classical

    /// 活動期間（直和型）
    type YearsActive =
        | StillActive of since: int
        | ActiveBetween of start: int * endYear: int

    /// 地域（直和型）
    type Location =
        | US
        | UK
        | England
        | Japan
        | Germany
        | Other of string

    /// アーティスト（直積型）
    type Artist = {
        Name: string
        Genre: MusicGenre
        Origin: Location
        YearsActive: YearsActive
    }

    // ============================================
    // パターンマッチング
    // ============================================

    /// アーティストが指定期間にアクティブだったか判定
    let wasArtistActive (artist: Artist) (yearStart: int) (yearEnd: int) : bool =
        match artist.YearsActive with
        | StillActive since -> since <= yearEnd
        | ActiveBetween (start, endY) -> start <= yearEnd && endY >= yearStart

    /// 活動期間の長さを計算
    let activeLength (artist: Artist) (currentYear: int) : int =
        match artist.YearsActive with
        | StillActive since -> currentYear - since
        | ActiveBetween (start, endY) -> endY - start

    /// 活動状態の説明を取得
    let describeActivity (yearsActive: YearsActive) : string =
        match yearsActive with
        | StillActive since -> $"Active since {since}"
        | ActiveBetween (start, endY) -> $"Active from {start} to {endY}"

    // ============================================
    // 検索条件のモデリング
    // ============================================

    /// 検索条件（直和型）
    type SearchCondition =
        | SearchByGenre of genres: MusicGenre list
        | SearchByOrigin of locations: Location list
        | SearchByActiveYears of start: int * endYear: int

    /// 単一の条件でアーティストをチェック
    let matchesCondition (artist: Artist) (condition: SearchCondition) : bool =
        match condition with
        | SearchByGenre genres -> List.contains artist.Genre genres
        | SearchByOrigin locations -> List.contains artist.Origin locations
        | SearchByActiveYears (start, endY) -> wasArtistActive artist start endY

    /// アーティストを検索
    let searchArtists (artists: Artist list) (conditions: SearchCondition list) : Artist list =
        artists
        |> List.filter (fun artist ->
            conditions |> List.forall (matchesCondition artist))


/// 支払い方法の例（パターンマッチングの練習用）
module PaymentMethods =

    /// 支払い方法（直和型）
    type PaymentMethod =
        | CreditCard of number: string * expiry: string
        | BankTransfer of accountNumber: string
        | Cash

    /// 支払い方法の説明を取得
    let describePayment (method: PaymentMethod) : string =
        match method with
        | CreditCard (number, _) -> $"Credit card ending in {number}"
        | BankTransfer account -> $"Bank transfer to account {account}"
        | Cash -> "Cash payment"

    /// 支払い方法が有効か判定
    let isValidPayment (method: PaymentMethod) : bool =
        match method with
        | CreditCard (number, expiry) ->
            number.Length >= 4 && expiry.Contains("/")
        | BankTransfer account ->
            account.Length > 0
        | Cash -> true
