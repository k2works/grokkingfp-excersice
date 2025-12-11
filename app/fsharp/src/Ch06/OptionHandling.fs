namespace Ch06

/// 第6章: Option 型による安全なエラーハンドリング
/// Option を使って null や例外を避ける方法を学ぶ
module OptionHandling =

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
    // 安全な除算
    // ============================================

    /// ゼロ除算を防ぐ安全な除算
    let safeDivide (a: int) (b: int) : int option =
        if b = 0 then None
        else Some (a / b)

    // ============================================
    // 文字列パース関数
    // ============================================

    /// 文字列を安全に整数に変換
    let tryParseInt (s: string) : int option =
        match System.Int32.TryParse(s) with
        | true, value -> Some value
        | false, _ -> None

    // ============================================
    // TV番組パーサー
    // ============================================

    /// 名前を抽出（例: "Breaking Bad (2008-2013)" → "Breaking Bad"）
    let extractName (rawShow: string) : string option =
        let bracketOpen = rawShow.IndexOf('(')
        if bracketOpen > 0 then
            Some (rawShow.Substring(0, bracketOpen).Trim())
        else
            None

    /// 開始年を抽出（例: "Breaking Bad (2008-2013)" → 2008）
    let extractYearStart (rawShow: string) : int option =
        let bracketOpen = rawShow.IndexOf('(')
        let dash = rawShow.IndexOf('-')
        if bracketOpen <> -1 && dash > bracketOpen + 1 then
            let yearStr = rawShow.Substring(bracketOpen + 1, dash - bracketOpen - 1)
            tryParseInt yearStr
        else
            None

    /// 終了年を抽出（例: "Breaking Bad (2008-2013)" → 2013）
    let extractYearEnd (rawShow: string) : int option =
        let dash = rawShow.IndexOf('-')
        let bracketClose = rawShow.IndexOf(')')
        if dash <> -1 && bracketClose > dash + 1 then
            let yearStr = rawShow.Substring(dash + 1, bracketClose - dash - 1)
            tryParseInt yearStr
        else
            None

    /// 単年を抽出（例: "Chernobyl (2019)" → 2019）
    let extractSingleYear (rawShow: string) : int option =
        let dash = rawShow.IndexOf('-')
        let bracketOpen = rawShow.IndexOf('(')
        let bracketClose = rawShow.IndexOf(')')
        if dash = -1 && bracketOpen <> -1 && bracketClose > bracketOpen + 1 then
            let yearStr = rawShow.Substring(bracketOpen + 1, bracketClose - bracketOpen - 1)
            tryParseInt yearStr
        else
            None

    /// TV番組をパース（Option版）
    let parseShow (rawShow: string) : TvShow option =
        match extractName rawShow with
        | None -> None
        | Some name ->
            let yearStart =
                extractYearStart rawShow
                |> Option.orElse (extractSingleYear rawShow)
            let yearEnd =
                extractYearEnd rawShow
                |> Option.orElse (extractSingleYear rawShow)
            match yearStart, yearEnd with
            | Some start, Some endYear ->
                Some { Title = name; Start = start; End = endYear }
            | _ -> None

    // ============================================
    // 複数の番組をパースする戦略
    // ============================================

    /// Best-effort 戦略: パースできたものだけ返す
    let parseShowsBestEffort (rawShows: string list) : TvShow list =
        rawShows
        |> List.choose parseShow

    /// All-or-nothing 戦略: 全部成功するか、全部失敗
    let parseShowsAllOrNothing (rawShows: string list) : TvShow list option =
        let parsed = rawShows |> List.map parseShow
        if List.forall Option.isSome parsed then
            Some (parsed |> List.choose id)
        else
            None

    // ============================================
    // Option のユーティリティ
    // ============================================

    /// 2つの数値文字列を加算
    let addStrings (a: string) (b: string) : int option =
        match tryParseInt a, tryParseInt b with
        | Some x, Some y -> Some (x + y)
        | _ -> None

    /// Option の orElse（F# では Option.orElse）
    let orElseExample () =
        let seven = Some 7
        let eight = Some 8
        let none: int option = None

        // orElse の動作
        let r1 = seven |> Option.orElse eight   // Some 7
        let r2 = none |> Option.orElse eight    // Some 8
        let r3 = seven |> Option.orElse none    // Some 7
        let r4 = none |> Option.orElse none     // None
        (r1, r2, r3, r4)

    // ============================================
    // forall と exists
    // ============================================

    /// Option.forall の例
    let forallExample (opt: int option) (threshold: int) : bool =
        opt |> Option.forall (fun x -> x < threshold)

    /// Option.exists の例
    let existsExample (opt: int option) (threshold: int) : bool =
        opt |> Option.exists (fun x -> x < threshold)


/// ユーザーモデル（forall/exists の例題用）
module UserModel =

    type User = {
        Name: string
        Email: string option
        Age: int
    }

    /// メールアドレスが設定されていないか、指定ドメインのユーザーをフィルタ
    let filterByEmailDomain (domain: string) (users: User list) : User list =
        users
        |> List.filter (fun user ->
            user.Email |> Option.forall (fun email -> email.EndsWith(domain)))

    /// メールアドレスが設定されていて、指定ドメインのユーザーをフィルタ
    let filterByEmailDomainExists (domain: string) (users: User list) : User list =
        users
        |> List.filter (fun user ->
            user.Email |> Option.exists (fun email -> email.EndsWith(domain)))
