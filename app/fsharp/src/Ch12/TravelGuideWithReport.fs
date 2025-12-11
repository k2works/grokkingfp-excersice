namespace Ch12

/// テスト戦略と改善されたエラーハンドリング
/// SearchReport を導入してテスト可能性を向上
module TravelGuideWithReport =

    open System
    open Ch11.TravelGuide

    // ============================================
    // 検索レポート
    // ============================================

    /// 検索の統計情報とエラー情報
    type SearchReport = {
        AttractionsSearched: int
        ArtistsFound: int
        MoviesFound: int
        Errors: string list
    }

    module SearchReport =
        let empty = {
            AttractionsSearched = 0
            ArtistsFound = 0
            MoviesFound = 0
            Errors = []
        }

        let addError error report =
            { report with Errors = error :: report.Errors }

        let withAttractions count report =
            { report with AttractionsSearched = count }

        let withArtists count report =
            { report with ArtistsFound = count }

        let withMovies count report =
            { report with MoviesFound = count }

    /// SearchReport 付きの旅行ガイド
    type TravelGuideWithReport = {
        Attraction: Attraction
        Subjects: string list
        SearchReport: SearchReport
    }

    // ============================================
    // エラーハンドリング付きデータアクセス
    // ============================================

    /// Either を使用したデータアクセスインターフェース
    type IDataAccessWithErrors =
        abstract member FindAttractions: name: string * ordering: AttractionOrdering * limit: int -> Async<Attraction list>
        abstract member FindArtistsFromLocation: locationId: LocationId * limit: int -> Async<Result<MusicArtist list, string>>
        abstract member FindMoviesAboutLocation: locationId: LocationId * limit: int -> Async<Result<Movie list, string>>

    // ============================================
    // 改善されたアプリケーションロジック
    // ============================================

    /// エラーハンドリング付き旅行ガイド生成
    let travelGuideWithReport (data: IDataAccessWithErrors) (attractionName: string) : Async<TravelGuideWithReport option> =
        async {
            let! attractions = data.FindAttractions(attractionName, ByLocationPopulation, 1)
            match attractions with
            | [] -> return None
            | attraction :: _ ->
                let! artistsResult = data.FindArtistsFromLocation(attraction.Location.Id, 2)
                let! moviesResult = data.FindMoviesAboutLocation(attraction.Location.Id, 2)

                let artistError =
                    match artistsResult with
                    | Error e -> Some e
                    | Ok _ -> None

                let movieError =
                    match moviesResult with
                    | Error e -> Some e
                    | Ok _ -> None

                let errors = [artistError; movieError] |> List.choose id

                let artists = artistsResult |> Result.defaultValue []
                let movies = moviesResult |> Result.defaultValue []

                let subjects =
                    (artists |> List.map (fun a -> a.Name))
                    @ (movies |> List.map (fun m -> m.Name))

                let report = {
                    AttractionsSearched = 1
                    ArtistsFound = List.length artists
                    MoviesFound = List.length movies
                    Errors = errors
                }

                return Some {
                    Attraction = attraction
                    Subjects = subjects
                    SearchReport = report
                }
        }

    /// 複数のアトラクションから旅行ガイドを生成（エラーハンドリング付き）
    let travelGuidesWithReport
        (data: IDataAccessWithErrors)
        (attractionName: string)
        (limit: int)
        : Async<TravelGuideWithReport list> =
        async {
            let! attractions = data.FindAttractions(attractionName, ByLocationPopulation, limit)
            let! guides =
                attractions
                |> List.map (fun attraction ->
                    async {
                        let! artistsResult = data.FindArtistsFromLocation(attraction.Location.Id, 2)
                        let! moviesResult = data.FindMoviesAboutLocation(attraction.Location.Id, 2)

                        let artistError =
                            match artistsResult with
                            | Error e -> Some e
                            | Ok _ -> None

                        let movieError =
                            match moviesResult with
                            | Error e -> Some e
                            | Ok _ -> None

                        let errors = [artistError; movieError] |> List.choose id

                        let artists = artistsResult |> Result.defaultValue []
                        let movies = moviesResult |> Result.defaultValue []

                        let subjects =
                            (artists |> List.map (fun a -> a.Name))
                            @ (movies |> List.map (fun m -> m.Name))

                        let report = {
                            AttractionsSearched = List.length attractions
                            ArtistsFound = List.length artists
                            MoviesFound = List.length movies
                            Errors = errors
                        }

                        return {
                            Attraction = attraction
                            Subjects = subjects
                            SearchReport = report
                        }
                    })
                |> Async.Parallel
            return guides |> Array.toList
        }

    // ============================================
    // テスト用のスタブ実装
    // ============================================

    /// 成功するテスト用データアクセス
    let successfulDataAccess
        (testAttractions: Attraction list)
        (testArtists: MusicArtist list)
        (testMovies: Movie list)
        : IDataAccessWithErrors =
        { new IDataAccessWithErrors with
            member _.FindAttractions(name, _, limit) =
                async {
                    return
                        testAttractions
                        |> List.filter (fun a -> a.Name.Contains(name))
                        |> List.truncate limit
                }

            member _.FindArtistsFromLocation(_, limit) =
                async { return Ok (testArtists |> List.truncate limit) }

            member _.FindMoviesAboutLocation(_, limit) =
                async { return Ok (testMovies |> List.truncate limit) }
        }

    /// 部分的に失敗するテスト用データアクセス
    let partiallyFailingDataAccess
        (testAttractions: Attraction list)
        (artistError: string)
        (movieError: string)
        : IDataAccessWithErrors =
        { new IDataAccessWithErrors with
            member _.FindAttractions(name, _, limit) =
                async {
                    return
                        testAttractions
                        |> List.filter (fun a -> a.Name.Contains(name))
                        |> List.truncate limit
                }

            member _.FindArtistsFromLocation(_, _) =
                async { return Error artistError }

            member _.FindMoviesAboutLocation(_, _) =
                async { return Error movieError }
        }

    // ============================================
    // プロパティベーステスト用のジェネレーター
    // ============================================

    module Generators =
        open System

        let private random = Random()

        /// ランダムな LocationId を生成
        let locationId () =
            LocationId (sprintf "Q%d" (random.Next(1, 10000)))

        /// ランダムな Location を生成
        let location () = {
            Id = locationId ()
            Name = sprintf "City%d" (random.Next(1, 1000))
            Population = random.Next(1000, 10000000)
        }

        /// ランダムな Attraction を生成
        let attraction () = {
            Name = sprintf "Attraction%d" (random.Next(1, 1000))
            Description = if random.Next(2) = 0 then Some "Description" else None
            Location = location ()
        }

        /// ランダムな MusicArtist を生成
        let musicArtist () = {
            Name = sprintf "Artist%d" (random.Next(1, 1000))
            Genre = if random.Next(2) = 0 then Some "Pop" else None
        }

        /// ランダムな Movie を生成
        let movie () = {
            Name = sprintf "Movie%d" (random.Next(1, 1000))
            BoxOffice = if random.Next(2) = 0 then Some (random.Next(1000000, 100000000)) else None
        }

        /// ランダムな Attraction リストを生成
        let attractions (count: int) =
            [ for _ in 1..count -> attraction () ]

        /// ランダムな MusicArtist リストを生成
        let musicArtists (count: int) =
            [ for _ in 1..count -> musicArtist () ]

        /// ランダムな Movie リストを生成
        let movies (count: int) =
            [ for _ in 1..count -> movie () ]

    // ============================================
    // 不変条件の検証
    // ============================================

    module Invariants =
        /// 結果のサイズは limit 以下
        let resultSizeIsLimited (results: 'a list) (limit: int) : bool =
            List.length results <= limit

        /// SearchReport のエラー数は検索数以下
        let errorsAreBounded (report: SearchReport) : bool =
            List.length report.Errors <= 2 // artists + movies の最大

        /// 見つかったアーティスト数はレポートと一致
        let artistCountMatchesReport (guide: TravelGuideWithReport) : bool =
            let artistCount =
                guide.Subjects
                |> List.filter (fun s -> s.StartsWith("Artist"))
                |> List.length
            artistCount = guide.SearchReport.ArtistsFound ||
            guide.SearchReport.Errors |> List.exists (fun e -> e.Contains("artist") || e.Contains("Artist"))

    // ============================================
    // ユーティリティ関数
    // ============================================

    /// 人気のあるロケーションをフィルタ
    let filterPopularLocations (locations: Location list) (minPopulation: int) : Location list =
        locations |> List.filter (fun loc -> loc.Population >= minPopulation)

    /// アトラクションを人口でソート
    let sortAttractionsByPopulation (attractions: Attraction list) : Attraction list =
        attractions |> List.sortByDescending (fun a -> a.Location.Population)

    /// アトラクションを名前でソート
    let sortAttractionsByName (attractions: Attraction list) : Attraction list =
        attractions |> List.sortBy (fun a -> a.Name)

