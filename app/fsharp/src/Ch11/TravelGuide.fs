namespace Ch11

/// 実践的なアプリケーション構築
/// Scala の TravelGuide に相当する機能を F# で実現
module TravelGuide =

    open System

    // ============================================
    // ドメインモデル
    // ============================================

    /// ロケーション ID（値オブジェクト）
    type LocationId = LocationId of string

    module LocationId =
        let value (LocationId id) = id
        let create id = LocationId id

    /// ロケーション
    type Location = {
        Id: LocationId
        Name: string
        Population: int
    }

    /// アトラクション（観光地）
    type Attraction = {
        Name: string
        Description: string option
        Location: Location
    }

    /// ミュージックアーティスト
    type MusicArtist = {
        Name: string
        Genre: string option
    }

    /// 映画
    type Movie = {
        Name: string
        BoxOffice: int option
    }

    /// 旅行ガイド
    type TravelGuide = {
        Attraction: Attraction
        Subjects: string list
    }

    /// アトラクションのソート順
    type AttractionOrdering =
        | ByName
        | ByLocationPopulation

    // ============================================
    // データアクセス層の抽象化
    // ============================================

    /// データアクセスインターフェース
    type IDataAccess =
        abstract member FindAttractions: name: string * ordering: AttractionOrdering * limit: int -> Async<Attraction list>
        abstract member FindArtistsFromLocation: locationId: LocationId * limit: int -> Async<MusicArtist list>
        abstract member FindMoviesAboutLocation: locationId: LocationId * limit: int -> Async<Movie list>

    // ============================================
    // リソース管理（IDisposable パターン）
    // ============================================

    /// リソースを安全に管理するための型
    type Resource<'a> = {
        Acquire: unit -> Async<'a>
        Release: 'a -> Async<unit>
    }

    module Resource =
        /// リソースを作成
        let make (acquire: unit -> Async<'a>) (release: 'a -> Async<unit>) : Resource<'a> =
            { Acquire = acquire; Release = release }

        /// リソースを使用して操作を実行
        let useAsync (f: 'a -> Async<'b>) (resource: Resource<'a>) : Async<'b> =
            async {
                let! r = resource.Acquire()
                try
                    return! f r
                finally
                    resource.Release r |> Async.RunSynchronously
            }

        /// 2つのリソースを結合
        let bind (f: 'a -> Resource<'b>) (resource: Resource<'a>) : Resource<'b> =
            {
                Acquire = fun () ->
                    async {
                        let! a = resource.Acquire()
                        let resourceB = f a
                        return! resourceB.Acquire()
                    }
                Release = fun b ->
                    async {
                        // Note: In a real implementation, we'd need to track both resources
                        return ()
                    }
            }

    // ============================================
    // キャッシュの実装
    // ============================================

    open Ch10.ConcurrentProcessing

    /// キャッシュ付きデータアクセス
    let cachedDataAccess (dataAccess: IDataAccess) : Async<IDataAccess> =
        async {
            let attractionCache = Ref.Of(Map.empty<string, Attraction list>)
            let artistCache = Ref.Of(Map.empty<string, MusicArtist list>)
            let movieCache = Ref.Of(Map.empty<string, Movie list>)

            return
                { new IDataAccess with
                    member _.FindAttractions(name, ordering, limit) =
                        async {
                            let key = sprintf "%s-%A-%d" name ordering limit
                            let cached = attractionCache.Get() |> Map.tryFind key
                            match cached with
                            | Some attractions -> return attractions
                            | None ->
                                let! attractions = dataAccess.FindAttractions(name, ordering, limit)
                                attractionCache.Update(Map.add key attractions)
                                return attractions
                        }

                    member _.FindArtistsFromLocation(locationId, limit) =
                        async {
                            let key = sprintf "%s-%d" (LocationId.value locationId) limit
                            let cached = artistCache.Get() |> Map.tryFind key
                            match cached with
                            | Some artists -> return artists
                            | None ->
                                let! artists = dataAccess.FindArtistsFromLocation(locationId, limit)
                                artistCache.Update(Map.add key artists)
                                return artists
                        }

                    member _.FindMoviesAboutLocation(locationId, limit) =
                        async {
                            let key = sprintf "%s-%d" (LocationId.value locationId) limit
                            let cached = movieCache.Get() |> Map.tryFind key
                            match cached with
                            | Some movies -> return movies
                            | None ->
                                let! movies = dataAccess.FindMoviesAboutLocation(locationId, limit)
                                movieCache.Update(Map.add key movies)
                                return movies
                        }
                }
        }

    // ============================================
    // アプリケーションロジック
    // ============================================

    /// 旅行ガイドを生成
    let travelGuide (data: IDataAccess) (attractionName: string) : Async<TravelGuide option> =
        async {
            let! attractions = data.FindAttractions(attractionName, ByLocationPopulation, 1)
            match attractions with
            | [] -> return None
            | attraction :: _ ->
                let! artists = data.FindArtistsFromLocation(attraction.Location.Id, 2)
                let! movies = data.FindMoviesAboutLocation(attraction.Location.Id, 2)
                let subjects =
                    (artists |> List.map (fun a -> a.Name))
                    @ (movies |> List.map (fun m -> m.Name))
                return Some {
                    Attraction = attraction
                    Subjects = subjects
                }
        }

    /// 複数のアトラクションから旅行ガイドを生成
    let travelGuides (data: IDataAccess) (attractionName: string) (limit: int) : Async<TravelGuide list> =
        async {
            let! attractions = data.FindAttractions(attractionName, ByLocationPopulation, limit)
            let! guides =
                attractions
                |> List.map (fun attraction ->
                    async {
                        let! artists = data.FindArtistsFromLocation(attraction.Location.Id, 2)
                        let! movies = data.FindMoviesAboutLocation(attraction.Location.Id, 2)
                        let subjects =
                            (artists |> List.map (fun a -> a.Name))
                            @ (movies |> List.map (fun m -> m.Name))
                        return {
                            Attraction = attraction
                            Subjects = subjects
                        }
                    })
                |> Async.Parallel
            return guides |> Array.toList
        }

    // ============================================
    // テスト用のスタブ実装
    // ============================================

    /// テスト用のデータアクセス実装
    let testDataAccess (testAttractions: Attraction list) (testArtists: MusicArtist list) (testMovies: Movie list) : IDataAccess =
        { new IDataAccess with
            member _.FindAttractions(name, _, limit) =
                async {
                    return
                        testAttractions
                        |> List.filter (fun a -> a.Name.Contains(name))
                        |> List.truncate limit
                }

            member _.FindArtistsFromLocation(_, limit) =
                async { return testArtists |> List.truncate limit }

            member _.FindMoviesAboutLocation(_, limit) =
                async { return testMovies |> List.truncate limit }
        }

    // ============================================
    // サンプルデータ
    // ============================================

    let sampleLocation = {
        Id = LocationId "Q123"
        Name = "Tokyo"
        Population = 14000000
    }

    let sampleAttraction = {
        Name = "Tokyo Tower"
        Description = Some "Famous landmark in Tokyo"
        Location = sampleLocation
    }

    let sampleArtist = {
        Name = "Hikaru Utada"
        Genre = Some "Pop"
    }

    let sampleMovie = {
        Name = "Lost in Translation"
        BoxOffice = Some 44000000
    }

