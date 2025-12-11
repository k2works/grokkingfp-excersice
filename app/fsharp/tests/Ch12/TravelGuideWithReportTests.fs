module Ch12.TravelGuideWithReportTests

open Xunit
open Ch11.TravelGuide
open Ch12.TravelGuideWithReport

// ============================================
// SearchReport のテスト
// ============================================

[<Fact>]
let ``SearchReport.empty は空のレポートを返す`` () =
    let report = SearchReport.empty
    Assert.Equal(0, report.AttractionsSearched)
    Assert.Equal(0, report.ArtistsFound)
    Assert.Equal(0, report.MoviesFound)
    Assert.Empty(report.Errors)

[<Fact>]
let ``SearchReport.addError はエラーを追加する`` () =
    let report =
        SearchReport.empty
        |> SearchReport.addError "Error 1"
        |> SearchReport.addError "Error 2"
    Assert.Equal(2, List.length report.Errors)

[<Fact>]
let ``SearchReport.withAttractions は検索数を設定する`` () =
    let report =
        SearchReport.empty
        |> SearchReport.withAttractions 5
    Assert.Equal(5, report.AttractionsSearched)

// ============================================
// エラーハンドリング付きデータアクセスのテスト
// ============================================

[<Fact>]
let ``successfulDataAccess は成功結果を返す`` () =
    let testLocation = {
        Id = LocationId "Q123"
        Name = "Test City"
        Population = 100000
    }
    let testAttractions = [
        { Name = "Test Tower"; Description = None; Location = testLocation }
    ]
    let testArtists = [{ Name = "Artist1"; Genre = None }]
    let testMovies = [{ Name = "Movie1"; BoxOffice = None }]

    let dataAccess = successfulDataAccess testAttractions testArtists testMovies

    let artistsResult =
        dataAccess.FindArtistsFromLocation(LocationId "Q123", 10)
        |> Async.RunSynchronously

    match artistsResult with
    | Ok artists -> Assert.Equal(1, List.length artists)
    | Error _ -> Assert.True(false, "Expected Ok result")

[<Fact>]
let ``partiallyFailingDataAccess はエラーを返す`` () =
    let testLocation = {
        Id = LocationId "Q123"
        Name = "Test City"
        Population = 100000
    }
    let testAttractions = [
        { Name = "Test Tower"; Description = None; Location = testLocation }
    ]

    let dataAccess = partiallyFailingDataAccess testAttractions "Network error" "Timeout"

    let artistsResult =
        dataAccess.FindArtistsFromLocation(LocationId "Q123", 10)
        |> Async.RunSynchronously

    match artistsResult with
    | Ok _ -> Assert.True(false, "Expected Error result")
    | Error e -> Assert.Equal("Network error", e)

// ============================================
// travelGuideWithReport のテスト
// ============================================

[<Fact>]
let ``travelGuideWithReport は成功時にレポートを含む`` () =
    let testLocation = {
        Id = LocationId "Q123"
        Name = "Test City"
        Population = 100000
    }
    let testAttractions = [
        { Name = "Test Tower"; Description = None; Location = testLocation }
    ]
    let testArtists = [{ Name = "Artist1"; Genre = None }]
    let testMovies = [{ Name = "Movie1"; BoxOffice = None }]

    let dataAccess = successfulDataAccess testAttractions testArtists testMovies
    let result = travelGuideWithReport dataAccess "Test" |> Async.RunSynchronously

    Assert.True(result.IsSome)
    let guide = result.Value
    Assert.Equal(1, guide.SearchReport.AttractionsSearched)
    Assert.Equal(1, guide.SearchReport.ArtistsFound)
    Assert.Equal(1, guide.SearchReport.MoviesFound)
    Assert.Empty(guide.SearchReport.Errors)

[<Fact>]
let ``travelGuideWithReport はエラー時にレポートにエラーを含む`` () =
    let testLocation = {
        Id = LocationId "Q123"
        Name = "Test City"
        Population = 100000
    }
    let testAttractions = [
        { Name = "Test Tower"; Description = None; Location = testLocation }
    ]

    let dataAccess = partiallyFailingDataAccess testAttractions "Artist error" "Movie error"
    let result = travelGuideWithReport dataAccess "Test" |> Async.RunSynchronously

    Assert.True(result.IsSome)
    let guide = result.Value
    Assert.Equal(2, List.length guide.SearchReport.Errors)
    Assert.Contains("Artist error", guide.SearchReport.Errors)
    Assert.Contains("Movie error", guide.SearchReport.Errors)

[<Fact>]
let ``travelGuideWithReport はアトラクションが見つからない場合 None を返す`` () =
    let dataAccess = successfulDataAccess [] [] []
    let result = travelGuideWithReport dataAccess "NonExistent" |> Async.RunSynchronously
    Assert.True(result.IsNone)

[<Fact>]
let ``travelGuidesWithReport は複数のガイドを返す`` () =
    let testLocation1 = { Id = LocationId "Q123"; Name = "City1"; Population = 200000 }
    let testLocation2 = { Id = LocationId "Q456"; Name = "City2"; Population = 100000 }
    let testAttractions = [
        { Name = "Tower1"; Description = None; Location = testLocation1 }
        { Name = "Tower2"; Description = None; Location = testLocation2 }
    ]

    let dataAccess = successfulDataAccess testAttractions [] []
    let result = travelGuidesWithReport dataAccess "Tower" 5 |> Async.RunSynchronously

    Assert.Equal(2, List.length result)

// ============================================
// Generators のテスト
// ============================================

[<Fact>]
let ``Generators.locationId は有効な LocationId を生成する`` () =
    let id = Generators.locationId ()
    let value = LocationId.value id
    Assert.StartsWith("Q", value)

[<Fact>]
let ``Generators.location は有効な Location を生成する`` () =
    let location = Generators.location ()
    Assert.True(location.Population >= 1000)
    Assert.True(location.Population <= 10000000)

[<Fact>]
let ``Generators.attraction は有効な Attraction を生成する`` () =
    let attraction = Generators.attraction ()
    Assert.StartsWith("Attraction", attraction.Name)

[<Fact>]
let ``Generators.attractions は指定数の Attraction を生成する`` () =
    let attractions = Generators.attractions 5
    Assert.Equal(5, List.length attractions)

// ============================================
// Invariants のテスト
// ============================================

[<Fact>]
let ``Invariants.resultSizeIsLimited は正しく検証する`` () =
    Assert.True(Invariants.resultSizeIsLimited [1; 2; 3] 5)
    Assert.True(Invariants.resultSizeIsLimited [1; 2; 3] 3)
    Assert.False(Invariants.resultSizeIsLimited [1; 2; 3] 2)

[<Fact>]
let ``Invariants.errorsAreBounded は正しく検証する`` () =
    let report1 = { SearchReport.empty with Errors = [] }
    let report2 = { SearchReport.empty with Errors = ["e1"; "e2"] }
    let report3 = { SearchReport.empty with Errors = ["e1"; "e2"; "e3"] }

    Assert.True(Invariants.errorsAreBounded report1)
    Assert.True(Invariants.errorsAreBounded report2)
    Assert.False(Invariants.errorsAreBounded report3)

// ============================================
// ユーティリティ関数のテスト
// ============================================

[<Fact>]
let ``filterPopularLocations は正しくフィルタする`` () =
    let locations = [
        { Id = LocationId "Q1"; Name = "Small"; Population = 5000 }
        { Id = LocationId "Q2"; Name = "Medium"; Population = 50000 }
        { Id = LocationId "Q3"; Name = "Large"; Population = 500000 }
    ]

    let result = filterPopularLocations locations 10000
    Assert.Equal(2, List.length result)
    Assert.DoesNotContain({ Id = LocationId "Q1"; Name = "Small"; Population = 5000 }, result)

[<Fact>]
let ``sortAttractionsByPopulation は人口順にソートする`` () =
    let loc1 = { Id = LocationId "Q1"; Name = "Small"; Population = 5000 }
    let loc2 = { Id = LocationId "Q2"; Name = "Large"; Population = 500000 }
    let attractions = [
        { Name = "Attr1"; Description = None; Location = loc1 }
        { Name = "Attr2"; Description = None; Location = loc2 }
    ]

    let result = sortAttractionsByPopulation attractions
    Assert.Equal("Attr2", result.[0].Name)
    Assert.Equal("Attr1", result.[1].Name)

[<Fact>]
let ``sortAttractionsByName は名前順にソートする`` () =
    let loc = { Id = LocationId "Q1"; Name = "City"; Population = 100000 }
    let attractions = [
        { Name = "Zoo"; Description = None; Location = loc }
        { Name = "Aquarium"; Description = None; Location = loc }
        { Name = "Museum"; Description = None; Location = loc }
    ]

    let result = sortAttractionsByName attractions
    Assert.Equal("Aquarium", result.[0].Name)
    Assert.Equal("Museum", result.[1].Name)
    Assert.Equal("Zoo", result.[2].Name)

// ============================================
// プロパティベーステスト（簡易版）
// ============================================

[<Fact>]
let ``filterPopularLocations の結果サイズは入力以下`` () =
    for _ in 1..10 do
        let locations = Generators.attractions 10 |> List.map (fun a -> a.Location)
        let minPop = System.Random().Next(1, 10000000)
        let result = filterPopularLocations locations minPop
        Assert.True(List.length result <= List.length locations)

[<Fact>]
let ``filterPopularLocations の結果はすべて条件を満たす`` () =
    for _ in 1..10 do
        let locations = Generators.attractions 10 |> List.map (fun a -> a.Location)
        let minPop = System.Random().Next(1, 100000)
        let result = filterPopularLocations locations minPop
        Assert.True(result |> List.forall (fun loc -> loc.Population >= minPop))

[<Fact>]
let ``sortAttractionsByPopulation は降順になる`` () =
    for _ in 1..10 do
        let attractions = Generators.attractions 5
        let result = sortAttractionsByPopulation attractions
        let populations = result |> List.map (fun a -> a.Location.Population)
        let sorted = populations |> List.sortDescending
        Assert.Equal<int list>(sorted, populations)

// ============================================
// 統合テスト
// ============================================

[<Fact>]
let ``完全なフローのテスト - 成功ケース`` () =
    let testLocation = {
        Id = LocationId "Q123"
        Name = "Tokyo"
        Population = 14000000
    }
    let testAttractions = [
        { Name = "Tokyo Tower"; Description = Some "Famous landmark"; Location = testLocation }
    ]
    let testArtists = [
        { Name = "Hikaru Utada"; Genre = Some "Pop" }
        { Name = "Ayumi Hamasaki"; Genre = Some "Pop" }
    ]
    let testMovies = [
        { Name = "Lost in Translation"; BoxOffice = Some 44000000 }
    ]

    let dataAccess = successfulDataAccess testAttractions testArtists testMovies
    let result = travelGuideWithReport dataAccess "Tokyo" |> Async.RunSynchronously

    Assert.True(result.IsSome)
    let guide = result.Value
    Assert.Equal("Tokyo Tower", guide.Attraction.Name)
    Assert.Equal(3, List.length guide.Subjects)
    Assert.Equal(1, guide.SearchReport.AttractionsSearched)
    Assert.Equal(2, guide.SearchReport.ArtistsFound)
    Assert.Equal(1, guide.SearchReport.MoviesFound)
    Assert.Empty(guide.SearchReport.Errors)

[<Fact>]
let ``完全なフローのテスト - 部分的失敗ケース`` () =
    let testLocation = {
        Id = LocationId "Q123"
        Name = "Tokyo"
        Population = 14000000
    }
    let testAttractions = [
        { Name = "Tokyo Tower"; Description = Some "Famous landmark"; Location = testLocation }
    ]

    let dataAccess = partiallyFailingDataAccess testAttractions "Network error" "Timeout"
    let result = travelGuideWithReport dataAccess "Tokyo" |> Async.RunSynchronously

    Assert.True(result.IsSome)
    let guide = result.Value
    Assert.Equal("Tokyo Tower", guide.Attraction.Name)
    Assert.Empty(guide.Subjects) // エラーのため空
    Assert.Equal(2, List.length guide.SearchReport.Errors)

