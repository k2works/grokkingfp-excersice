module Ch11.TravelGuideTests

open Xunit
open Ch11.TravelGuide

// ============================================
// ドメインモデルのテスト
// ============================================

[<Fact>]
let ``LocationId は値を保持する`` () =
    let id = LocationId "Q123"
    Assert.Equal("Q123", LocationId.value id)

[<Fact>]
let ``Location は正しく作成される`` () =
    let location = {
        Id = LocationId "Q123"
        Name = "Tokyo"
        Population = 14000000
    }
    Assert.Equal("Tokyo", location.Name)
    Assert.Equal(14000000, location.Population)

[<Fact>]
let ``Attraction は正しく作成される`` () =
    let location = {
        Id = LocationId "Q123"
        Name = "Tokyo"
        Population = 14000000
    }
    let attraction = {
        Name = "Tokyo Tower"
        Description = Some "Famous landmark"
        Location = location
    }
    Assert.Equal("Tokyo Tower", attraction.Name)
    Assert.Equal(Some "Famous landmark", attraction.Description)

[<Fact>]
let ``TravelGuide は正しく作成される`` () =
    let location = {
        Id = LocationId "Q123"
        Name = "Tokyo"
        Population = 14000000
    }
    let attraction = {
        Name = "Tokyo Tower"
        Description = None
        Location = location
    }
    let guide = {
        Attraction = attraction
        Subjects = ["Artist1"; "Movie1"]
    }
    Assert.Equal(2, List.length guide.Subjects)

// ============================================
// データアクセスのテスト
// ============================================

[<Fact>]
let ``testDataAccess は正しいデータを返す`` () =
    let testLocation = {
        Id = LocationId "Q123"
        Name = "Test City"
        Population = 100000
    }
    let testAttractions = [
        { Name = "Test Tower"; Description = Some "A test"; Location = testLocation }
    ]
    let testArtists = [
        { Name = "Test Artist"; Genre = Some "Pop" }
    ]
    let testMovies = [
        { Name = "Test Movie"; BoxOffice = Some 1000000 }
    ]

    let dataAccess = testDataAccess testAttractions testArtists testMovies

    let attractions =
        dataAccess.FindAttractions("Test", ByName, 10)
        |> Async.RunSynchronously
    Assert.Equal(1, List.length attractions)
    Assert.Equal("Test Tower", attractions.[0].Name)

[<Fact>]
let ``testDataAccess はフィルタリングする`` () =
    let testLocation = {
        Id = LocationId "Q123"
        Name = "Test City"
        Population = 100000
    }
    let testAttractions = [
        { Name = "Test Tower"; Description = None; Location = testLocation }
        { Name = "Other Place"; Description = None; Location = testLocation }
    ]

    let dataAccess = testDataAccess testAttractions [] []

    let attractions =
        dataAccess.FindAttractions("Tower", ByName, 10)
        |> Async.RunSynchronously
    Assert.Equal(1, List.length attractions)

[<Fact>]
let ``testDataAccess は limit を尊重する`` () =
    let testLocation = {
        Id = LocationId "Q123"
        Name = "Test City"
        Population = 100000
    }
    let testAttractions = [
        { Name = "Tower 1"; Description = None; Location = testLocation }
        { Name = "Tower 2"; Description = None; Location = testLocation }
        { Name = "Tower 3"; Description = None; Location = testLocation }
    ]

    let dataAccess = testDataAccess testAttractions [] []

    let attractions =
        dataAccess.FindAttractions("Tower", ByName, 2)
        |> Async.RunSynchronously
    Assert.Equal(2, List.length attractions)

// ============================================
// travelGuide 関数のテスト
// ============================================

[<Fact>]
let ``travelGuide は有効なアトラクションに対してガイドを返す`` () =
    let testLocation = {
        Id = LocationId "Q123"
        Name = "Test City"
        Population = 100000
    }
    let testAttractions = [
        { Name = "Test Tower"; Description = Some "A tower"; Location = testLocation }
    ]
    let testArtists = [
        { Name = "Artist1"; Genre = None }
        { Name = "Artist2"; Genre = Some "Rock" }
    ]
    let testMovies = [
        { Name = "Movie1"; BoxOffice = None }
    ]

    let dataAccess = testDataAccess testAttractions testArtists testMovies
    let result = travelGuide dataAccess "Test" |> Async.RunSynchronously

    Assert.True(result.IsSome)
    let guide = result.Value
    Assert.Equal("Test Tower", guide.Attraction.Name)
    Assert.Equal(3, List.length guide.Subjects) // 2 artists + 1 movie

[<Fact>]
let ``travelGuide は見つからない場合 None を返す`` () =
    let dataAccess = testDataAccess [] [] []
    let result = travelGuide dataAccess "NonExistent" |> Async.RunSynchronously
    Assert.True(result.IsNone)

[<Fact>]
let ``travelGuides は複数のガイドを返す`` () =
    let testLocation1 = {
        Id = LocationId "Q123"
        Name = "City1"
        Population = 200000
    }
    let testLocation2 = {
        Id = LocationId "Q456"
        Name = "City2"
        Population = 100000
    }
    let testAttractions = [
        { Name = "Tower1"; Description = None; Location = testLocation1 }
        { Name = "Tower2"; Description = None; Location = testLocation2 }
    ]
    let testArtists = [{ Name = "Artist1"; Genre = None }]
    let testMovies = [{ Name = "Movie1"; BoxOffice = None }]

    let dataAccess = testDataAccess testAttractions testArtists testMovies
    let result = travelGuides dataAccess "Tower" 5 |> Async.RunSynchronously

    Assert.Equal(2, List.length result)

// ============================================
// キャッシュのテスト
// ============================================

[<Fact>]
let ``cachedDataAccess はデータをキャッシュする`` () =
    let mutable callCount = 0
    let testLocation = {
        Id = LocationId "Q123"
        Name = "Test City"
        Population = 100000
    }
    let testAttractions = [
        { Name = "Test Tower"; Description = None; Location = testLocation }
    ]

    let countingDataAccess =
        { new IDataAccess with
            member _.FindAttractions(name, ordering, limit) =
                async {
                    callCount <- callCount + 1
                    return
                        testAttractions
                        |> List.filter (fun a -> a.Name.Contains(name))
                        |> List.truncate limit
                }
            member _.FindArtistsFromLocation(_, limit) =
                async { return [] }
            member _.FindMoviesAboutLocation(_, limit) =
                async { return [] }
        }

    let cached = cachedDataAccess countingDataAccess |> Async.RunSynchronously

    // 最初の呼び出し
    let _ = cached.FindAttractions("Test", ByName, 10) |> Async.RunSynchronously
    Assert.Equal(1, callCount)

    // 2回目の呼び出し（キャッシュから）
    let _ = cached.FindAttractions("Test", ByName, 10) |> Async.RunSynchronously
    Assert.Equal(1, callCount) // 呼び出し回数は増えない

    // 異なるパラメータでの呼び出し
    let _ = cached.FindAttractions("Other", ByName, 10) |> Async.RunSynchronously
    Assert.Equal(2, callCount)

// ============================================
// Resource のテスト
// ============================================

[<Fact>]
let ``Resource.make は正しくリソースを作成する`` () =
    let mutable acquired = false
    let mutable released = false

    let resource = Resource.make
                    (fun () -> async { acquired <- true; return "resource" })
                    (fun _ -> async { released <- true })

    let result =
        resource
        |> Resource.useAsync (fun r -> async { return r + "-used" })
        |> Async.RunSynchronously

    Assert.Equal("resource-used", result)
    Assert.True(acquired)
    Assert.True(released)

// ============================================
// サンプルデータのテスト
// ============================================

[<Fact>]
let ``sampleLocation は正しい値を持つ`` () =
    Assert.Equal("Tokyo", sampleLocation.Name)
    Assert.Equal(14000000, sampleLocation.Population)

[<Fact>]
let ``sampleAttraction は正しい値を持つ`` () =
    Assert.Equal("Tokyo Tower", sampleAttraction.Name)
    Assert.True(sampleAttraction.Description.IsSome)

[<Fact>]
let ``sampleArtist は正しい値を持つ`` () =
    Assert.Equal("Hikaru Utada", sampleArtist.Name)

[<Fact>]
let ``sampleMovie は正しい値を持つ`` () =
    Assert.Equal("Lost in Translation", sampleMovie.Name)

