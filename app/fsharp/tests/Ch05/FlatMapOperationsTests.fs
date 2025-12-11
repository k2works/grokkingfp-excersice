module Ch05.FlatMapOperationsTests

open Xunit
open Ch05.FlatMapOperations

// ============================================
// flatten のテスト
// ============================================

[<Fact>]
let ``flatten はネストしたリストを平坦化する`` () =
    let nested = [["a"; "b"]; ["c"]]
    let flat = flatten nested

    Assert.Equal<string list>(["a"; "b"; "c"], flat)

[<Fact>]
let ``flatten は空のリストを正しく処理する`` () =
    let nested = [["a"]; []; ["b"]]
    let flat = flatten nested

    Assert.Equal<string list>(["a"; "b"], flat)

// ============================================
// getAllAuthors のテスト
// ============================================

[<Fact>]
let ``getAllAuthors は本のリストから著者のリストを取得する`` () =
    let books = [
        { Title = "FP in Scala"; Authors = ["Chiusano"; "Bjarnason"] }
        { Title = "The Hobbit"; Authors = ["Tolkien"] }
    ]

    let authors = getAllAuthors books

    Assert.Equal<string list>(["Chiusano"; "Bjarnason"; "Tolkien"], authors)

// ============================================
// flatMap によるサイズ変化のテスト
// ============================================

[<Fact>]
let ``duplicate は各要素を複製する（サイズ増加）`` () =
    let result = duplicate [1; 2; 3]

    // 1, 11, 2, 12, 3, 13
    Assert.Equal<int list>([1; 11; 2; 12; 3; 13], result)

[<Fact>]
let ``mapToList は各要素を2倍にする（サイズ維持）`` () =
    let result = mapToList [1; 2; 3]

    Assert.Equal<int list>([2; 4; 6], result)

[<Fact>]
let ``filterEven は偶数のみをフィルタする（サイズ減少）`` () =
    let result = filterEven [1; 2; 3; 4; 5; 6]

    Assert.Equal<int list>([2; 4; 6], result)

// ============================================
// 映画のレコメンデーションのテスト
// ============================================

[<Fact>]
let ``bookAdaptations は著者から映画化作品を取得する`` () =
    let tolkienMovies = bookAdaptations "Tolkien"

    Assert.Equal(2, List.length tolkienMovies)
    Assert.Equal("An Unexpected Journey", tolkienMovies.[0].Title)
    Assert.Equal("The Desolation of Smaug", tolkienMovies.[1].Title)

[<Fact>]
let ``bookAdaptations は映画化作品がない著者には空リストを返す`` () =
    let movies = bookAdaptations "Unknown"

    Assert.Empty(movies)

[<Fact>]
let ``getRecommendations はレコメンデーションを生成する`` () =
    let books = [
        { Title = "FP in Scala"; Authors = ["Chiusano"; "Bjarnason"] }
        { Title = "The Hobbit"; Authors = ["Tolkien"] }
    ]

    let recommendations = getRecommendations books

    Assert.Equal(2, List.length recommendations)
    Assert.Contains("An Unexpected Journey", recommendations.[0])
    Assert.Contains("Tolkien", recommendations.[0])
    Assert.Contains("The Hobbit", recommendations.[0])

[<Fact>]
let ``getRecommendationsSeq はシーケンス式でも同じ結果を返す`` () =
    let books = [
        { Title = "The Hobbit"; Authors = ["Tolkien"] }
    ]

    let r1 = getRecommendations books
    let r2 = getRecommendationsSeq books

    Assert.Equal<string list>(r1, r2)

// ============================================
// 円内の点の判定のテスト
// ============================================

[<Fact>]
let ``isInside は点が円内にあるかを判定する`` () =
    // 点 (1, 1) は半径 2 の円内にある (1^2 + 1^2 = 2 <= 4)
    Assert.True(isInside { X = 1; Y = 1 } 2)

    // 点 (5, 2) は半径 2 の円内にない (5^2 + 2^2 = 29 > 4)
    Assert.False(isInside { X = 5; Y = 2 } 2)

    // 点 (1, 1) は半径 1 の円内にない (1^2 + 1^2 = 2 > 1)
    Assert.False(isInside { X = 1; Y = 1 } 1)

    // 原点は常に円内
    Assert.True(isInside { X = 0; Y = 0 } 1)

[<Fact>]
let ``allCombinations は全組み合わせを生成する`` () =
    let points = [{ X = 5; Y = 2 }; { X = 1; Y = 1 }]
    let radiuses = [2; 1]

    let combinations = allCombinations radiuses points

    Assert.Equal(4, List.length combinations)

[<Fact>]
let ``insidePointsOnly は条件を満たす組み合わせのみを返す`` () =
    let points = [{ X = 5; Y = 2 }; { X = 1; Y = 1 }]
    let radiuses = [2; 1]

    let insideOnly = insidePointsOnly radiuses points

    // 半径2の円内に (1,1) のみが入る
    Assert.Single(insideOnly) |> ignore
    Assert.Contains("Point(1,1)", insideOnly.[0])
    Assert.Contains("radius of 2", insideOnly.[0])

// ============================================
// シーケンス式のテスト
// ============================================

[<Fact>]
let ``listComprehension はリストの全組み合わせを生成する`` () =
    let result = listComprehension [1; 2] [2; 1]

    // 1*2=2, 1*1=1, 2*2=4, 2*1=2
    Assert.Equal<int list>([2; 1; 4; 2], result)

[<Fact>]
let ``setToListComprehension はセットからリストへの変換を行う`` () =
    let result = setToListComprehension (Set.ofList [1; 2]) [2; 1]

    // セットは順序が保証されないため、要素の存在のみ確認
    Assert.Equal(4, List.length result)
    Assert.Contains(2, result)
    Assert.Contains(1, result)
    Assert.Contains(4, result)

// ============================================
// 追加のユーティリティのテスト
// ============================================

[<Fact>]
let ``allTripleCombinations は3つのリストの全組み合わせを生成する`` () =
    let result = allTripleCombinations [1; 2] [10; 20] [100; 200]

    // 2 * 2 * 2 = 8 combinations
    Assert.Equal(8, List.length result)
    Assert.Contains(111, result) // 1 + 10 + 100
    Assert.Contains(222, result) // 2 + 20 + 200

[<Fact>]
let ``filterEvenWithFlatMap は flatMap で偶数をフィルタする`` () =
    let result = filterEvenWithFlatMap [1; 2; 3; 4; 5; 6]

    Assert.Equal<int list>([2; 4; 6], result)

[<Fact>]
let ``filterWithOption は述語に基づいてフィルタする`` () =
    let result = filterWithOption (fun i -> i > 3) [1; 2; 3; 4; 5]

    Assert.Equal<int list>([4; 5], result)
