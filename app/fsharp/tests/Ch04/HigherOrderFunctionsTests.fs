module Ch04.HigherOrderFunctionsTests

open Xunit
open Ch04.HigherOrderFunctions
open Ch04.ProgrammingLanguages

// ============================================
// 基本的な関数のテスト
// ============================================

[<Fact>]
let ``wordScore は 'a' を除外した文字数を返す`` () =
    Assert.Equal(2, wordScore "java")   // j, v
    Assert.Equal(4, wordScore "rust")   // r, u, s, t
    Assert.Equal(3, wordScore "scala")  // s, c, l
    Assert.Equal(6, wordScore "haskell") // h, s, k, e, l, l

[<Fact>]
let ``bonus は 'c' を含む場合 5 を返す`` () =
    Assert.Equal(5, bonus "scala")
    Assert.Equal(5, bonus "clojure")
    Assert.Equal(0, bonus "java")
    Assert.Equal(0, bonus "rust")

[<Fact>]
let ``penalty は 's' を含む場合 7 を返す`` () =
    Assert.Equal(7, penalty "scala")
    Assert.Equal(7, penalty "rust")
    Assert.Equal(7, penalty "haskell")
    Assert.Equal(0, penalty "java")

// ============================================
// sortBy のテスト
// ============================================

[<Fact>]
let ``sortByScore はスコアの昇順でソートする`` () =
    let words = ["rust"; "java"]
    let sorted = sortByScore words

    // java: 2, rust: 4
    Assert.Equal<string list>(["java"; "rust"], sorted)

[<Fact>]
let ``sortByScoreDescending はスコアの降順でソートする`` () =
    let words = ["rust"; "java"; "haskell"]
    let sorted = sortByScoreDescending words

    // haskell: 6, rust: 4, java: 2
    Assert.Equal<string list>(["haskell"; "rust"; "java"], sorted)

// ============================================
// map のテスト
// ============================================

[<Fact>]
let ``lengths は各文字列の長さを返す`` () =
    Assert.Equal<int list>([5; 4; 3], lengths ["scala"; "rust"; "ada"])

[<Fact>]
let ``doubles は各数値を2倍にする`` () =
    Assert.Equal<int list>([10; 2; 4; 8; 0], doubles [5; 1; 2; 4; 0])

// ============================================
// filter のテスト
// ============================================

[<Fact>]
let ``filterOdds は奇数のみを返す`` () =
    Assert.Equal<int list>([5; 1], filterOdds [5; 1; 2; 4; 0])

[<Fact>]
let ``filterLargerThan は指定した値より大きい要素を返す`` () =
    Assert.Equal<int list>([5], filterLargerThan 4 [5; 1; 2; 4; 0])
    Assert.Equal<int list>([5; 2; 4], filterLargerThan 1 [5; 1; 2; 4; 0])

// ============================================
// fold のテスト
// ============================================

[<Fact>]
let ``sum はリストの合計を返す`` () =
    Assert.Equal(112, sum [5; 1; 2; 4; 100])
    Assert.Equal(0, sum [])

[<Fact>]
let ``maximum はリストの最大値を返す`` () =
    Assert.Equal(15, maximum [5; 1; 2; 4; 15])
    Assert.Equal(100, maximum [5; 1; 100; 4; 15])

[<Fact>]
let ``countWhere は条件を満たす要素の数を返す`` () =
    Assert.Equal(2, countWhere (fun i -> i > 3) [1; 2; 3; 4; 5])
    Assert.Equal(2, countWhere (fun (s: string) -> s.Length > 1) ["a"; "bb"; "ccc"])

// ============================================
// 関数を返す関数のテスト
// ============================================

[<Fact>]
let ``largerThan はnより大きいか判定する関数を返す`` () =
    let largerThan4 = largerThan 4

    Assert.True(largerThan4 5)
    Assert.False(largerThan4 4)
    Assert.False(largerThan4 3)

[<Fact>]
let ``divisibleBy はnで割り切れるか判定する関数を返す`` () =
    let divisibleBy3 = divisibleBy 3

    Assert.True(divisibleBy3 9)
    Assert.True(divisibleBy3 6)
    Assert.False(divisibleBy3 7)

[<Fact>]
let ``containsChar は指定した文字を含むか判定する関数を返す`` () =
    let containsA = containsChar 'a'

    Assert.True(containsA "scala")
    Assert.True(containsA "java")
    Assert.False(containsA "rust")

// ============================================
// ワードランキングのテスト
// ============================================

[<Fact>]
let ``rankedWords は指定したスコア関数でランキングする`` () =
    let words = ["ada"; "haskell"; "scala"; "java"; "rust"]

    // 基本スコアでランキング (haskell:6, rust:4, scala:3, java:2, ada:1)
    let ranking1 = rankByScore words
    Assert.Equal<string list>(["haskell"; "rust"; "scala"; "java"; "ada"], ranking1)

[<Fact>]
let ``rankByScoreWithBonus はボーナス付きでランキングする`` () =
    let words = ["ada"; "haskell"; "scala"; "java"; "rust"]

    // ボーナス付きスコアでランキング
    // scala: 3+5=8, haskell: 6+0=6, rust: 4+0=4, java: 2+0=2, ada: 1+0=1
    let ranking2 = rankByScoreWithBonus words
    Assert.Equal<string list>(["scala"; "haskell"; "rust"; "java"; "ada"], ranking2)

[<Fact>]
let ``rankByScoreWithBonusAndPenalty はボーナスとペナルティ付きでランキングする`` () =
    let words = ["ada"; "haskell"; "scala"; "java"; "rust"]

    // ボーナスとペナルティ付きスコアでランキング
    // java: 2+0-0=2, scala: 3+5-7=1, ada: 1+0-0=1, haskell: 6+0-7=-1, rust: 4+0-7=-3
    // scala と ada は同スコアなので順序は安定ソートに依存
    let ranking3 = rankByScoreWithBonusAndPenalty words
    // java が最高スコアで最初、haskell と rust が最後
    Assert.Equal("java", ranking3.[0])
    Assert.Equal("haskell", ranking3.[3])
    Assert.Equal("rust", ranking3.[4])

// ============================================
// ProgrammingLanguages のテスト
// ============================================

[<Fact>]
let ``getNames は言語名のリストを返す`` () =
    let languages = [
        { Name = "Java"; Year = 1995 }
        { Name = "Scala"; Year = 2004 }
    ]

    Assert.Equal<string list>(["Java"; "Scala"], getNames languages)

[<Fact>]
let ``filterByYear は指定した年より後の言語をフィルタする`` () =
    let languages = [
        { Name = "Java"; Year = 1995 }
        { Name = "Scala"; Year = 2004 }
        { Name = "Kotlin"; Year = 2011 }
    ]

    let young = filterByYear 2000 languages

    Assert.Equal(2, List.length young)
    Assert.Equal<string list>(["Scala"; "Kotlin"], getNames young)

[<Fact>]
let ``sortByYear は年でソートする`` () =
    let languages = [
        { Name = "Scala"; Year = 2004 }
        { Name = "Java"; Year = 1995 }
        { Name = "Kotlin"; Year = 2011 }
    ]

    let sorted = sortByYear languages

    Assert.Equal<string list>(["Java"; "Scala"; "Kotlin"], getNames sorted)
