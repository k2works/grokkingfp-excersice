module Ch03.ImmutableOperationsTests

open Xunit
open Ch03.ImmutableOperations
open Ch03.Itinerary
open Ch03.StringOperations

// ============================================
// 基本的なリスト操作のテスト
// ============================================

[<Fact>]
let ``firstTwo は最初の2要素を返す`` () =
    Assert.Equal<string list>(["a"; "b"], firstTwo ["a"; "b"; "c"])
    Assert.Equal<string list>(["a"; "b"], firstTwo ["a"; "b"])
    Assert.Equal<string list>(["a"], firstTwo ["a"])
    Assert.Equal<string list>([], firstTwo ([] : string list))

[<Fact>]
let ``lastTwo は最後の2要素を返す`` () =
    Assert.Equal<string list>(["b"; "c"], lastTwo ["a"; "b"; "c"])
    Assert.Equal<string list>(["a"; "b"], lastTwo ["a"; "b"])
    Assert.Equal<string list>(["a"], lastTwo ["a"])
    Assert.Equal<string list>([], lastTwo ([] : string list))

[<Fact>]
let ``slice はリストの一部を切り出す`` () =
    Assert.Equal<string list>(["b"; "c"], slice 1 3 ["a"; "b"; "c"; "d"])
    Assert.Equal<string list>(["a"; "b"], slice 0 2 ["a"; "b"; "c"])
    Assert.Equal<string list>([], slice 0 0 ["a"; "b"; "c"])

// ============================================
// 要素の追加のテスト
// ============================================

[<Fact>]
let ``appended はリストの末尾に要素を追加する`` () =
    let appleBook = ["Apple"; "Book"]
    let appleBookMango = appended "Mango" appleBook

    // 元のリストは変わらない（イミュータブル）
    Assert.Equal(2, List.length appleBook)
    Assert.Equal(3, List.length appleBookMango)
    Assert.Equal<string list>(["Apple"; "Book"; "Mango"], appleBookMango)

[<Fact>]
let ``appendedAll はリストの末尾に複数の要素を追加する`` () =
    let ab = ["a"; "b"]
    let cd = ["c"; "d"]
    let abcd = appendedAll cd ab

    Assert.Equal<string list>(["a"; "b"; "c"; "d"], abcd)

// ============================================
// リスト変換パターンのテスト
// ============================================

[<Fact>]
let ``moveFirstTwoToEnd は最初の2要素を末尾に移動する`` () =
    Assert.Equal<string list>(["c"; "a"; "b"], moveFirstTwoToEnd ["a"; "b"; "c"])
    Assert.Equal<string list>(["c"; "d"; "a"; "b"], moveFirstTwoToEnd ["a"; "b"; "c"; "d"])

[<Fact>]
let ``insertBeforeLast は最後の要素の前に挿入する`` () =
    Assert.Equal<string list>(["a"; "c"; "b"], insertBeforeLast "c" ["a"; "b"])
    Assert.Equal<string list>(["a"; "b"; "X"; "c"], insertBeforeLast "X" ["a"; "b"; "c"])

[<Fact>]
let ``insertAtMiddle は中央に要素を挿入する`` () =
    Assert.Equal<string list>(["a"; "b"; "X"; "c"; "d"], insertAtMiddle "X" ["a"; "b"; "c"; "d"])
    Assert.Equal<string list>(["a"; "X"; "b"], insertAtMiddle "X" ["a"; "b"])
    Assert.Equal<string list>(["X"; "a"], insertAtMiddle "X" ["a"])

// ============================================
// 旅程の再計画のテスト
// ============================================

[<Fact>]
let ``replan は指定した都市の前に新しい都市を挿入する`` () =
    let planA = ["Paris"; "Berlin"; "Kraków"]
    let planB = replan planA "Vienna" "Kraków"

    // Plan B には Vienna が追加されている
    Assert.Equal<string list>(["Paris"; "Berlin"; "Vienna"; "Kraków"], planB)

    // Plan A は変更されていない（イミュータブル）
    Assert.Equal<string list>(["Paris"; "Berlin"; "Kraków"], planA)

[<Fact>]
let ``replan は都市が見つからない場合末尾に追加する`` () =
    let plan = ["Paris"; "Berlin"]
    let result = replan plan "Vienna" "NotFound"

    Assert.Equal<string list>(["Paris"; "Berlin"; "Vienna"], result)

// ============================================
// 文字列操作のテスト
// ============================================

[<Fact>]
let ``abbreviate は名前を省略形に変換する`` () =
    Assert.Equal("A. Church", abbreviate "Alonzo Church")
    Assert.Equal("A. Turing", abbreviate "Alan Turing")

[<Fact>]
let ``abbreviate は既に省略形の場合はそのまま返す`` () =
    Assert.Equal("A. Church", abbreviate "A. Church")

[<Fact>]
let ``abbreviate はスペースがない場合はそのまま返す`` () =
    Assert.Equal("Alonzo", abbreviate "Alonzo")

[<Fact>]
let ``firstNChars は最初のn文字を返す`` () =
    Assert.Equal("Hel", firstNChars 3 "Hello")
    Assert.Equal("Hello", firstNChars 10 "Hello")
    Assert.Equal("", firstNChars 0 "Hello")

[<Fact>]
let ``lastNChars は最後のn文字を返す`` () =
    Assert.Equal("llo", lastNChars 3 "Hello")
    Assert.Equal("Hello", lastNChars 10 "Hello")
    Assert.Equal("", lastNChars 0 "Hello")
