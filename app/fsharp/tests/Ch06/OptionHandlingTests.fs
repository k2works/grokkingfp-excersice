module Ch06.OptionHandlingTests

open Xunit
open Ch06.OptionHandling
open Ch06.UserModel

// ============================================
// 安全な除算のテスト
// ============================================

[<Fact>]
let ``safeDivide は正常に除算できる`` () =
    Assert.Equal(Some 5, safeDivide 10 2)
    Assert.Equal(Some 3, safeDivide 7 2)
    Assert.Equal(Some 0, safeDivide 0 5)

[<Fact>]
let ``safeDivide はゼロ除算で None を返す`` () =
    Assert.Equal(None, safeDivide 10 0)
    Assert.Equal(None, safeDivide 0 0)

// ============================================
// 文字列パースのテスト
// ============================================

[<Fact>]
let ``tryParseInt は有効な整数文字列をパースする`` () =
    Assert.Equal(Some 123, tryParseInt "123")
    Assert.Equal(Some -456, tryParseInt "-456")
    Assert.Equal(Some 0, tryParseInt "0")

[<Fact>]
let ``tryParseInt は無効な文字列で None を返す`` () =
    Assert.Equal(None, tryParseInt "abc")
    Assert.Equal(None, tryParseInt "12.34")
    Assert.Equal(None, tryParseInt "")

// ============================================
// TV番組パーサーのテスト
// ============================================

[<Fact>]
let ``extractName は番組名を抽出する`` () =
    Assert.Equal(Some "Breaking Bad", extractName "Breaking Bad (2008-2013)")
    Assert.Equal(Some "The Wire", extractName "The Wire (2002-2008)")

[<Fact>]
let ``extractName は無効な形式で None を返す`` () =
    Assert.Equal(None, extractName "(2008-2013)")
    Assert.Equal(None, extractName "No brackets")

[<Fact>]
let ``extractYearStart は開始年を抽出する`` () =
    Assert.Equal(Some 2008, extractYearStart "Breaking Bad (2008-2013)")
    Assert.Equal(Some 2002, extractYearStart "The Wire (2002-2008)")

[<Fact>]
let ``extractYearStart は無効な形式で None を返す`` () =
    Assert.Equal(None, extractYearStart "Chernobyl (2019)")
    Assert.Equal(None, extractYearStart "Invalid")

[<Fact>]
let ``extractYearEnd は終了年を抽出する`` () =
    Assert.Equal(Some 2013, extractYearEnd "Breaking Bad (2008-2013)")
    Assert.Equal(Some 2008, extractYearEnd "The Wire (2002-2008)")

[<Fact>]
let ``extractSingleYear は単年を抽出する`` () =
    Assert.Equal(Some 2019, extractSingleYear "Chernobyl (2019)")
    Assert.Equal(None, extractSingleYear "Breaking Bad (2008-2013)")

[<Fact>]
let ``parseShow は正常な形式をパースする`` () =
    let result = parseShow "Breaking Bad (2008-2013)"
    Assert.True(result.IsSome)
    let show = result.Value
    Assert.Equal("Breaking Bad", show.Title)
    Assert.Equal(2008, show.Start)
    Assert.Equal(2013, show.End)

[<Fact>]
let ``parseShow は単年の番組をパースする`` () =
    let result = parseShow "Chernobyl (2019)"
    Assert.True(result.IsSome)
    let show = result.Value
    Assert.Equal("Chernobyl", show.Title)
    Assert.Equal(2019, show.Start)
    Assert.Equal(2019, show.End)

[<Fact>]
let ``parseShow は無効な形式で None を返す`` () =
    Assert.True((parseShow "The Wire 2002-2008").IsNone)
    Assert.True((parseShow "(2008-2013)").IsNone)
    Assert.True((parseShow "Invalid").IsNone)

// ============================================
// 複数番組パースのテスト
// ============================================

[<Fact>]
let ``parseShowsBestEffort はパースできたものだけ返す`` () =
    let rawShows = [
        "Breaking Bad (2008-2013)"
        "Invalid Format"
        "Mad Men (2007-2015)"
    ]
    let result = parseShowsBestEffort rawShows

    Assert.Equal(2, List.length result)
    Assert.Equal("Breaking Bad", result.[0].Title)
    Assert.Equal("Mad Men", result.[1].Title)

[<Fact>]
let ``parseShowsAllOrNothing は全部成功したら Some を返す`` () =
    let rawShows = [
        "Breaking Bad (2008-2013)"
        "Mad Men (2007-2015)"
    ]
    let result = parseShowsAllOrNothing rawShows

    Assert.True(result.IsSome)
    Assert.Equal(2, List.length result.Value)

[<Fact>]
let ``parseShowsAllOrNothing は一つでも失敗したら None を返す`` () =
    let rawShows = [
        "Breaking Bad (2008-2013)"
        "Invalid Format"
    ]
    let result = parseShowsAllOrNothing rawShows

    Assert.True(result.IsNone)

// ============================================
// addStrings のテスト
// ============================================

[<Fact>]
let ``addStrings は2つの数値文字列を加算する`` () =
    Assert.Equal(Some 30, addStrings "10" "20")
    Assert.Equal(Some 0, addStrings "5" "-5")

[<Fact>]
let ``addStrings は無効な文字列で None を返す`` () =
    Assert.Equal(None, addStrings "10" "abc")
    Assert.Equal(None, addStrings "xyz" "20")
    Assert.Equal(None, addStrings "abc" "xyz")

// ============================================
// forall と exists のテスト
// ============================================

[<Fact>]
let ``forallExample は Option.forall の動作を示す`` () =
    // Some の場合: 条件をチェック
    Assert.True(forallExample (Some 996) 2020)
    Assert.False(forallExample (Some 996) 500)

    // None の場合: 常に true
    Assert.True(forallExample None 2020)
    Assert.True(forallExample None 500)

[<Fact>]
let ``existsExample は Option.exists の動作を示す`` () =
    // Some の場合: 条件をチェック
    Assert.True(existsExample (Some 996) 2020)
    Assert.False(existsExample (Some 996) 500)

    // None の場合: 常に false
    Assert.False(existsExample None 2020)
    Assert.False(existsExample None 500)

// ============================================
// UserModel のテスト
// ============================================

[<Fact>]
let ``filterByEmailDomain は forall で正しくフィルタする`` () =
    let users = [
        { Name = "Alice"; Email = Some "alice@example.com"; Age = 25 }
        { Name = "Bob"; Email = None; Age = 30 }
        { Name = "Charlie"; Email = Some "charlie@test.com"; Age = 17 }
    ]

    // メールなし or example.com
    let result = filterByEmailDomain "@example.com" users

    Assert.Equal(2, List.length result)
    Assert.Contains(users.[0], result) // Alice
    Assert.Contains(users.[1], result) // Bob (None なので forall = true)

[<Fact>]
let ``filterByEmailDomainExists は exists で正しくフィルタする`` () =
    let users = [
        { Name = "Alice"; Email = Some "alice@example.com"; Age = 25 }
        { Name = "Bob"; Email = None; Age = 30 }
        { Name = "Charlie"; Email = Some "charlie@test.com"; Age = 17 }
    ]

    // メールあり and test.com
    let result = filterByEmailDomainExists "@test.com" users

    Assert.Single(result) |> ignore
    Assert.Equal("Charlie", result.[0].Name)
