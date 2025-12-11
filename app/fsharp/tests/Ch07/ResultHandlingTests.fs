module Ch07.ResultHandlingTests

open Xunit
open Ch07.ResultHandling
open Ch07.MusicArtists
open Ch07.PaymentMethods

// ============================================
// 基本的な Result のテスト
// ============================================

[<Fact>]
let ``tryParseInt は有効な整数文字列をパースする`` () =
    Assert.Equal(Ok 123, tryParseInt "123")
    Assert.Equal(Ok -456, tryParseInt "-456")

[<Fact>]
let ``tryParseInt は無効な文字列でエラーメッセージを返す`` () =
    match tryParseInt "abc" with
    | Error msg -> Assert.Contains("Can't parse", msg)
    | Ok _ -> Assert.Fail("Expected Error")

// ============================================
// TV番組パーサーのテスト（Result版）
// ============================================

[<Fact>]
let ``extractName は番組名を抽出する`` () =
    Assert.Equal(Ok "Breaking Bad", extractName "Breaking Bad (2008-2013)")

[<Fact>]
let ``extractName は無効な形式でエラーメッセージを返す`` () =
    match extractName "(2008-2013)" with
    | Error msg -> Assert.Contains("Can't extract name", msg)
    | Ok _ -> Assert.Fail("Expected Error")

[<Fact>]
let ``extractYearStart は開始年を抽出する`` () =
    Assert.Equal(Ok 2008, extractYearStart "Breaking Bad (2008-2013)")

[<Fact>]
let ``extractYearStart は無効な形式でエラーメッセージを返す`` () =
    match extractYearStart "Chernobyl (2019)" with
    | Error msg -> Assert.Contains("Can't extract start year", msg)
    | Ok _ -> Assert.Fail("Expected Error")

[<Fact>]
let ``parseShow は正常な形式をパースする`` () =
    match parseShow "Breaking Bad (2008-2013)" with
    | Ok show ->
        Assert.Equal("Breaking Bad", show.Title)
        Assert.Equal(2008, show.Start)
        Assert.Equal(2013, show.End)
    | Error _ -> Assert.Fail("Expected Ok")

[<Fact>]
let ``parseShow は単年の番組をパースする`` () =
    match parseShow "Chernobyl (2019)" with
    | Ok show ->
        Assert.Equal("Chernobyl", show.Title)
        Assert.Equal(2019, show.Start)
        Assert.Equal(2019, show.End)
    | Error _ -> Assert.Fail("Expected Ok")

[<Fact>]
let ``parseShow は無効な形式でエラーメッセージを返す`` () =
    match parseShow "(2008-2013)" with
    | Error msg -> Assert.Contains("Can't extract name", msg)
    | Ok _ -> Assert.Fail("Expected Error")

// ============================================
// バリデーションのテスト
// ============================================

[<Fact>]
let ``validateAge は有効な年齢を受け入れる`` () =
    Assert.Equal(Ok 25, validateAge 25)
    Assert.Equal(Ok 0, validateAge 0)
    Assert.Equal(Ok 150, validateAge 150)

[<Fact>]
let ``validateAge は負の年齢を拒否する`` () =
    match validateAge -5 with
    | Error msg -> Assert.Equal("Age cannot be negative", msg)
    | Ok _ -> Assert.Fail("Expected Error")

[<Fact>]
let ``validateAge は150歳より大きい年齢を拒否する`` () =
    match validateAge 200 with
    | Error msg -> Assert.Equal("Age cannot be greater than 150", msg)
    | Ok _ -> Assert.Fail("Expected Error")

[<Fact>]
let ``validateName は有効な名前を受け入れる`` () =
    Assert.Equal(Ok "Alice", validateName "Alice")

[<Fact>]
let ``validateName は空の名前を拒否する`` () =
    match validateName "" with
    | Error msg -> Assert.Equal("Name cannot be empty", msg)
    | Ok _ -> Assert.Fail("Expected Error")

[<Fact>]
let ``validateEmail は有効なメールアドレスを受け入れる`` () =
    Assert.Equal(Ok "test@example.com", validateEmail "test@example.com")

[<Fact>]
let ``validateEmail は @ がないメールアドレスを拒否する`` () =
    match validateEmail "invalid-email" with
    | Error msg -> Assert.Equal("Email must contain @", msg)
    | Ok _ -> Assert.Fail("Expected Error")

// ============================================
// MusicArtists のテスト
// ============================================

[<Fact>]
let ``wasArtistActive は StillActive のアーティストを正しく判定する`` () =
    let metallica = {
        Name = "Metallica"
        Genre = HeavyMetal
        Origin = US
        YearsActive = StillActive 1981
    }

    Assert.True(wasArtistActive metallica 1990 2000)
    Assert.True(wasArtistActive metallica 2000 2020)
    Assert.False(wasArtistActive metallica 1970 1980)

[<Fact>]
let ``wasArtistActive は ActiveBetween のアーティストを正しく判定する`` () =
    let ledZeppelin = {
        Name = "Led Zeppelin"
        Genre = HardRock
        Origin = England
        YearsActive = ActiveBetween (1968, 1980)
    }

    Assert.True(wasArtistActive ledZeppelin 1970 1975)
    Assert.True(wasArtistActive ledZeppelin 1975 1985)
    Assert.False(wasArtistActive ledZeppelin 1981 1990)

[<Fact>]
let ``activeLength は活動期間の長さを計算する`` () =
    let metallica = {
        Name = "Metallica"
        Genre = HeavyMetal
        Origin = US
        YearsActive = StillActive 1981
    }

    let ledZeppelin = {
        Name = "Led Zeppelin"
        Genre = HardRock
        Origin = England
        YearsActive = ActiveBetween (1968, 1980)
    }

    Assert.Equal(43, activeLength metallica 2024)
    Assert.Equal(12, activeLength ledZeppelin 2024)

[<Fact>]
let ``describeActivity は活動状態を説明する`` () =
    Assert.Equal("Active since 1981", describeActivity (StillActive 1981))
    Assert.Equal("Active from 1968 to 1980", describeActivity (ActiveBetween (1968, 1980)))

// ============================================
// 検索条件のテスト
// ============================================

[<Fact>]
let ``searchArtists はジャンルでフィルタする`` () =
    let artists = [
        { Name = "Metallica"; Genre = HeavyMetal; Origin = US; YearsActive = StillActive 1981 }
        { Name = "Beatles"; Genre = Pop; Origin = UK; YearsActive = ActiveBetween (1960, 1970) }
    ]

    let result = searchArtists artists [SearchByGenre [HeavyMetal]]

    Assert.Single(result) |> ignore
    Assert.Equal("Metallica", result.[0].Name)

[<Fact>]
let ``searchArtists は複数の条件でフィルタする`` () =
    let artists = [
        { Name = "Metallica"; Genre = HeavyMetal; Origin = US; YearsActive = StillActive 1981 }
        { Name = "Iron Maiden"; Genre = HeavyMetal; Origin = UK; YearsActive = StillActive 1975 }
        { Name = "Beatles"; Genre = Pop; Origin = UK; YearsActive = ActiveBetween (1960, 1970) }
    ]

    let conditions = [
        SearchByGenre [HeavyMetal]
        SearchByOrigin [UK]
    ]

    let result = searchArtists artists conditions

    Assert.Single(result) |> ignore
    Assert.Equal("Iron Maiden", result.[0].Name)

[<Fact>]
let ``searchArtists は活動期間でフィルタする`` () =
    let artists = [
        { Name = "Metallica"; Genre = HeavyMetal; Origin = US; YearsActive = StillActive 1981 }
        { Name = "Beatles"; Genre = Pop; Origin = UK; YearsActive = ActiveBetween (1960, 1970) }
    ]

    let result = searchArtists artists [SearchByActiveYears (1965, 1975)]

    // Beatles は 1960-1970 で活動、Metallica は 1981- なので Beatles のみ
    Assert.Single(result) |> ignore
    Assert.Equal("Beatles", result.[0].Name)

// ============================================
// PaymentMethods のテスト
// ============================================

[<Fact>]
let ``describePayment はクレジットカードを説明する`` () =
    let payment = CreditCard ("1234", "12/25")
    Assert.Equal("Credit card ending in 1234", describePayment payment)

[<Fact>]
let ``describePayment は銀行振込を説明する`` () =
    let payment = BankTransfer "9876"
    Assert.Equal("Bank transfer to account 9876", describePayment payment)

[<Fact>]
let ``describePayment は現金を説明する`` () =
    Assert.Equal("Cash payment", describePayment Cash)

[<Fact>]
let ``isValidPayment はクレジットカードを検証する`` () =
    Assert.True(isValidPayment (CreditCard ("1234", "12/25")))
    Assert.False(isValidPayment (CreditCard ("123", "12/25")))
    Assert.False(isValidPayment (CreditCard ("1234", "1225")))

[<Fact>]
let ``isValidPayment は銀行振込を検証する`` () =
    Assert.True(isValidPayment (BankTransfer "12345"))
    Assert.False(isValidPayment (BankTransfer ""))

[<Fact>]
let ``isValidPayment は現金を常に有効とする`` () =
    Assert.True(isValidPayment Cash)
