using Ch07;
using LanguageExt;
using static LanguageExt.Prelude;
using Xunit;
using static Ch07.ResultHandling;
using static Ch07.MusicArtists;
using static Ch07.PaymentMethods;

namespace GrokkingFP.CSharp.Tests.Ch07;

/// <summary>
/// 第7章: Either/Result 型のテスト
/// </summary>
public class ResultHandlingTests
{
    // ============================================
    // TryParseInt のテスト
    // ============================================

    [Fact]
    public void TryParseInt_ReturnsRightForValidInteger()
    {
        var result = ResultHandling.TryParseInt("42");
        Assert.True(result.IsRight);
        Assert.Equal(42, result.Match(Left: _ => 0, Right: r => r));
    }

    [Fact]
    public void TryParseInt_ReturnsLeftForInvalidInput()
    {
        var result = ResultHandling.TryParseInt("abc");
        Assert.True(result.IsLeft);
        Assert.Contains("Can't parse", result.Match(Left: l => l, Right: _ => ""));
    }

    // ============================================
    // TV番組パーサーのテスト（Either版）
    // ============================================

    [Fact]
    public void ExtractName_ReturnsRightForValidFormat()
    {
        var result = ExtractName("Breaking Bad (2008-2013)");
        Assert.True(result.IsRight);
        Assert.Equal("Breaking Bad", result.Match(Left: _ => "", Right: r => r));
    }

    [Fact]
    public void ExtractName_ReturnsLeftWithErrorMessage()
    {
        var result = ExtractName("No Brackets");
        Assert.True(result.IsLeft);
        Assert.Contains("Can't extract name", result.Match(Left: l => l, Right: _ => ""));
    }

    [Fact]
    public void ExtractYearStart_ReturnsRightForYearRange()
    {
        var result = ExtractYearStart("Breaking Bad (2008-2013)");
        Assert.True(result.IsRight);
        Assert.Equal(2008, result.Match(Left: _ => 0, Right: r => r));
    }

    [Fact]
    public void ExtractYearEnd_ReturnsRightForYearRange()
    {
        var result = ExtractYearEnd("Breaking Bad (2008-2013)");
        Assert.True(result.IsRight);
        Assert.Equal(2013, result.Match(Left: _ => 0, Right: r => r));
    }

    [Fact]
    public void ExtractSingleYear_ReturnsRightForSingleYear()
    {
        var result = ExtractSingleYear("Chernobyl (2019)");
        Assert.True(result.IsRight);
        Assert.Equal(2019, result.Match(Left: _ => 0, Right: r => r));
    }

    [Fact]
    public void ParseShow_ParsesYearRangeWithEither()
    {
        var result = ParseShow("Breaking Bad (2008-2013)");
        Assert.True(result.IsRight);
        var show = result.Match(Left: _ => (TvShow?)null, Right: s => s);
        Assert.NotNull(show);
        Assert.Equal("Breaking Bad", show!.Title);
        Assert.Equal(2008, show.Start);
        Assert.Equal(2013, show.End);
    }

    [Fact]
    public void ParseShow_ParsesSingleYearWithEither()
    {
        var result = ParseShow("Chernobyl (2019)");
        Assert.True(result.IsRight);
        var show = result.Match(Left: _ => (TvShow?)null, Right: s => s);
        Assert.NotNull(show);
        Assert.Equal("Chernobyl", show!.Title);
        Assert.Equal(2019, show.Start);
        Assert.Equal(2019, show.End);
    }

    [Fact]
    public void ParseShow_ReturnsLeftWithErrorForInvalidFormat()
    {
        var result = ParseShow("Invalid");
        Assert.True(result.IsLeft);
    }

    // ============================================
    // OrElse のテスト
    // ============================================

    [Fact]
    public void OrElse_ReturnsFirstWhenRight()
    {
        var first = Right<string, int>(42);
        var alternative = Right<string, int>(100);
        Assert.Equal(42, first.OrElse(alternative).Match(Left: _ => 0, Right: r => r));
    }

    [Fact]
    public void OrElse_ReturnsAlternativeWhenLeft()
    {
        var first = Left<string, int>("error");
        var alternative = Right<string, int>(100);
        Assert.Equal(100, first.OrElse(alternative).Match(Left: _ => 0, Right: r => r));
    }

    // ============================================
    // バリデーションのテスト
    // ============================================

    [Fact]
    public void ValidateAge_ReturnsRightForValidAge()
    {
        Assert.True(ValidateAge(25).IsRight);
        Assert.True(ValidateAge(0).IsRight);
        Assert.True(ValidateAge(150).IsRight);
    }

    [Fact]
    public void ValidateAge_ReturnsLeftForInvalidAge()
    {
        var negativeResult = ValidateAge(-1);
        Assert.True(negativeResult.IsLeft);
        Assert.Contains("negative", negativeResult.Match(Left: l => l, Right: _ => ""));

        var tooOldResult = ValidateAge(151);
        Assert.True(tooOldResult.IsLeft);
        Assert.Contains("150", tooOldResult.Match(Left: l => l, Right: _ => ""));
    }

    [Fact]
    public void ValidateName_ReturnsRightForValidName()
    {
        Assert.True(ValidateName("Alice").IsRight);
        Assert.True(ValidateName("Bob").IsRight);
    }

    [Fact]
    public void ValidateName_ReturnsLeftForInvalidName()
    {
        var emptyResult = ValidateName("");
        Assert.True(emptyResult.IsLeft);
        Assert.Contains("empty", emptyResult.Match(Left: l => l, Right: _ => ""));

        var tooLongResult = ValidateName(new string('a', 101));
        Assert.True(tooLongResult.IsLeft);
        Assert.Contains("100", tooLongResult.Match(Left: l => l, Right: _ => ""));
    }

    [Fact]
    public void ValidateEmail_ReturnsRightForValidEmail()
    {
        Assert.True(ValidateEmail("test@example.com").IsRight);
    }

    [Fact]
    public void ValidateEmail_ReturnsLeftForInvalidEmail()
    {
        var emptyResult = ValidateEmail("");
        Assert.True(emptyResult.IsLeft);

        var noAtResult = ValidateEmail("invalid.email");
        Assert.True(noAtResult.IsLeft);
        Assert.Contains("@", noAtResult.Match(Left: l => l, Right: _ => ""));
    }
}

/// <summary>
/// 音楽アーティストのテスト
/// </summary>
public class MusicArtistsTests
{
    private readonly Artist _metallica = new Artist(
        "Metallica",
        MusicGenre.HeavyMetal,
        new US(),
        new StillActive(1981)
    );

    private readonly Artist _beatles = new Artist(
        "The Beatles",
        MusicGenre.Pop,
        new UK(),
        new ActiveBetween(1960, 1970)
    );

    // ============================================
    // パターンマッチングのテスト
    // ============================================

    [Fact]
    public void WasArtistActive_ChecksStillActiveArtist()
    {
        Assert.True(WasArtistActive(_metallica, 2000, 2020));
        Assert.True(WasArtistActive(_metallica, 1980, 1985));
        Assert.False(WasArtistActive(_metallica, 1970, 1980));
    }

    [Fact]
    public void WasArtistActive_ChecksActiveBetweenArtist()
    {
        Assert.True(WasArtistActive(_beatles, 1965, 1968));
        Assert.True(WasArtistActive(_beatles, 1955, 1965));
        Assert.False(WasArtistActive(_beatles, 1971, 1980));
    }

    [Fact]
    public void ActiveLength_CalculatesForStillActive()
    {
        Assert.Equal(43, ActiveLength(_metallica, 2024));
    }

    [Fact]
    public void ActiveLength_CalculatesForActiveBetween()
    {
        Assert.Equal(10, ActiveLength(_beatles, 2024));
    }

    [Fact]
    public void DescribeActivity_DescribesStillActive()
    {
        var description = DescribeActivity(new StillActive(1981));
        Assert.Equal("Active since 1981", description);
    }

    [Fact]
    public void DescribeActivity_DescribesActiveBetween()
    {
        var description = DescribeActivity(new ActiveBetween(1960, 1970));
        Assert.Equal("Active from 1960 to 1970", description);
    }

    // ============================================
    // 検索のテスト
    // ============================================

    [Fact]
    public void MatchesCondition_MatchesByGenre()
    {
        var condition = new SearchByGenre(Seq(MusicGenre.HeavyMetal, MusicGenre.HardRock));
        Assert.True(MatchesCondition(_metallica, condition));
        Assert.False(MatchesCondition(_beatles, condition));
    }

    [Fact]
    public void MatchesCondition_MatchesByOrigin()
    {
        var condition = new SearchByOrigin(Seq<Location>(new US()));
        Assert.True(MatchesCondition(_metallica, condition));
        Assert.False(MatchesCondition(_beatles, condition));
    }

    [Fact]
    public void MatchesCondition_MatchesByActiveYears()
    {
        var condition = new SearchByActiveYears(1965, 1968);
        Assert.True(MatchesCondition(_beatles, condition));
        Assert.False(MatchesCondition(_metallica, condition));
    }

    [Fact]
    public void SearchArtists_FiltersWithMultipleConditions()
    {
        var artists = Seq(_metallica, _beatles);
        var conditions = Seq<SearchCondition>(
            new SearchByGenre(Seq(MusicGenre.HeavyMetal)),
            new SearchByOrigin(Seq<Location>(new US()))
        );
        var result = SearchArtists(artists, conditions);
        Assert.Equal(1, result.Count);
        Assert.Equal("Metallica", result.ToList().First().Name);
    }
}

/// <summary>
/// 支払い方法のテスト
/// </summary>
public class PaymentMethodsTests
{
    [Fact]
    public void DescribePayment_DescribesCreditCard()
    {
        var card = new CreditCard("1234567890123456", "12/25");
        var description = DescribePayment(card);
        Assert.Contains("Credit card", description);
        Assert.Contains("1234567890123456", description);
    }

    [Fact]
    public void DescribePayment_DescribesBankTransfer()
    {
        var transfer = new BankTransfer("123-456-789");
        var description = DescribePayment(transfer);
        Assert.Contains("Bank transfer", description);
        Assert.Contains("123-456-789", description);
    }

    [Fact]
    public void DescribePayment_DescribesCash()
    {
        var cash = new Cash();
        var description = DescribePayment(cash);
        Assert.Equal("Cash payment", description);
    }

    [Fact]
    public void IsValidPayment_ValidatesCreditCard()
    {
        Assert.True(IsValidPayment(new CreditCard("1234", "12/25")));
        Assert.False(IsValidPayment(new CreditCard("123", "12/25"))); // too short
        Assert.False(IsValidPayment(new CreditCard("1234", "1225"))); // no slash
    }

    [Fact]
    public void IsValidPayment_ValidatesBankTransfer()
    {
        Assert.True(IsValidPayment(new BankTransfer("123")));
        Assert.False(IsValidPayment(new BankTransfer("")));
    }

    [Fact]
    public void IsValidPayment_ValidatesCash()
    {
        Assert.True(IsValidPayment(new Cash()));
    }
}
