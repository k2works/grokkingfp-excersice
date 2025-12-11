using Ch06;
using LanguageExt;
using static LanguageExt.Prelude;
using Xunit;
using static Ch06.OptionHandling;

namespace GrokkingFP.CSharp.Tests.Ch06;

/// <summary>
/// 第6章: Option 型のテスト
/// </summary>
public class OptionHandlingTests
{
    // ============================================
    // SafeDivide のテスト
    // ============================================

    [Fact]
    public void SafeDivide_ReturnsQuotientWhenDivisorIsNotZero()
    {
        Assert.Equal(Some(5), SafeDivide(10, 2));
        Assert.Equal(Some(3), SafeDivide(9, 3));
    }

    [Fact]
    public void SafeDivide_ReturnsNoneWhenDivisorIsZero()
    {
        Assert.Equal(None, SafeDivide(10, 0));
        Assert.Equal(None, SafeDivide(0, 0));
    }

    // ============================================
    // TryParseInt のテスト
    // ============================================

    [Fact]
    public void TryParseInt_ReturnsSomeForValidInteger()
    {
        Assert.Equal(Some(42), TryParseInt("42"));
        Assert.Equal(Some(-10), TryParseInt("-10"));
        Assert.Equal(Some(0), TryParseInt("0"));
    }

    [Fact]
    public void TryParseInt_ReturnsNoneForInvalidInput()
    {
        Assert.Equal(None, TryParseInt("abc"));
        Assert.Equal(None, TryParseInt(""));
        Assert.Equal(None, TryParseInt("12.5"));
    }

    // ============================================
    // TV番組パーサーのテスト
    // ============================================

    [Fact]
    public void ExtractName_ExtractsNameBeforeBracket()
    {
        Assert.Equal(Some("Breaking Bad"), ExtractName("Breaking Bad (2008-2013)"));
        Assert.Equal(Some("Chernobyl"), ExtractName("Chernobyl (2019)"));
    }

    [Fact]
    public void ExtractName_ReturnsNoneForInvalidFormat()
    {
        Assert.Equal(None, ExtractName("No Brackets"));
        Assert.Equal(None, ExtractName("(2019)"));
    }

    [Fact]
    public void ExtractYearStart_ExtractsStartYear()
    {
        Assert.Equal(Some(2008), ExtractYearStart("Breaking Bad (2008-2013)"));
        Assert.Equal(Some(1990), ExtractYearStart("The Simpsons (1990-2023)"));
    }

    [Fact]
    public void ExtractYearStart_ReturnsNoneForSingleYear()
    {
        Assert.Equal(None, ExtractYearStart("Chernobyl (2019)"));
    }

    [Fact]
    public void ExtractYearEnd_ExtractsEndYear()
    {
        Assert.Equal(Some(2013), ExtractYearEnd("Breaking Bad (2008-2013)"));
        Assert.Equal(Some(2023), ExtractYearEnd("The Simpsons (1990-2023)"));
    }

    [Fact]
    public void ExtractYearEnd_ReturnsNoneForSingleYear()
    {
        Assert.Equal(None, ExtractYearEnd("Chernobyl (2019)"));
    }

    [Fact]
    public void ExtractSingleYear_ExtractsSingleYear()
    {
        Assert.Equal(Some(2019), ExtractSingleYear("Chernobyl (2019)"));
        Assert.Equal(Some(2021), ExtractSingleYear("Mare of Easttown (2021)"));
    }

    [Fact]
    public void ExtractSingleYear_ReturnsNoneForYearRange()
    {
        Assert.Equal(None, ExtractSingleYear("Breaking Bad (2008-2013)"));
    }

    [Fact]
    public void ParseShow_ParsesYearRange()
    {
        var result = ParseShow("Breaking Bad (2008-2013)");
        Assert.True(result.IsSome);
        var show = result.Match(Some: s => s, None: () => null!);
        Assert.Equal("Breaking Bad", show.Title);
        Assert.Equal(2008, show.Start);
        Assert.Equal(2013, show.End);
    }

    [Fact]
    public void ParseShow_ParsesSingleYear()
    {
        var result = ParseShow("Chernobyl (2019)");
        Assert.True(result.IsSome);
        var show = result.Match(Some: s => s, None: () => null!);
        Assert.Equal("Chernobyl", show.Title);
        Assert.Equal(2019, show.Start);
        Assert.Equal(2019, show.End);
    }

    [Fact]
    public void ParseShow_ReturnsNoneForInvalidFormat()
    {
        Assert.True(ParseShow("Invalid Format").IsNone);
        Assert.True(ParseShow("No Year ()").IsNone);
    }

    // ============================================
    // 複数番組のパース戦略テスト
    // ============================================

    [Fact]
    public void ParseShowsBestEffort_ReturnsOnlySuccessfulParses()
    {
        var rawShows = Seq(
            "Breaking Bad (2008-2013)",
            "Invalid",
            "Chernobyl (2019)"
        );
        var result = ParseShowsBestEffort(rawShows);
        Assert.Equal(2, result.Count);
    }

    [Fact]
    public void ParseShowsAllOrNothing_ReturnsSomeWhenAllSucceed()
    {
        var rawShows = Seq(
            "Breaking Bad (2008-2013)",
            "Chernobyl (2019)"
        );
        var result = ParseShowsAllOrNothing(rawShows);
        Assert.True(result.IsSome);
        var shows = result.Match(Some: s => s, None: () => Seq<TvShow>());
        Assert.Equal(2, shows.Count);
    }

    [Fact]
    public void ParseShowsAllOrNothing_ReturnsNoneWhenAnyFails()
    {
        var rawShows = Seq(
            "Breaking Bad (2008-2013)",
            "Invalid",
            "Chernobyl (2019)"
        );
        var result = ParseShowsAllOrNothing(rawShows);
        Assert.True(result.IsNone);
    }

    // ============================================
    // AddStrings のテスト
    // ============================================

    [Fact]
    public void AddStrings_AddsTwoValidNumbers()
    {
        Assert.Equal(Some(30), AddStrings("10", "20"));
        Assert.Equal(Some(0), AddStrings("-5", "5"));
    }

    [Fact]
    public void AddStrings_ReturnsNoneWhenEitherIsInvalid()
    {
        Assert.Equal(None, AddStrings("abc", "10"));
        Assert.Equal(None, AddStrings("10", "xyz"));
        Assert.Equal(None, AddStrings("abc", "xyz"));
    }

    // ============================================
    // ForAll/Exists のテスト
    // ============================================

    [Fact]
    public void ForAllExample_ReturnsTrueForNone()
    {
        Assert.True(ForAllExample(None, 10));
    }

    [Fact]
    public void ForAllExample_ReturnsPredicateResultForSome()
    {
        Assert.True(ForAllExample(Some(5), 10));
        Assert.False(ForAllExample(Some(15), 10));
    }

    [Fact]
    public void ExistsExample_ReturnsFalseForNone()
    {
        Assert.False(ExistsExample(None, 10));
    }

    [Fact]
    public void ExistsExample_ReturnsPredicateResultForSome()
    {
        Assert.True(ExistsExample(Some(5), 10));
        Assert.False(ExistsExample(Some(15), 10));
    }
}

/// <summary>
/// ユーザーモデルのテスト
/// </summary>
public class UserModelTests
{
    private readonly Seq<UserModel.User> _users = Seq(
        new UserModel.User("Alice", Some("alice@example.com"), 30),
        new UserModel.User("Bob", None, 25),
        new UserModel.User("Charlie", Some("charlie@other.com"), 35)
    );

    [Fact]
    public void FilterByEmailDomain_IncludesUsersWithNoneEmail()
    {
        var result = UserModel.FilterByEmailDomain(_users, "@example.com");
        // Alice (matches domain) + Bob (None = ForAll returns true)
        Assert.Equal(2, result.Count);
    }

    [Fact]
    public void FilterByEmailDomainExists_ExcludesUsersWithNoneEmail()
    {
        var result = UserModel.FilterByEmailDomainExists(_users, "@example.com");
        // Only Alice (Exists requires Some)
        Assert.Equal(1, result.Count);
        Assert.Equal("Alice", result.ToList().First().Name);
    }
}
