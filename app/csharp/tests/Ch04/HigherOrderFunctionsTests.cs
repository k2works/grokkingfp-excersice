using Ch04;
using LanguageExt;
using static LanguageExt.Prelude;
using Xunit;

namespace GrokkingFP.CSharp.Tests.Ch04;

/// <summary>
/// 第4章: 高階関数のテスト
/// </summary>
public class HigherOrderFunctionsTests
{
    // ============================================
    // 基本的な高階関数のテスト
    // ============================================

    [Fact]
    public void WordScore_ExcludesLetterA()
    {
        Assert.Equal(3, HigherOrderFunctions.WordScore("Scala")); // "Scl" → 3
        Assert.Equal(8, HigherOrderFunctions.WordScore("function")); // no 'a' → 8
        Assert.Equal(0, HigherOrderFunctions.WordScore("aaa"));
    }

    [Fact]
    public void Bonus_Returns5WhenContainsC()
    {
        Assert.Equal(5, HigherOrderFunctions.Bonus("Scala"));
        Assert.Equal(0, HigherOrderFunctions.Bonus("Java"));
    }

    [Fact]
    public void Penalty_Returns7WhenContainsS()
    {
        Assert.Equal(7, HigherOrderFunctions.Penalty("Rust")); // lowercase 's'
        Assert.Equal(0, HigherOrderFunctions.Penalty("Java"));
    }

    [Fact]
    public void IsOdd_ReturnsTrueForOddNumbers()
    {
        Assert.True(HigherOrderFunctions.IsOdd(1));
        Assert.True(HigherOrderFunctions.IsOdd(3));
        Assert.False(HigherOrderFunctions.IsOdd(2));
    }

    [Fact]
    public void IsEven_ReturnsTrueForEvenNumbers()
    {
        Assert.True(HigherOrderFunctions.IsEven(0));
        Assert.True(HigherOrderFunctions.IsEven(2));
        Assert.False(HigherOrderFunctions.IsEven(1));
    }

    // ============================================
    // SortBy のテスト
    // ============================================

    [Fact]
    public void SortByScore_SortsAscending()
    {
        var words = Seq("ada", "haskell", "scala", "java");
        var sorted = HigherOrderFunctions.SortByScore(words);
        Assert.Equal(Seq("ada", "java", "scala", "haskell"), sorted);
    }

    [Fact]
    public void SortByScoreDescending_SortsDescending()
    {
        var words = Seq("ada", "haskell", "scala", "java");
        var sorted = HigherOrderFunctions.SortByScoreDescending(words);
        Assert.Equal(Seq("haskell", "scala", "java", "ada"), sorted);
    }

    // ============================================
    // Map のテスト
    // ============================================

    [Fact]
    public void Lengths_ReturnsLengthOfEachString()
    {
        var strings = Seq("hello", "world", "!");
        Assert.Equal(Seq(5, 5, 1), HigherOrderFunctions.Lengths(strings));
    }

    [Fact]
    public void Doubles_DoublesEachNumber()
    {
        var numbers = Seq(1, 2, 3, 4);
        Assert.Equal(Seq(2, 4, 6, 8), HigherOrderFunctions.Doubles(numbers));
    }

    // ============================================
    // Filter のテスト
    // ============================================

    [Fact]
    public void FilterOdds_ReturnsOnlyOddNumbers()
    {
        var numbers = Seq(1, 2, 3, 4, 5, 6);
        Assert.Equal(Seq(1, 3, 5), HigherOrderFunctions.FilterOdds(numbers));
    }

    [Fact]
    public void FilterLargerThan_ReturnsNumbersLargerThanN()
    {
        var numbers = Seq(1, 2, 3, 4, 5);
        Assert.Equal(Seq(4, 5), HigherOrderFunctions.FilterLargerThan(numbers, 3));
    }

    // ============================================
    // Fold のテスト
    // ============================================

    [Fact]
    public void Sum_ReturnsSumOfNumbers()
    {
        Assert.Equal(15, HigherOrderFunctions.Sum(Seq(1, 2, 3, 4, 5)));
        Assert.Equal(0, HigherOrderFunctions.Sum(Seq<int>()));
    }

    [Fact]
    public void Maximum_ReturnsMaximumNumber()
    {
        Assert.Equal(5, HigherOrderFunctions.Maximum(Seq(1, 5, 3, 2, 4)));
        Assert.Equal(-1, HigherOrderFunctions.Maximum(Seq(-5, -3, -1, -4)));
    }

    [Fact]
    public void CountWhere_CountsMatchingElements()
    {
        var numbers = Seq(1, 2, 3, 4, 5, 6);
        Assert.Equal(3, HigherOrderFunctions.CountWhere(numbers, HigherOrderFunctions.IsEven));
    }

    // ============================================
    // 関数を返す関数のテスト
    // ============================================

    [Fact]
    public void LargerThan_ReturnsPredicateFunction()
    {
        var largerThan5 = HigherOrderFunctions.LargerThan(5);
        Assert.True(largerThan5(6));
        Assert.False(largerThan5(5));
        Assert.False(largerThan5(4));
    }

    [Fact]
    public void DivisibleBy_ReturnsPredicateFunction()
    {
        var divisibleBy3 = HigherOrderFunctions.DivisibleBy(3);
        Assert.True(divisibleBy3(9));
        Assert.True(divisibleBy3(0));
        Assert.False(divisibleBy3(5));
    }

    [Fact]
    public void ContainsChar_ReturnsPredicateFunction()
    {
        var containsA = HigherOrderFunctions.ContainsChar('a');
        Assert.True(containsA("Scala"));
        Assert.False(containsA("PHP"));
    }

    // ============================================
    // ワードランキングのテスト
    // ============================================

    [Fact]
    public void RankByScore_RanksByBasicScore()
    {
        var words = Seq("ada", "haskell", "scala", "java");
        var ranked = HigherOrderFunctions.RankByScore(words);
        Assert.Equal("haskell", ranked.Head);
    }

    [Fact]
    public void RankByScoreWithBonus_IncludesBonus()
    {
        var words = Seq("ada", "haskell", "scala", "java");
        var ranked = HigherOrderFunctions.RankByScoreWithBonus(words);
        // scala: 3 + 5 = 8, haskell: 7 + 0 = 7
        Assert.Equal("scala", ranked.Head);
    }

    [Fact]
    public void RankByScoreWithBonusAndPenalty_IncludesBonusAndPenalty()
    {
        var words = Seq("ada", "haskell", "scala", "java");
        var ranked = HigherOrderFunctions.RankByScoreWithBonusAndPenalty(words);
        // haskell: 7 + 0 - 7 = 0
        // scala: 3 + 5 - 7 = 1
        // java: 2 + 0 - 0 = 2
        // ada: 0 + 0 - 0 = 0
        Assert.Equal("java", ranked.Head);
    }
}

/// <summary>
/// プログラミング言語のテスト
/// </summary>
public class ProgrammingLanguagesTests
{
    private readonly Seq<ProgrammingLanguages.ProgrammingLanguage> _languages = Seq(
        new ProgrammingLanguages.ProgrammingLanguage("Scala", 2004),
        new ProgrammingLanguages.ProgrammingLanguage("Haskell", 1990),
        new ProgrammingLanguages.ProgrammingLanguage("Java", 1995),
        new ProgrammingLanguages.ProgrammingLanguage("Rust", 2010)
    );

    [Fact]
    public void FilterByYear_ReturnsLanguagesAfterYear()
    {
        var filtered = ProgrammingLanguages.FilterByYear(_languages, 2000);
        Assert.Equal(2, filtered.Count);
        Assert.Contains(filtered, l => l.Name == "Scala");
        Assert.Contains(filtered, l => l.Name == "Rust");
    }

    [Fact]
    public void GetNames_ReturnsAllNames()
    {
        var names = ProgrammingLanguages.GetNames(_languages);
        Assert.Equal(Seq("Scala", "Haskell", "Java", "Rust"), names);
    }

    [Fact]
    public void SortByYear_SortsAscending()
    {
        var sorted = ProgrammingLanguages.SortByYear(_languages);
        var sortedList = sorted.ToList();
        Assert.Equal("Haskell", sortedList.First().Name);
        Assert.Equal("Rust", sortedList.Last().Name);
    }
}
