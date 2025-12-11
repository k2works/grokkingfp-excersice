using Ch01;
using LanguageExt;
using static LanguageExt.Prelude;
using Xunit;

namespace GrokkingFP.CSharp.Tests.Ch01;

/// <summary>
/// 第1章: 関数型プログラミング入門のテスト
/// </summary>
public class IntroCSharpTests
{
    // ============================================
    // 基本関数のテスト
    // ============================================

    [Fact]
    public void WordScore_ReturnsStringLength()
    {
        Assert.Equal(5, IntroCSharp.WordScore("hello"));
        Assert.Equal(0, IntroCSharp.WordScore(""));
        Assert.Equal(11, IntroCSharp.WordScore("hello world"));
    }

    [Fact]
    public void WordScoreImperative_ReturnsStringLength()
    {
        Assert.Equal(5, IntroCSharp.WordScoreImperative("hello"));
        Assert.Equal(0, IntroCSharp.WordScoreImperative(""));
    }

    [Fact]
    public void Increment_AddsOneToNumber()
    {
        Assert.Equal(7, IntroCSharp.Increment(6));
        Assert.Equal(1, IntroCSharp.Increment(0));
        Assert.Equal(-5, IntroCSharp.Increment(-6));
        Assert.Equal(int.MaxValue, IntroCSharp.Increment(int.MaxValue - 1));
    }

    [Fact]
    public void GetFirstCharacter_ReturnsFirstChar()
    {
        Assert.Equal('h', IntroCSharp.GetFirstCharacter("hello"));
        Assert.Equal('A', IntroCSharp.GetFirstCharacter("ABC"));
    }

    [Fact]
    public void Add_ReturnsSumOfTwoNumbers()
    {
        Assert.Equal(5, IntroCSharp.Add(2, 3));
        Assert.Equal(0, IntroCSharp.Add(-1, 1));
        Assert.Equal(-5, IntroCSharp.Add(-2, -3));
    }

    [Fact]
    public void Double_ReturnsDoubledValue()
    {
        Assert.Equal(4, IntroCSharp.Double(2));
        Assert.Equal(0, IntroCSharp.Double(0));
        Assert.Equal(-8, IntroCSharp.Double(-4));
    }

    [Fact]
    public void IsEven_ReturnsTrueForEvenNumbers()
    {
        Assert.True(IntroCSharp.IsEven(0));
        Assert.True(IntroCSharp.IsEven(2));
        Assert.True(IntroCSharp.IsEven(100));
        Assert.True(IntroCSharp.IsEven(-4));
    }

    [Fact]
    public void IsEven_ReturnsFalseForOddNumbers()
    {
        Assert.False(IntroCSharp.IsEven(1));
        Assert.False(IntroCSharp.IsEven(3));
        Assert.False(IntroCSharp.IsEven(-7));
    }

    // ============================================
    // 変換関数のテスト
    // ============================================

    [Fact]
    public void ScoreUpperCase_ReturnsLengthOfUpperCasedString()
    {
        Assert.Equal(5, IntroCSharp.ScoreUpperCase("hello"));
        Assert.Equal(5, IntroCSharp.ScoreUpperCase("HELLO"));
    }

    [Fact]
    public void ProcessWord_TrimsLowercasesAndRemovesSpaces()
    {
        Assert.Equal("helloworld", IntroCSharp.ProcessWord("  Hello World  "));
        Assert.Equal("test", IntroCSharp.ProcessWord("TEST"));
        Assert.Equal("abc", IntroCSharp.ProcessWord("  A B C  "));
    }

    // ============================================
    // カリー化と部分適用のテスト
    // ============================================

    [Fact]
    public void AddCurried_WorksWithPartialApplication()
    {
        var addThree = IntroCSharp.AddCurried(3);
        Assert.Equal(7, addThree(4));
        Assert.Equal(3, addThree(0));
        Assert.Equal(0, addThree(-3));
    }

    [Fact]
    public void AddFive_AddsFiveToNumber()
    {
        Assert.Equal(8, IntroCSharp.AddFive(3));
        Assert.Equal(5, IntroCSharp.AddFive(0));
        Assert.Equal(0, IntroCSharp.AddFive(-5));
    }

    [Fact]
    public void DoubleIt_DoublesNumber()
    {
        Assert.Equal(10, IntroCSharp.DoubleIt(5));
        Assert.Equal(0, IntroCSharp.DoubleIt(0));
        Assert.Equal(-6, IntroCSharp.DoubleIt(-3));
    }

    // ============================================
    // Seq 操作のテスト
    // ============================================

    [Fact]
    public void DoubleAll_DoublesAllElements()
    {
        var numbers = Seq(1, 2, 3, 4, 5);
        var doubled = IntroCSharp.DoubleAll(numbers);
        Assert.Equal(Seq(2, 4, 6, 8, 10), doubled);
    }

    [Fact]
    public void FilterEvens_ReturnsOnlyEvenNumbers()
    {
        var numbers = Seq(1, 2, 3, 4, 5, 6);
        var evens = IntroCSharp.FilterEvens(numbers);
        Assert.Equal(Seq(2, 4, 6), evens);
    }

    [Fact]
    public void Sum_ReturnsSumOfAllElements()
    {
        Assert.Equal(15, IntroCSharp.Sum(Seq(1, 2, 3, 4, 5)));
        Assert.Equal(0, IntroCSharp.Sum(Seq<int>()));
        Assert.Equal(-6, IntroCSharp.Sum(Seq(-1, -2, -3)));
    }

    // ============================================
    // Option を使った安全な操作のテスト
    // ============================================

    [Fact]
    public void GetFirstCharacterSafe_ReturnsSomeForNonEmptyString()
    {
        var result = IntroCSharp.GetFirstCharacterSafe("hello");
        Assert.True(result.IsSome);
        Assert.Equal('h', result.Match(c => c, () => '\0'));
    }

    [Fact]
    public void GetFirstCharacterSafe_ReturnsNoneForEmptyString()
    {
        Assert.True(IntroCSharp.GetFirstCharacterSafe("").IsNone);
        Assert.True(IntroCSharp.GetFirstCharacterSafe(null!).IsNone);
    }

    [Fact]
    public void SafeDivide_ReturnsSomeForValidDivision()
    {
        var result = IntroCSharp.SafeDivide(10, 2);
        Assert.True(result.IsSome);
        Assert.Equal(5, result.Match(x => x, () => 0));
    }

    [Fact]
    public void SafeDivide_ReturnsNoneForDivisionByZero()
    {
        Assert.True(IntroCSharp.SafeDivide(10, 0).IsNone);
    }

    // ============================================
    // 関数合成のテスト
    // ============================================

    [Fact]
    public void IncrementThenDouble_IncrementsFirst()
    {
        // (3 + 1) * 2 = 8
        Assert.Equal(8, IntroCSharp.IncrementThenDouble(3));
        // (0 + 1) * 2 = 2
        Assert.Equal(2, IntroCSharp.IncrementThenDouble(0));
    }

    [Fact]
    public void DoubleThenIncrement_DoublesFirst()
    {
        // (3 * 2) + 1 = 7
        Assert.Equal(7, IntroCSharp.DoubleThenIncrement(3));
        // (0 * 2) + 1 = 1
        Assert.Equal(1, IntroCSharp.DoubleThenIncrement(0));
    }

    [Fact]
    public void Compose_CreatesNewFunction()
    {
        Func<int, int> addOne = x => x + 1;
        Func<int, string> toString = x => x.ToString();

        var composed = IntroCSharp.Compose(addOne, toString);

        Assert.Equal("6", composed(5));
        Assert.Equal("1", composed(0));
    }
}
