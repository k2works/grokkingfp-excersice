using Ch02;
using LanguageExt;
using static LanguageExt.Prelude;
using Xunit;

namespace GrokkingFP.CSharp.Tests.Ch02;

/// <summary>
/// 第2章: 純粋関数のテスト
/// </summary>
public class PureFunctionsTests
{
    // ============================================
    // 基本的な純粋関数のテスト
    // ============================================

    [Fact]
    public void Increment_AddsOneToNumber()
    {
        Assert.Equal(7, PureFunctions.Increment(6));
        Assert.Equal(1, PureFunctions.Increment(0));
        Assert.Equal(-5, PureFunctions.Increment(-6));
        Assert.Equal(int.MaxValue, PureFunctions.Increment(int.MaxValue - 1));
    }

    [Fact]
    public void Add_ReturnsSumOfTwoNumbers()
    {
        Assert.Equal(5, PureFunctions.Add(2, 3));
        Assert.Equal(0, PureFunctions.Add(0, 0));
        Assert.Equal(-5, PureFunctions.Add(-2, -3));
    }

    [Fact]
    public void Double_ReturnsDoubledValue()
    {
        Assert.Equal(4, PureFunctions.Double(2));
        Assert.Equal(0, PureFunctions.Double(0));
        Assert.Equal(-8, PureFunctions.Double(-4));
    }

    [Fact]
    public void IsEven_ReturnsTrueForEvenNumbers()
    {
        Assert.True(PureFunctions.IsEven(0));
        Assert.True(PureFunctions.IsEven(2));
        Assert.True(PureFunctions.IsEven(-2));
        Assert.True(PureFunctions.IsEven(int.MinValue));
    }

    [Fact]
    public void IsEven_ReturnsFalseForOddNumbers()
    {
        Assert.False(PureFunctions.IsEven(1));
        Assert.False(PureFunctions.IsEven(-1));
        Assert.False(PureFunctions.IsEven(int.MaxValue));
    }

    // ============================================
    // 文字列操作のテスト
    // ============================================

    [Fact]
    public void ToUpperCase_ConvertsToUpperCase()
    {
        Assert.Equal("HELLO", PureFunctions.ToUpperCase("hello"));
        Assert.Equal("HELLO", PureFunctions.ToUpperCase("Hello"));
        Assert.Equal("", PureFunctions.ToUpperCase(""));
    }

    [Fact]
    public void Reverse_ReversesString()
    {
        Assert.Equal("olleh", PureFunctions.Reverse("hello"));
        Assert.Equal("", PureFunctions.Reverse(""));
        Assert.Equal("a", PureFunctions.Reverse("a"));
    }

    [Fact]
    public void WordScore_ExcludesLetterA()
    {
        Assert.Equal(3, PureFunctions.WordScore("Scala")); // "Scl" → 3
        Assert.Equal(8, PureFunctions.WordScore("function")); // no 'a' → 8
        Assert.Equal(0, PureFunctions.WordScore("")); // empty → 0
        Assert.Equal(0, PureFunctions.WordScore("aaa")); // all 'a' → 0
    }

    [Fact]
    public void WordScoreIgnoreCase_ExcludesLetterABothCases()
    {
        Assert.Equal(3, PureFunctions.WordScoreIgnoreCase("Scala")); // "Scl" → 3
        Assert.Equal(3, PureFunctions.WordScoreIgnoreCase("SCALA")); // "SCL" → 3
        Assert.Equal(0, PureFunctions.WordScoreIgnoreCase("AaAa")); // all A/a → 0
    }

    // ============================================
    // Option を使った安全な操作のテスト
    // ============================================

    [Fact]
    public void SafeDivide_ReturnsSomeForValidDivision()
    {
        var result = PureFunctions.SafeDivide(10, 2);
        Assert.True(result.IsSome);
        Assert.Equal(5, result.Match(x => x, () => 0));
    }

    [Fact]
    public void SafeDivide_ReturnsNoneForDivisionByZero()
    {
        Assert.True(PureFunctions.SafeDivide(10, 0).IsNone);
        Assert.True(PureFunctions.SafeDivide(0, 0).IsNone);
    }

    [Fact]
    public void SafeSqrt_ReturnsSomeForNonNegative()
    {
        Assert.Equal(2.0, PureFunctions.SafeSqrt(4).Match(x => x, () => 0));
        Assert.Equal(0.0, PureFunctions.SafeSqrt(0).Match(x => x, () => -1));
    }

    [Fact]
    public void SafeSqrt_ReturnsNoneForNegative()
    {
        Assert.True(PureFunctions.SafeSqrt(-1).IsNone);
    }

    [Fact]
    public void ParseInt_ReturnsSomeForValidNumber()
    {
        Assert.Equal(42, PureFunctions.ParseInt("42").Match(x => x, () => 0));
        Assert.Equal(-5, PureFunctions.ParseInt("-5").Match(x => x, () => 0));
    }

    [Fact]
    public void ParseInt_ReturnsNoneForInvalidString()
    {
        Assert.True(PureFunctions.ParseInt("abc").IsNone);
        Assert.True(PureFunctions.ParseInt("").IsNone);
        Assert.True(PureFunctions.ParseInt("12.5").IsNone);
    }

    [Fact]
    public void SafeHead_ReturnsSomeForNonEmptySeq()
    {
        var seq = Seq(1, 2, 3);
        Assert.Equal(1, PureFunctions.SafeHead(seq).Match(x => x, () => 0));
    }

    [Fact]
    public void SafeHead_ReturnsNoneForEmptySeq()
    {
        Assert.True(PureFunctions.SafeHead(Seq<int>()).IsNone);
    }
}

/// <summary>
/// ショッピングカートのテスト
/// </summary>
public class ShoppingCartTests
{
    [Fact]
    public void GetDiscountPercentage_Returns5WhenBookExists()
    {
        var items = Seq("Apple", "Book", "Pen");
        Assert.Equal(5, ShoppingCart.GetDiscountPercentage(items));
    }

    [Fact]
    public void GetDiscountPercentage_Returns0WhenNoBook()
    {
        var items = Seq("Apple", "Pen");
        Assert.Equal(0, ShoppingCart.GetDiscountPercentage(items));
    }

    [Fact]
    public void GetDiscountPercentage_Returns0ForEmptyCart()
    {
        Assert.Equal(0, ShoppingCart.GetDiscountPercentage(Seq<string>()));
    }

    [Fact]
    public void AddItem_ReturnsNewCartWithItem()
    {
        var cart = Seq("Apple");
        var newCart = ShoppingCart.AddItem(cart, "Book");

        Assert.Equal(2, newCart.Count);
        Assert.Contains("Book", newCart);
        Assert.Equal(1, cart.Count); // 元のカートは変更されない
    }

    [Fact]
    public void RemoveItem_ReturnsNewCartWithoutItem()
    {
        var cart = Seq("Apple", "Book", "Pen");
        var newCart = ShoppingCart.RemoveItem(cart, "Book");

        Assert.Equal(2, newCart.Count);
        Assert.DoesNotContain("Book", newCart);
        Assert.Equal(3, cart.Count); // 元のカートは変更されない
    }

    [Fact]
    public void GetAdvancedDiscountPercentage_Returns15ForBookAndElectronics()
    {
        var items = Seq("Book", "Laptop");
        Assert.Equal(15, ShoppingCart.GetAdvancedDiscountPercentage(items));
    }

    [Fact]
    public void GetAdvancedDiscountPercentage_Returns10ForBookAndFiveOrMoreItems()
    {
        var items = Seq("Book", "A", "B", "C", "D");
        Assert.Equal(10, ShoppingCart.GetAdvancedDiscountPercentage(items));
    }

    [Fact]
    public void GetAdvancedDiscountPercentage_Returns7ForElectronicsAndThreeOrMoreItems()
    {
        var items = Seq("Phone", "A", "B");
        Assert.Equal(7, ShoppingCart.GetAdvancedDiscountPercentage(items));
    }

    [Fact]
    public void GetAdvancedDiscountPercentage_Returns5ForTenOrMoreItems()
    {
        var items = Seq("A", "B", "C", "D", "E", "F", "G", "H", "I", "J");
        Assert.Equal(5, ShoppingCart.GetAdvancedDiscountPercentage(items));
    }

    [Fact]
    public void CalculateTotal_ReturnsSumOfPrices()
    {
        var items = Seq(("Apple", 100m), ("Book", 500m), ("Pen", 50m));
        Assert.Equal(650m, ShoppingCart.CalculateTotal(items));
    }

    [Fact]
    public void CalculateTotalWithDiscount_AppliesDiscount()
    {
        var items = Seq(("Apple", 100m), ("Book", 500m), ("Pen", 50m));
        // Book があるので 5% 割引: 650 * 0.95 = 617.5
        Assert.Equal(617.5m, ShoppingCart.CalculateTotalWithDiscount(items));
    }
}

/// <summary>
/// チップ計算のテスト
/// </summary>
public class TipCalculatorTests
{
    [Fact]
    public void GetTipPercentage_Returns20ForSixOrMorePeople()
    {
        var names = Seq("A", "B", "C", "D", "E", "F");
        Assert.Equal(20, TipCalculator.GetTipPercentage(names));
    }

    [Fact]
    public void GetTipPercentage_Returns10ForOneToFivePeople()
    {
        Assert.Equal(10, TipCalculator.GetTipPercentage(Seq("A")));
        Assert.Equal(10, TipCalculator.GetTipPercentage(Seq("A", "B", "C", "D", "E")));
    }

    [Fact]
    public void GetTipPercentage_Returns0ForNoPeople()
    {
        Assert.Equal(0, TipCalculator.GetTipPercentage(Seq<string>()));
    }

    [Fact]
    public void GetTipPercentageMatch_ReturnsCorrectPercentage()
    {
        Assert.Equal(20, TipCalculator.GetTipPercentageMatch(Seq("A", "B", "C", "D", "E", "F")));
        Assert.Equal(10, TipCalculator.GetTipPercentageMatch(Seq("A", "B")));
        Assert.Equal(0, TipCalculator.GetTipPercentageMatch(Seq<string>()));
    }

    [Fact]
    public void CalculateTip_ReturnsCorrectTipAmount()
    {
        // 100 * 10% = 10
        Assert.Equal(10m, TipCalculator.CalculateTip(100m, Seq("A", "B")));
        // 100 * 20% = 20
        Assert.Equal(20m, TipCalculator.CalculateTip(100m, Seq("A", "B", "C", "D", "E", "F")));
    }

    [Fact]
    public void CalculatePerPerson_ReturnsSomeForNonEmptyGroup()
    {
        // bill: 100, tip: 10%, total: 110, per person: 55
        var result = TipCalculator.CalculatePerPerson(100m, Seq("A", "B"));
        Assert.True(result.IsSome);
        Assert.Equal(55m, result.Match(x => x, () => 0));
    }

    [Fact]
    public void CalculatePerPerson_ReturnsNoneForEmptyGroup()
    {
        var result = TipCalculator.CalculatePerPerson(100m, Seq<string>());
        Assert.True(result.IsNone);
    }

    [Fact]
    public void GenerateBillSummary_ReturnsCorrectSummary()
    {
        var (bill, tip, total, perPerson) =
            TipCalculator.GenerateBillSummary(100m, Seq("A", "B"));

        Assert.Equal(100m, bill);
        Assert.Equal(10m, tip);
        Assert.Equal(110m, total);
        Assert.Equal(55m, perPerson.Match(x => x, () => 0));
    }
}
