using Ch03;
using LanguageExt;
using static LanguageExt.Prelude;
using Xunit;

namespace GrokkingFP.CSharp.Tests.Ch03;

/// <summary>
/// 第3章: イミュータブルなデータ操作のテスト
/// </summary>
public class ImmutableOperationsTests
{
    // ============================================
    // 基本的なリスト操作のテスト
    // ============================================

    [Fact]
    public void FirstN_ReturnsFirstNElements()
    {
        var seq = Seq(1, 2, 3, 4, 5);
        Assert.Equal(Seq(1, 2, 3), ImmutableOperations.FirstN(seq, 3));
    }

    [Fact]
    public void FirstN_ReturnsAllWhenNIsLarger()
    {
        var seq = Seq(1, 2);
        Assert.Equal(Seq(1, 2), ImmutableOperations.FirstN(seq, 5));
    }

    [Fact]
    public void LastN_ReturnsLastNElements()
    {
        var seq = Seq(1, 2, 3, 4, 5);
        Assert.Equal(Seq(3, 4, 5), ImmutableOperations.LastN(seq, 3));
    }

    [Fact]
    public void LastN_ReturnsAllWhenNIsLarger()
    {
        var seq = Seq(1, 2);
        Assert.Equal(Seq(1, 2), ImmutableOperations.LastN(seq, 5));
    }

    [Fact]
    public void FirstTwo_ReturnsFirstTwoElements()
    {
        var seq = Seq(1, 2, 3, 4, 5);
        Assert.Equal(Seq(1, 2), ImmutableOperations.FirstTwo(seq));
    }

    [Fact]
    public void LastTwo_ReturnsLastTwoElements()
    {
        var seq = Seq(1, 2, 3, 4, 5);
        Assert.Equal(Seq(4, 5), ImmutableOperations.LastTwo(seq));
    }

    [Fact]
    public void Slice_ReturnsElementsInRange()
    {
        var seq = Seq(1, 2, 3, 4, 5);
        Assert.Equal(Seq(2, 3, 4), ImmutableOperations.Slice(seq, 1, 4));
    }

    // ============================================
    // 要素の追加のテスト
    // ============================================

    [Fact]
    public void Appended_AddsElementAtEnd()
    {
        var seq = Seq(1, 2, 3);
        Assert.Equal(Seq(1, 2, 3, 4), ImmutableOperations.Appended(seq, 4));
    }

    [Fact]
    public void AppendedAll_AddsAllElementsAtEnd()
    {
        var seq = Seq(1, 2);
        var elements = Seq(3, 4, 5);
        Assert.Equal(Seq(1, 2, 3, 4, 5), ImmutableOperations.AppendedAll(seq, elements));
    }

    // ============================================
    // リスト変換パターンのテスト
    // ============================================

    [Fact]
    public void MoveFirstTwoToEnd_MovesElements()
    {
        var seq = Seq(1, 2, 3, 4, 5);
        Assert.Equal(Seq(3, 4, 5, 1, 2), ImmutableOperations.MoveFirstTwoToEnd(seq));
    }

    [Fact]
    public void InsertBeforeLast_InsertsElement()
    {
        var seq = Seq(1, 2, 3);
        Assert.Equal(Seq(1, 2, 99, 3), ImmutableOperations.InsertBeforeLast(seq, 99));
    }

    [Fact]
    public void InsertAtMiddle_InsertsAtMiddle()
    {
        var seq = Seq(1, 2, 3, 4);
        Assert.Equal(Seq(1, 2, 99, 3, 4), ImmutableOperations.InsertAtMiddle(seq, 99));
    }

    [Fact]
    public void InsertAtMiddle_InsertsAtMiddleOddLength()
    {
        var seq = Seq(1, 2, 3, 4, 5);
        Assert.Equal(Seq(1, 2, 99, 3, 4, 5), ImmutableOperations.InsertAtMiddle(seq, 99));
    }

    // ============================================
    // イミュータビリティの確認
    // ============================================

    [Fact]
    public void Operations_DoNotMutateOriginal()
    {
        var original = Seq(1, 2, 3);
        var _ = ImmutableOperations.Appended(original, 4);
        Assert.Equal(3, original.Count);
    }
}

/// <summary>
/// 旅程の再計画のテスト
/// </summary>
public class ItineraryTests
{
    [Fact]
    public void Replan_InsertsBeforeSpecifiedCity()
    {
        var plan = Seq("Tokyo", "Osaka", "Kyoto");
        var result = Itinerary.Replan(plan, "Nagoya", "Osaka");
        Assert.Equal(Seq("Tokyo", "Nagoya", "Osaka", "Kyoto"), result);
    }

    [Fact]
    public void Replan_AddsToEndWhenCityNotFound()
    {
        var plan = Seq("Tokyo", "Osaka");
        var result = Itinerary.Replan(plan, "Kyoto", "NotFound");
        Assert.Equal(Seq("Tokyo", "Osaka", "Kyoto"), result);
    }

    [Fact]
    public void Replan_InsertsAtBeginningWhenBeforeFirstCity()
    {
        var plan = Seq("Tokyo", "Osaka");
        var result = Itinerary.Replan(plan, "Nagoya", "Tokyo");
        Assert.Equal(Seq("Nagoya", "Tokyo", "Osaka"), result);
    }
}

/// <summary>
/// 文字列操作のテスト
/// </summary>
public class StringOperationsTests
{
    [Fact]
    public void Abbreviate_ConvertsToInitialDotLastName()
    {
        Assert.Equal("A. Church", StringOperations.Abbreviate("Alonzo Church"));
        Assert.Equal("J. McCarthy", StringOperations.Abbreviate("John McCarthy"));
    }

    [Fact]
    public void Abbreviate_ReturnsOriginalWhenNoSpace()
    {
        Assert.Equal("Alonzo", StringOperations.Abbreviate("Alonzo"));
    }

    [Fact]
    public void FirstNChars_ReturnsFirstNCharacters()
    {
        Assert.Equal("Hel", StringOperations.FirstNChars("Hello", 3));
        Assert.Equal("Hi", StringOperations.FirstNChars("Hi", 5));
    }

    [Fact]
    public void LastNChars_ReturnsLastNCharacters()
    {
        Assert.Equal("llo", StringOperations.LastNChars("Hello", 3));
        Assert.Equal("Hi", StringOperations.LastNChars("Hi", 5));
    }
}
