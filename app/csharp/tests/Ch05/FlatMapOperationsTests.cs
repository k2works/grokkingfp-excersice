using Ch05;
using LanguageExt;
using static LanguageExt.Prelude;
using Xunit;
using static Ch05.FlatMapOperations;

namespace GrokkingFP.CSharp.Tests.Ch05;

/// <summary>
/// 第5章: FlatMap 操作のテスト
/// </summary>
public class FlatMapOperationsTests
{
    // ============================================
    // Flatten のテスト
    // ============================================

    [Fact]
    public void Flatten_FlattensNestedSeq()
    {
        var nested = Seq(Seq(1, 2), Seq(3, 4), Seq(5));
        Assert.Equal(Seq(1, 2, 3, 4, 5), FlatMapOperations.Flatten(nested));
    }

    [Fact]
    public void Flatten_HandlesEmptyInnerSeqs()
    {
        var nested = Seq(Seq(1, 2), Seq<int>(), Seq(3));
        Assert.Equal(Seq(1, 2, 3), FlatMapOperations.Flatten(nested));
    }

    // ============================================
    // GetAllAuthors のテスト
    // ============================================

    [Fact]
    public void GetAllAuthors_ReturnsAllAuthorsFromBooks()
    {
        var books = Seq(
            new Book("Book1", Seq("Author1", "Author2")),
            new Book("Book2", Seq("Author3"))
        );
        var authors = FlatMapOperations.GetAllAuthors(books);
        Assert.Equal(Seq("Author1", "Author2", "Author3"), authors);
    }

    [Fact]
    public void GetAllAuthors_HandlesEmptyAuthors()
    {
        var books = Seq(
            new Book("Book1", Seq<string>()),
            new Book("Book2", Seq("Author1"))
        );
        var authors = FlatMapOperations.GetAllAuthors(books);
        Assert.Equal(Seq("Author1"), authors);
    }

    // ============================================
    // サイズ変化のテスト
    // ============================================

    [Fact]
    public void Duplicate_IncreasesSize()
    {
        var numbers = Seq(1, 2, 3);
        var result = FlatMapOperations.Duplicate(numbers);
        // [1, 11, 2, 12, 3, 13]
        Assert.Equal(6, result.Count);
        Assert.Equal(Seq(1, 11, 2, 12, 3, 13), result);
    }

    [Fact]
    public void MapToSeq_MaintainsSize()
    {
        var numbers = Seq(1, 2, 3);
        var result = FlatMapOperations.MapToSeq(numbers);
        Assert.Equal(Seq(2, 4, 6), result);
    }

    [Fact]
    public void FilterEven_DecreasesSize()
    {
        var numbers = Seq(1, 2, 3, 4, 5, 6);
        var result = FlatMapOperations.FilterEven(numbers);
        Assert.Equal(Seq(2, 4, 6), result);
    }

    // ============================================
    // レコメンデーションのテスト
    // ============================================

    [Fact]
    public void BookAdaptations_ReturnsTolkienMovies()
    {
        var movies = FlatMapOperations.BookAdaptations("Tolkien");
        Assert.Equal(2, movies.Count);
    }

    [Fact]
    public void BookAdaptations_ReturnsEmptyForOtherAuthors()
    {
        var movies = FlatMapOperations.BookAdaptations("Unknown");
        Assert.True(movies.IsEmpty);
    }

    [Fact]
    public void GetRecommendations_GeneratesRecommendationStrings()
    {
        var books = Seq(
            new Book("The Hobbit", Seq("Tolkien"))
        );
        var recommendations = FlatMapOperations.GetRecommendations(books);
        Assert.Equal(2, recommendations.Count);
        Assert.True(((string)recommendations.Head).Contains("An Unexpected Journey"));
    }

    [Fact]
    public void GetRecommendationsLinq_ProducesSameResultsAsBind()
    {
        var books = Seq(
            new Book("The Hobbit", Seq("Tolkien"))
        );
        var bindResult = FlatMapOperations.GetRecommendations(books);
        var linqResult = FlatMapOperations.GetRecommendationsLinq(books);
        Assert.Equal(bindResult, linqResult);
    }

    // ============================================
    // 円内の点のテスト
    // ============================================

    [Fact]
    public void IsInside_ReturnsTrueForPointInCircle()
    {
        Assert.True(FlatMapOperations.IsInside(new Point(0, 0), 5));
        Assert.True(FlatMapOperations.IsInside(new Point(3, 4), 5)); // 3² + 4² = 25 = 5²
    }

    [Fact]
    public void IsInside_ReturnsFalseForPointOutsideCircle()
    {
        Assert.False(FlatMapOperations.IsInside(new Point(5, 5), 5));
    }

    [Fact]
    public void AllCombinations_GeneratesAllPairs()
    {
        var radiuses = Seq(5, 10);
        var points = Seq(new Point(3, 4), new Point(8, 8));
        var result = FlatMapOperations.AllCombinations(radiuses, points);
        Assert.Equal(4, result.Count);
    }

    [Fact]
    public void InsidePointsOnly_FiltersToInsidePoints()
    {
        var radiuses = Seq(5);
        var points = Seq(new Point(3, 4), new Point(8, 8));
        var result = FlatMapOperations.InsidePointsOnly(radiuses, points);
        Assert.Single(result);
        Assert.True(((string)result.Head).Contains("Point(3,4)"));
    }

    // ============================================
    // LINQ クエリ式のテスト
    // ============================================

    [Fact]
    public void ListComprehension_GeneratesAllProducts()
    {
        var xs = Seq(1, 2);
        var ys = Seq(3, 4);
        var result = FlatMapOperations.ListComprehension(xs, ys);
        // 1*3, 1*4, 2*3, 2*4 = 3, 4, 6, 8
        Assert.Equal(Seq(3, 4, 6, 8), result);
    }

    [Fact]
    public void SetToSeqComprehension_WorksWithHashSet()
    {
        var xs = new System.Collections.Generic.HashSet<int> { 1, 2 };
        var ys = Seq(3, 4);
        var result = FlatMapOperations.SetToSeqComprehension(xs, ys);
        Assert.Equal(4, result.Count);
    }

    [Fact]
    public void AllTripleCombinations_GeneratesAllSums()
    {
        var xs = Seq(1);
        var ys = Seq(2);
        var zs = Seq(3, 4);
        var result = FlatMapOperations.AllTripleCombinations(xs, ys, zs);
        // 1+2+3, 1+2+4 = 6, 7
        Assert.Equal(Seq(6, 7), result);
    }

    [Fact]
    public void FilterEvenWithBind_SameAsFilterEven()
    {
        var numbers = Seq(1, 2, 3, 4, 5, 6);
        var result1 = FlatMapOperations.FilterEven(numbers);
        var result2 = FlatMapOperations.FilterEvenWithBind(numbers);
        Assert.Equal(result1, result2);
    }

    [Fact]
    public void FilterWithOption_FiltersUsingPredicate()
    {
        var numbers = Seq(1, 2, 3, 4, 5);
        var result = FlatMapOperations.FilterWithOption(numbers, x => x > 3);
        Assert.Equal(Seq(4, 5), result);
    }
}
