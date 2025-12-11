using Ch12;
using LanguageExt;
using static LanguageExt.Prelude;
using Xunit;
using static Ch12.TestingStrategies;

namespace GrokkingFP.CSharp.Tests.Ch12;

/// <summary>
/// 第12章: テスト戦略のテスト
/// </summary>
public class PureFunctionTests
{
    [Fact]
    public void Add_ReturnsSumOfTwoNumbers()
    {
        Assert.Equal(5, Add(2, 3));
        Assert.Equal(0, Add(0, 0));
        Assert.Equal(-1, Add(2, -3));
    }

    [Fact]
    public void ToUpperCase_ConvertsToUpperCase()
    {
        Assert.Equal("HELLO", ToUpperCase("hello"));
        Assert.Equal("HELLO WORLD", ToUpperCase("Hello World"));
        Assert.Equal("", ToUpperCase(""));
    }

    [Fact]
    public void FilterPositive_FiltersCorrectly()
    {
        var numbers = Seq(1, -2, 3, -4, 5);
        var result = FilterPositive(numbers, n => n > 0);
        Assert.Equal(Seq(1, 3, 5), result);
    }
}

public class PropertyBasedTests
{
    [Theory]
    [InlineData(1, 2)]
    [InlineData(0, 0)]
    [InlineData(-5, 5)]
    [InlineData(100, -100)]
    public void Addition_IsCommutative(int a, int b)
    {
        Assert.True(AdditionIsCommutative(a, b));
    }

    [Theory]
    [InlineData(1, 2, 3)]
    [InlineData(0, 0, 0)]
    [InlineData(-5, 5, 10)]
    public void Addition_IsAssociative(int a, int b, int c)
    {
        Assert.True(AdditionIsAssociative(a, b, c));
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(-100)]
    [InlineData(int.MaxValue)]
    public void Addition_HasIdentity(int a)
    {
        Assert.True(AdditionHasIdentity(a));
    }

    [Fact]
    public void Addition_Properties_HoldForRandomInputs()
    {
        for (int i = 0; i < 100; i++)
        {
            var a = TestDataGenerator.RandomInt(-1000, 1000);
            var b = TestDataGenerator.RandomInt(-1000, 1000);
            var c = TestDataGenerator.RandomInt(-1000, 1000);

            Assert.True(AdditionIsCommutative(a, b));
            Assert.True(AdditionIsAssociative(a, b, c));
            Assert.True(AdditionHasIdentity(a));
        }
    }
}

public class OptionTests
{
    [Fact]
    public void SafeDivide_ReturnsSomeForValidDivision()
    {
        var result = SafeDivide(10, 2);
        Assert.True(result.IsSome);
        Assert.Equal(5, result.IfNone(0));
    }

    [Fact]
    public void SafeDivide_ReturnsNoneForZeroDivision()
    {
        var result = SafeDivide(10, 0);
        Assert.True(result.IsNone);
    }

    [Theory]
    [InlineData("123", 123)]
    [InlineData("-456", -456)]
    [InlineData("0", 0)]
    public void ParseInt_ReturnsSomeForValidInput(string input, int expected)
    {
        var result = ParseInt(input);
        Assert.True(result.IsSome);
        Assert.Equal(expected, result.IfNone(0));
    }

    [Theory]
    [InlineData("abc")]
    [InlineData("12.34")]
    [InlineData("")]
    public void ParseInt_ReturnsNoneForInvalidInput(string input)
    {
        var result = ParseInt(input);
        Assert.True(result.IsNone);
    }

    [Fact]
    public void SafeHead_ReturnsSomeForNonEmptySeq()
    {
        var result = SafeHead(Seq(1, 2, 3));
        Assert.True(result.IsSome);
        Assert.Equal(1, result.IfNone(0));
    }

    [Fact]
    public void SafeHead_ReturnsNoneForEmptySeq()
    {
        var result = SafeHead(Seq<int>());
        Assert.True(result.IsNone);
    }
}

public class EitherValidationTests
{
    [Fact]
    public void ValidateString_ReturnsRightForValidInput()
    {
        var result = ValidateString("hello", 3, 10);
        Assert.True(result.IsRight);
        var value = result.Match(
            Left: _ => "",
            Right: v => v);
        Assert.Equal("hello", value);
    }

    [Fact]
    public void ValidateString_ReturnsLeftForEmptyInput()
    {
        var result = ValidateString("", 3, 10);
        Assert.True(result.IsLeft);
        var error = result.Match(
            Left: e => e,
            Right: _ => default);
        Assert.Equal(ValidationError.Empty, error);
    }

    [Fact]
    public void ValidateString_ReturnsLeftForTooShortInput()
    {
        var result = ValidateString("ab", 3, 10);
        Assert.True(result.IsLeft);
        var error = result.Match(
            Left: e => e,
            Right: _ => default);
        Assert.Equal(ValidationError.TooShort, error);
    }

    [Fact]
    public void ValidateString_ReturnsLeftForTooLongInput()
    {
        var result = ValidateString("verylongstring", 3, 10);
        Assert.True(result.IsLeft);
        var error = result.Match(
            Left: e => e,
            Right: _ => default);
        Assert.Equal(ValidationError.TooLong, error);
    }

    [Fact]
    public void ValidateEmail_ReturnsRightForValidEmail()
    {
        var result = ValidateEmail("test@example.com");
        Assert.True(result.IsRight);
    }

    [Fact]
    public void ValidateEmail_ReturnsLeftForInvalidEmail()
    {
        var result = ValidateEmail("invalid-email");
        Assert.True(result.IsLeft);
        var error = result.Match(
            Left: e => e,
            Right: _ => default);
        Assert.Equal(ValidationError.InvalidFormat, error);
    }
}

public class StubAndSpyTests
{
    [Fact]
    public async Task SuccessStub_ReturnsConfiguredData()
    {
        var stub = new SuccessStub("test-data");
        var result = await stub.FetchData("any-id");

        Assert.True(result.IsRight);
        var value = result.Match(
            Left: _ => "",
            Right: v => v);
        Assert.Equal("test-data", value);
    }

    [Fact]
    public async Task FailureStub_ReturnsConfiguredError()
    {
        var stub = new FailureStub("error-message");
        var result = await stub.FetchData("any-id");

        Assert.True(result.IsLeft);
        var error = result.Match(
            Left: e => e,
            Right: _ => "");
        Assert.Equal("error-message", error);
    }

    [Fact]
    public async Task SpyService_RecordsCallCount()
    {
        var inner = new SuccessStub("data");
        var spy = new SpyService(inner);

        await spy.FetchData("id1");
        await spy.FetchData("id2");
        await spy.FetchData("id3");

        Assert.Equal(3, spy.CallCount);
    }

    [Fact]
    public async Task SpyService_RecordsCalledIds()
    {
        var inner = new SuccessStub("data");
        var spy = new SpyService(inner);

        await spy.FetchData("first");
        await spy.FetchData("second");

        Assert.Equal(2, spy.CalledIds.Count);
        Assert.Contains("first", spy.CalledIds);
        Assert.Contains("second", spy.CalledIds);
    }
}

public class ComposableValidationTests
{
    [Fact]
    public void Validator_ReturnsValidForGoodInput()
    {
        var result = Validator.Validate(
            "hello@test.com",
            Validator.NotEmpty(),
            Validator.MinLength(5),
            Validator.ContainsAt()
        );

        Assert.True(result.IsValid);
        Assert.Equal("hello@test.com", result.Value);
        Assert.Empty(result.Errors);
    }

    [Fact]
    public void Validator_CollectsAllErrors()
    {
        var result = Validator.Validate(
            "",
            Validator.NotEmpty(),
            Validator.MinLength(5),
            Validator.ContainsAt()
        );

        Assert.False(result.IsValid);
        Assert.Contains("Value cannot be empty", result.Errors);
    }

    [Fact]
    public void Validator_ReturnsMultipleErrorsForMultipleFailures()
    {
        var result = Validator.Validate(
            "ab",
            Validator.NotEmpty(),
            Validator.MinLength(5),
            Validator.ContainsAt()
        );

        Assert.False(result.IsValid);
        Assert.Equal(2, result.Errors.Count);
    }
}

public class TestDataGeneratorTests
{
    [Fact]
    public void RandomInt_GeneratesNumberInRange()
    {
        for (int i = 0; i < 100; i++)
        {
            var result = TestDataGenerator.RandomInt(0, 100);
            Assert.InRange(result, 0, 99);
        }
    }

    [Fact]
    public void RandomString_GeneratesCorrectLength()
    {
        var result = TestDataGenerator.RandomString(10);
        Assert.Equal(10, result.Length);
    }

    [Fact]
    public void RandomSeq_GeneratesCorrectCount()
    {
        var result = TestDataGenerator.RandomSeq(() => TestDataGenerator.RandomInt(), 5);
        Assert.Equal(5, result.Count);
    }

    [Fact]
    public void RandomOption_GeneratesBothSomeAndNone()
    {
        var someCount = 0;
        var noneCount = 0;

        for (int i = 0; i < 1000; i++)
        {
            var result = TestDataGenerator.RandomOption(() => 42, 0.5);
            if (result.IsSome) someCount++;
            else noneCount++;
        }

        Assert.True(someCount > 100);
        Assert.True(noneCount > 100);
    }
}

public class ValidationResultTests
{
    [Fact]
    public void ValidationResult_Valid_IsValid()
    {
        var result = ValidationResult<string>.Valid("test");
        Assert.True(result.IsValid);
        Assert.Equal("test", result.Value);
        Assert.Empty(result.Errors);
    }

    [Fact]
    public void ValidationResult_Invalid_IsNotValid()
    {
        var result = ValidationResult<string>.Invalid("error1", "error2");
        Assert.False(result.IsValid);
        Assert.Null(result.Value);
        Assert.Equal(2, result.Errors.Count);
    }
}
