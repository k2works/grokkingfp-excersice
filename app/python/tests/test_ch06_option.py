"""第6章: Maybe 型による安全なエラーハンドリングのテスト"""

from returns.maybe import Nothing, Some

from grokking_fp.ch06_option import (
    TvShow,
    add_strings,
    chain_operations,
    extract_name,
    extract_single_year,
    extract_year_end,
    extract_year_start,
    filter_some,
    first_some,
    get_value_or_default,
    parse_int,
    parse_show,
    parse_shows_all_or_nothing,
    parse_shows_best_effort,
    safe_divide,
    safe_find,
    safe_get_nested_value,
    safe_head,
    sequence_maybes,
    try_first_then_second,
)


class TestParseInt:
    """parse_int 関数のテスト"""

    def test_valid_integer(self) -> None:
        assert parse_int("42") == Some(42)

    def test_negative_integer(self) -> None:
        assert parse_int("-10") == Some(-10)

    def test_invalid_string(self) -> None:
        assert parse_int("abc") == Nothing

    def test_empty_string(self) -> None:
        assert parse_int("") == Nothing


class TestExtractName:
    """extract_name 関数のテスト"""

    def test_valid_name(self) -> None:
        assert extract_name("Breaking Bad (2008-2013)") == Some("Breaking Bad")

    def test_name_with_extra_spaces(self) -> None:
        assert extract_name("  Mad Men  (2007-2015)") == Some("Mad Men")

    def test_no_bracket(self) -> None:
        assert extract_name("Breaking Bad") == Nothing

    def test_bracket_at_start(self) -> None:
        assert extract_name("(2008-2013)") == Nothing


class TestExtractYearStart:
    """extract_year_start 関数のテスト"""

    def test_valid_year(self) -> None:
        assert extract_year_start("Breaking Bad (2008-2013)") == Some(2008)

    def test_invalid_year(self) -> None:
        assert extract_year_start("Breaking Bad (abc-2013)") == Nothing

    def test_no_dash(self) -> None:
        assert extract_year_start("Chernobyl (2019)") == Nothing


class TestExtractYearEnd:
    """extract_year_end 関数のテスト"""

    def test_valid_year(self) -> None:
        assert extract_year_end("Breaking Bad (2008-2013)") == Some(2013)

    def test_invalid_year(self) -> None:
        assert extract_year_end("Breaking Bad (2008-abc)") == Nothing

    def test_empty_year(self) -> None:
        assert extract_year_end("Breaking Bad (2008-)") == Nothing


class TestExtractSingleYear:
    """extract_single_year 関数のテスト"""

    def test_single_year(self) -> None:
        assert extract_single_year("Chernobyl (2019)") == Some(2019)

    def test_range_year(self) -> None:
        assert extract_single_year("Breaking Bad (2008-2013)") == Nothing

    def test_invalid_year(self) -> None:
        assert extract_single_year("Show (abc)") == Nothing


class TestParseShow:
    """parse_show 関数のテスト"""

    def test_valid_range(self) -> None:
        result = parse_show("Breaking Bad (2008-2013)")
        assert result == Some(TvShow("Breaking Bad", 2008, 2013))

    def test_single_year(self) -> None:
        result = parse_show("Chernobyl (2019)")
        assert result == Some(TvShow("Chernobyl", 2019, 2019))

    def test_invalid_format(self) -> None:
        assert parse_show("Invalid") == Nothing

    def test_missing_name(self) -> None:
        assert parse_show("(2008-2013)") == Nothing


class TestSafeDivide:
    """safe_divide 関数のテスト"""

    def test_valid_division(self) -> None:
        assert safe_divide(10, 2) == Some(5)

    def test_division_by_zero(self) -> None:
        assert safe_divide(10, 0) == Nothing

    def test_integer_division(self) -> None:
        assert safe_divide(7, 2) == Some(3)


class TestSafeHead:
    """safe_head 関数のテスト"""

    def test_non_empty_list(self) -> None:
        assert safe_head([1, 2, 3]) == Some(1)

    def test_empty_list(self) -> None:
        assert safe_head([]) == Nothing


class TestSafeFind:
    """safe_find 関数のテスト"""

    def test_found(self) -> None:
        assert safe_find([1, 2, 3, 4], lambda x: x > 2) == Some(3)

    def test_not_found(self) -> None:
        assert safe_find([1, 2, 3, 4], lambda x: x > 10) == Nothing


class TestAddStrings:
    """add_strings 関数のテスト"""

    def test_valid_addition(self) -> None:
        assert add_strings("10", "20") == Some(30)

    def test_first_invalid(self) -> None:
        assert add_strings("abc", "20") == Nothing

    def test_second_invalid(self) -> None:
        assert add_strings("10", "abc") == Nothing


class TestChainOperations:
    """chain_operations 関数のテスト"""

    def test_successful_chain(self) -> None:
        assert chain_operations(100) == Some(10)

    def test_failed_chain(self) -> None:
        assert chain_operations(7) == Nothing


class TestParseShowsBestEffort:
    """parse_shows_best_effort 関数のテスト"""

    def test_all_valid(self) -> None:
        shows = ["Breaking Bad (2008-2013)", "Mad Men (2007-2015)"]
        result = parse_shows_best_effort(shows)
        assert len(result) == 2
        assert result[0].title == "Breaking Bad"

    def test_some_invalid(self) -> None:
        shows = ["Breaking Bad (2008-2013)", "Invalid", "Mad Men (2007-2015)"]
        result = parse_shows_best_effort(shows)
        assert len(result) == 2

    def test_all_invalid(self) -> None:
        shows = ["Invalid1", "Invalid2"]
        result = parse_shows_best_effort(shows)
        assert len(result) == 0


class TestParseShowsAllOrNothing:
    """parse_shows_all_or_nothing 関数のテスト"""

    def test_all_valid(self) -> None:
        shows = ["Breaking Bad (2008-2013)", "Mad Men (2007-2015)"]
        result = parse_shows_all_or_nothing(shows)
        assert isinstance(result, Some)
        assert len(result.unwrap()) == 2

    def test_some_invalid(self) -> None:
        shows = ["Breaking Bad (2008-2013)", "Invalid"]
        result = parse_shows_all_or_nothing(shows)
        assert result == Nothing


class TestGetValueOrDefault:
    """get_value_or_default 関数のテスト"""

    def test_some_value(self) -> None:
        assert get_value_or_default(Some(42), 0) == 42

    def test_nothing(self) -> None:
        assert get_value_or_default(Nothing, 0) == 0


class TestTryFirstThenSecond:
    """try_first_then_second 関数のテスト"""

    def test_first_some(self) -> None:
        assert try_first_then_second(Some(1), Some(2)) == Some(1)

    def test_first_nothing(self) -> None:
        assert try_first_then_second(Nothing, Some(2)) == Some(2)

    def test_both_nothing(self) -> None:
        assert try_first_then_second(Nothing, Nothing) == Nothing


class TestSafeGetNestedValue:
    """safe_get_nested_value 関数のテスト"""

    def test_valid_path(self) -> None:
        data = {"a": {"b": {"c": "value"}}}
        assert safe_get_nested_value(data, "a", "b", "c") == Some("value")

    def test_invalid_path(self) -> None:
        data = {"a": {"b": {"c": "value"}}}
        assert safe_get_nested_value(data, "a", "x", "c") == Nothing


class TestFilterSome:
    """filter_some 関数のテスト"""

    def test_mixed_list(self) -> None:
        assert filter_some([Some(1), Nothing, Some(3), Nothing]) == [1, 3]

    def test_all_nothing(self) -> None:
        assert filter_some([Nothing, Nothing]) == []


class TestFirstSome:
    """first_some 関数のテスト"""

    def test_found(self) -> None:
        assert first_some([Nothing, Nothing, Some(3), Some(4)]) == Some(3)

    def test_not_found(self) -> None:
        assert first_some([Nothing, Nothing]) == Nothing


class TestSequenceMaybes:
    """sequence_maybes 関数のテスト"""

    def test_all_some(self) -> None:
        assert sequence_maybes([Some(1), Some(2), Some(3)]) == Some([1, 2, 3])

    def test_has_nothing(self) -> None:
        assert sequence_maybes([Some(1), Nothing, Some(3)]) == Nothing
