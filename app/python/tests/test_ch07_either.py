"""第7章: Result 型と複合的なエラー処理のテスト"""

from returns.result import Failure, Success

from grokking_fp.ch07_either import (
    Artist,
    Location,
    MusicGenre,
    PeriodInYears,
    SearchByActiveYears,
    SearchByGenre,
    SearchByOrigin,
    TvShow,
    User,
    ValidationError,
    active_length,
    create_user,
    extract_name_result,
    extract_single_year_result,
    extract_year_end_result,
    extract_year_start_result,
    get_or_else,
    map_error,
    matches_condition,
    parse_int_result,
    parse_show_result,
    result_to_maybe,
    safe_divide_result,
    search_artists,
    sequence_results,
    validate_age,
    validate_age_typed,
    validate_email,
    validate_name_typed,
    validate_password,
    was_artist_active,
)


class TestParseIntResult:
    """parse_int_result 関数のテスト"""

    def test_valid_integer(self) -> None:
        assert parse_int_result("42") == Success(42)

    def test_invalid_string(self) -> None:
        result = parse_int_result("abc")
        assert isinstance(result, Failure)
        assert "Cannot parse" in result.failure()


class TestSafeDivideResult:
    """safe_divide_result 関数のテスト"""

    def test_valid_division(self) -> None:
        assert safe_divide_result(10, 2) == Success(5)

    def test_division_by_zero(self) -> None:
        result = safe_divide_result(10, 0)
        assert result == Failure("Division by zero")


class TestExtractNameResult:
    """extract_name_result 関数のテスト"""

    def test_valid_name(self) -> None:
        assert extract_name_result("Breaking Bad (2008-2013)") == Success("Breaking Bad")

    def test_invalid_format(self) -> None:
        result = extract_name_result("(2008-2013)")
        assert isinstance(result, Failure)
        assert "Can't extract name" in result.failure()


class TestExtractYearStartResult:
    """extract_year_start_result 関数のテスト"""

    def test_valid_year(self) -> None:
        assert extract_year_start_result("Breaking Bad (2008-2013)") == Success(2008)

    def test_invalid_year(self) -> None:
        result = extract_year_start_result("Breaking Bad (abc-2013)")
        assert isinstance(result, Failure)


class TestExtractYearEndResult:
    """extract_year_end_result 関数のテスト"""

    def test_valid_year(self) -> None:
        assert extract_year_end_result("Breaking Bad (2008-2013)") == Success(2013)

    def test_empty_year(self) -> None:
        result = extract_year_end_result("Breaking Bad (2008-)")
        assert isinstance(result, Failure)


class TestExtractSingleYearResult:
    """extract_single_year_result 関数のテスト"""

    def test_single_year(self) -> None:
        assert extract_single_year_result("Chernobyl (2019)") == Success(2019)

    def test_range_year(self) -> None:
        result = extract_single_year_result("Breaking Bad (2008-2013)")
        assert isinstance(result, Failure)


class TestParseShowResult:
    """parse_show_result 関数のテスト"""

    def test_valid_range(self) -> None:
        result = parse_show_result("Breaking Bad (2008-2013)")
        assert result == Success(TvShow("Breaking Bad", 2008, 2013))

    def test_single_year(self) -> None:
        result = parse_show_result("Chernobyl (2019)")
        assert result == Success(TvShow("Chernobyl", 2019, 2019))

    def test_invalid_format(self) -> None:
        result = parse_show_result("(2008-2013)")
        assert isinstance(result, Failure)


class TestValidateAge:
    """validate_age 関数のテスト"""

    def test_valid_age(self) -> None:
        assert validate_age(25) == Success(25)

    def test_negative_age(self) -> None:
        assert validate_age(-5) == Failure("Age cannot be negative")

    def test_too_old(self) -> None:
        assert validate_age(200) == Failure("Age cannot be greater than 150")


class TestValidateEmail:
    """validate_email 関数のテスト"""

    def test_valid_email(self) -> None:
        assert validate_email("user@example.com") == Success("user@example.com")

    def test_missing_at(self) -> None:
        assert validate_email("invalid") == Failure("Email must contain '@'")

    def test_invalid_domain(self) -> None:
        result = validate_email("user@invalid.xyz")
        assert isinstance(result, Failure)


class TestValidatePassword:
    """validate_password 関数のテスト"""

    def test_valid_password(self) -> None:
        assert validate_password("SecurePass123") == Success("SecurePass123")

    def test_too_short(self) -> None:
        result = validate_password("short")
        assert result == Failure("Password must be at least 8 characters")

    def test_no_uppercase(self) -> None:
        result = validate_password("lowercase123")
        assert "uppercase" in result.failure()

    def test_no_digit(self) -> None:
        result = validate_password("NoDigitHere")
        assert "digit" in result.failure()


class TestArtist:
    """Artist 関連のテスト"""

    def test_was_artist_active_still_active(self) -> None:
        metallica = Artist(
            "Metallica", MusicGenre.HEAVY_METAL, Location.US, PeriodInYears(1981, None)
        )
        assert was_artist_active(metallica, 2000, 2020) is True
        assert was_artist_active(metallica, 1970, 1975) is False

    def test_was_artist_active_ended(self) -> None:
        led_zeppelin = Artist(
            "Led Zeppelin", MusicGenre.HARD_ROCK, Location.UK, PeriodInYears(1968, 1980)
        )
        assert was_artist_active(led_zeppelin, 1970, 1975) is True
        assert was_artist_active(led_zeppelin, 1990, 2000) is False

    def test_active_length_still_active(self) -> None:
        metallica = Artist(
            "Metallica", MusicGenre.HEAVY_METAL, Location.US, PeriodInYears(1981, None)
        )
        assert active_length(metallica, 2024) == 43

    def test_active_length_ended(self) -> None:
        led_zeppelin = Artist(
            "Led Zeppelin", MusicGenre.HARD_ROCK, Location.UK, PeriodInYears(1968, 1980)
        )
        assert active_length(led_zeppelin, 2024) == 12


class TestSearchCondition:
    """SearchCondition 関連のテスト"""

    def test_matches_genre(self) -> None:
        metallica = Artist(
            "Metallica", MusicGenre.HEAVY_METAL, Location.US, PeriodInYears(1981, None)
        )
        assert matches_condition(metallica, SearchByGenre([MusicGenre.HEAVY_METAL])) is True
        assert matches_condition(metallica, SearchByGenre([MusicGenre.POP])) is False

    def test_matches_origin(self) -> None:
        metallica = Artist(
            "Metallica", MusicGenre.HEAVY_METAL, Location.US, PeriodInYears(1981, None)
        )
        assert matches_condition(metallica, SearchByOrigin([Location.US])) is True
        assert matches_condition(metallica, SearchByOrigin([Location.UK])) is False

    def test_matches_active_years(self) -> None:
        metallica = Artist(
            "Metallica", MusicGenre.HEAVY_METAL, Location.US, PeriodInYears(1981, None)
        )
        assert matches_condition(metallica, SearchByActiveYears(2000, 2020)) is True


class TestSearchArtists:
    """search_artists 関数のテスト"""

    def test_search_by_genre(self) -> None:
        metallica = Artist(
            "Metallica", MusicGenre.HEAVY_METAL, Location.US, PeriodInYears(1981, None)
        )
        queen = Artist(
            "Queen", MusicGenre.HARD_ROCK, Location.UK, PeriodInYears(1970, 1995)
        )
        artists = [metallica, queen]
        result = search_artists(artists, [SearchByGenre([MusicGenre.HEAVY_METAL])])
        assert len(result) == 1
        assert result[0].name == "Metallica"

    def test_search_multiple_conditions(self) -> None:
        metallica = Artist(
            "Metallica", MusicGenre.HEAVY_METAL, Location.US, PeriodInYears(1981, None)
        )
        queen = Artist(
            "Queen", MusicGenre.HARD_ROCK, Location.UK, PeriodInYears(1970, 1995)
        )
        artists = [metallica, queen]
        result = search_artists(
            artists,
            [SearchByGenre([MusicGenre.HARD_ROCK]), SearchByOrigin([Location.UK])],
        )
        assert len(result) == 1
        assert result[0].name == "Queen"


class TestCreateUser:
    """create_user 関数のテスト"""

    def test_valid_user(self) -> None:
        result = create_user("Alice", "alice@example.com", 25)
        assert result == Success(User("Alice", "alice@example.com", 25))

    def test_invalid_email(self) -> None:
        result = create_user("Bob", "invalid", 25)
        assert isinstance(result, Failure)

    def test_invalid_age(self) -> None:
        result = create_user("Charlie", "charlie@example.com", -5)
        assert isinstance(result, Failure)


class TestValidationErrorTyped:
    """型付きバリデーションエラーのテスト"""

    def test_validate_name_empty(self) -> None:
        assert validate_name_typed("") == Failure(ValidationError.EMPTY_NAME)

    def test_validate_name_valid(self) -> None:
        assert validate_name_typed("Alice") == Success("Alice")

    def test_validate_age_too_young(self) -> None:
        assert validate_age_typed(15) == Failure(ValidationError.AGE_TOO_YOUNG)

    def test_validate_age_too_old(self) -> None:
        assert validate_age_typed(150) == Failure(ValidationError.AGE_TOO_OLD)

    def test_validate_age_valid(self) -> None:
        assert validate_age_typed(25) == Success(25)


class TestResultToMaybe:
    """result_to_maybe 関数のテスト"""

    def test_success_to_some(self) -> None:
        from returns.maybe import Some

        result = result_to_maybe(Success(42))
        assert result == Some(42)

    def test_failure_to_nothing(self) -> None:
        from returns.maybe import Nothing

        result = result_to_maybe(Failure("error"))
        assert result == Nothing


class TestGetOrElse:
    """get_or_else 関数のテスト"""

    def test_success(self) -> None:
        assert get_or_else(Success(42), 0) == 42

    def test_failure(self) -> None:
        assert get_or_else(Failure("error"), 0) == 0


class TestMapError:
    """map_error 関数のテスト"""

    def test_failure(self) -> None:
        result = map_error(Failure("error"), lambda e: f"ERROR: {e}")
        assert result == Failure("ERROR: error")

    def test_success(self) -> None:
        result = map_error(Success(42), lambda e: f"ERROR: {e}")
        assert result == Success(42)


class TestSequenceResults:
    """sequence_results 関数のテスト"""

    def test_all_success(self) -> None:
        assert sequence_results([Success(1), Success(2), Success(3)]) == Success([1, 2, 3])

    def test_has_failure(self) -> None:
        result = sequence_results([Success(1), Failure("error"), Success(3)])
        assert result == Failure("error")
