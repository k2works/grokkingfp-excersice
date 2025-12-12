"""第1章: 関数型プログラミング入門のテスト"""

import pytest
from grokking_fp.ch01_intro import (
    absolute,
    add,
    calculate_score_imperative,
    clamp,
    double,
    get_bounded_score,
    get_first_character,
    greet,
    increment,
    is_empty,
    is_even,
    is_positive,
    max_value,
    min_value,
    reverse_string,
    to_uppercase,
    word_score,
)


class TestWordScore:
    """word_score 関数のテスト"""

    def test_basic_word(self) -> None:
        assert word_score("hello") == 5

    def test_empty_string(self) -> None:
        assert word_score("") == 0

    def test_single_character(self) -> None:
        assert word_score("a") == 1

    def test_long_word(self) -> None:
        assert word_score("Python") == 6


class TestCalculateScoreImperative:
    """calculate_score_imperative 関数のテスト"""

    def test_matches_word_score(self) -> None:
        """命令型と関数型は同じ結果を返す"""
        words = ["hello", "", "Python", "a"]
        for word in words:
            assert calculate_score_imperative(word) == word_score(word)


class TestIncrement:
    """increment 関数のテスト"""

    def test_positive_number(self) -> None:
        assert increment(5) == 6

    def test_zero(self) -> None:
        assert increment(0) == 1

    def test_negative_number(self) -> None:
        assert increment(-1) == 0

    def test_boundary(self) -> None:
        assert increment(-100) == -99


class TestGetFirstCharacter:
    """get_first_character 関数のテスト"""

    def test_basic(self) -> None:
        assert get_first_character("hello") == "h"

    def test_uppercase(self) -> None:
        assert get_first_character("Python") == "P"

    def test_single_char(self) -> None:
        assert get_first_character("a") == "a"

    def test_empty_string_raises(self) -> None:
        with pytest.raises(IndexError):
            get_first_character("")


class TestAdd:
    """add 関数のテスト"""

    def test_positive_numbers(self) -> None:
        assert add(2, 3) == 5

    def test_negative_numbers(self) -> None:
        assert add(-1, -2) == -3

    def test_mixed_signs(self) -> None:
        assert add(-1, 1) == 0

    def test_zero(self) -> None:
        assert add(0, 5) == 5
        assert add(5, 0) == 5


class TestDouble:
    """double 関数のテスト"""

    def test_positive(self) -> None:
        assert double(5) == 10

    def test_zero(self) -> None:
        assert double(0) == 0

    def test_negative(self) -> None:
        assert double(-3) == -6


class TestGreet:
    """greet 関数のテスト"""

    def test_basic(self) -> None:
        assert greet("World") == "Hello, World!"

    def test_name(self) -> None:
        assert greet("Python") == "Hello, Python!"

    def test_empty(self) -> None:
        assert greet("") == "Hello, !"


class TestToUppercase:
    """to_uppercase 関数のテスト"""

    def test_lowercase(self) -> None:
        assert to_uppercase("hello") == "HELLO"

    def test_mixed_case(self) -> None:
        assert to_uppercase("Python") == "PYTHON"

    def test_already_upper(self) -> None:
        assert to_uppercase("HELLO") == "HELLO"

    def test_empty(self) -> None:
        assert to_uppercase("") == ""


class TestReverseString:
    """reverse_string 関数のテスト"""

    def test_basic(self) -> None:
        assert reverse_string("hello") == "olleh"

    def test_palindrome(self) -> None:
        assert reverse_string("radar") == "radar"

    def test_empty(self) -> None:
        assert reverse_string("") == ""

    def test_single_char(self) -> None:
        assert reverse_string("a") == "a"


class TestAbsolute:
    """absolute 関数のテスト"""

    def test_positive(self) -> None:
        assert absolute(5) == 5

    def test_negative(self) -> None:
        assert absolute(-5) == 5

    def test_zero(self) -> None:
        assert absolute(0) == 0


class TestMaxValue:
    """max_value 関数のテスト"""

    def test_first_larger(self) -> None:
        assert max_value(5, 3) == 5

    def test_second_larger(self) -> None:
        assert max_value(3, 5) == 5

    def test_equal(self) -> None:
        assert max_value(3, 3) == 3

    def test_negative(self) -> None:
        assert max_value(-1, -5) == -1


class TestMinValue:
    """min_value 関数のテスト"""

    def test_first_smaller(self) -> None:
        assert min_value(3, 5) == 3

    def test_second_smaller(self) -> None:
        assert min_value(5, 3) == 3

    def test_equal(self) -> None:
        assert min_value(3, 3) == 3

    def test_negative(self) -> None:
        assert min_value(-1, -5) == -5


class TestClamp:
    """clamp 関数のテスト"""

    def test_within_range(self) -> None:
        assert clamp(5, 0, 10) == 5

    def test_below_min(self) -> None:
        assert clamp(-5, 0, 10) == 0

    def test_above_max(self) -> None:
        assert clamp(15, 0, 10) == 10

    def test_at_boundaries(self) -> None:
        assert clamp(0, 0, 10) == 0
        assert clamp(10, 0, 10) == 10


class TestIsEven:
    """is_even 関数のテスト"""

    def test_even_positive(self) -> None:
        assert is_even(4) is True
        assert is_even(2) is True

    def test_odd_positive(self) -> None:
        assert is_even(3) is False
        assert is_even(1) is False

    def test_zero(self) -> None:
        assert is_even(0) is True

    def test_negative_even(self) -> None:
        assert is_even(-2) is True
        assert is_even(-4) is True

    def test_negative_odd(self) -> None:
        assert is_even(-3) is False


class TestIsPositive:
    """is_positive 関数のテスト"""

    def test_positive(self) -> None:
        assert is_positive(5) is True
        assert is_positive(1) is True

    def test_zero(self) -> None:
        assert is_positive(0) is False

    def test_negative(self) -> None:
        assert is_positive(-3) is False


class TestIsEmpty:
    """is_empty 関数のテスト"""

    def test_empty(self) -> None:
        assert is_empty("") is True

    def test_not_empty(self) -> None:
        assert is_empty("hello") is False

    def test_whitespace(self) -> None:
        assert is_empty(" ") is False


class TestGetBoundedScore:
    """get_bounded_score 関数のテスト"""

    def test_within_bounds(self) -> None:
        assert get_bounded_score(50) == 50

    def test_above_max(self) -> None:
        assert get_bounded_score(150) == 100

    def test_below_min(self) -> None:
        assert get_bounded_score(-10) == 0

    def test_at_boundaries(self) -> None:
        assert get_bounded_score(0) == 0
        assert get_bounded_score(100) == 100
