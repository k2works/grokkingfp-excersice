"""第2章: 純粋関数とテストのテスト"""

from grokking_fp.ch02_pure_functions import (
    add,
    all_positive,
    any_negative,
    append_exclamation,
    apply_twice,
    average,
    bonus_score,
    calculate_discount,
    calculate_final_price,
    calculate_tip,
    compose,
    count_vowels,
    double_all,
    filter_positive,
    find_longest,
    format_timestamp,
    get_discount_percentage,
    get_tip_percentage,
    referential_transparency_example,
    string_length,
    sum_list,
    word_score,
    word_score_no_a,
)


class TestAdd:
    """add 関数のテスト"""

    def test_positive_numbers(self) -> None:
        assert add(2, 3) == 5

    def test_negative_numbers(self) -> None:
        assert add(-1, -2) == -3

    def test_mixed(self) -> None:
        assert add(-1, 1) == 0


class TestStringLength:
    """string_length 関数のテスト"""

    def test_basic(self) -> None:
        assert string_length("hello") == 5

    def test_empty(self) -> None:
        assert string_length("") == 0


class TestWordScore:
    """word_score 関数のテスト"""

    def test_basic(self) -> None:
        assert word_score("Scala") == 5
        assert word_score("Python") == 6


class TestBonusScore:
    """bonus_score 関数のテスト"""

    def test_with_c(self) -> None:
        assert bonus_score("Scala") == 10
        assert bonus_score("cat") == 8

    def test_without_c(self) -> None:
        assert bonus_score("Python") == 6

    def test_uppercase_c(self) -> None:
        assert bonus_score("C") == 6  # 1 + 5


class TestGetDiscountPercentage:
    """get_discount_percentage 関数のテスト"""

    def test_with_book(self) -> None:
        assert get_discount_percentage(["Apple", "Book"]) == 5

    def test_without_book(self) -> None:
        assert get_discount_percentage(["Apple", "Orange"]) == 0

    def test_empty_list(self) -> None:
        assert get_discount_percentage([]) == 0


class TestCalculateDiscount:
    """calculate_discount 関数のテスト"""

    def test_ten_percent(self) -> None:
        assert calculate_discount(100.0, 10) == 10.0

    def test_zero_percent(self) -> None:
        assert calculate_discount(100.0, 0) == 0.0

    def test_five_percent(self) -> None:
        assert calculate_discount(200.0, 5) == 10.0


class TestCalculateFinalPrice:
    """calculate_final_price 関数のテスト"""

    def test_with_book_discount(self) -> None:
        assert calculate_final_price(100.0, ["Apple", "Book"]) == 95.0

    def test_without_discount(self) -> None:
        assert calculate_final_price(100.0, ["Apple"]) == 100.0


class TestGetTipPercentage:
    """get_tip_percentage 関数のテスト"""

    def test_large_group(self) -> None:
        names = ["A", "B", "C", "D", "E", "F"]
        assert get_tip_percentage(names) == 20

    def test_small_group(self) -> None:
        names = ["A", "B"]
        assert get_tip_percentage(names) == 10

    def test_empty_group(self) -> None:
        assert get_tip_percentage([]) == 0

    def test_boundary_five(self) -> None:
        names = ["A", "B", "C", "D", "E"]
        assert get_tip_percentage(names) == 10

    def test_boundary_six(self) -> None:
        names = ["A", "B", "C", "D", "E", "F"]
        assert get_tip_percentage(names) == 20


class TestCalculateTip:
    """calculate_tip 関数のテスト"""

    def test_small_group(self) -> None:
        assert calculate_tip(100.0, ["Alice", "Bob"]) == 10.0

    def test_large_group(self) -> None:
        assert calculate_tip(100.0, ["A", "B", "C", "D", "E", "F"]) == 20.0


class TestReferentialTransparency:
    """referential_transparency_example 関数のテスト"""

    def test_example(self) -> None:
        assert referential_transparency_example() is True


class TestAppendExclamation:
    """append_exclamation 関数のテスト"""

    def test_basic(self) -> None:
        assert append_exclamation("Hello") == "Hello!"

    def test_empty(self) -> None:
        assert append_exclamation("") == "!"


class TestCountVowels:
    """count_vowels 関数のテスト"""

    def test_basic(self) -> None:
        assert count_vowels("hello") == 2

    def test_no_vowels(self) -> None:
        assert count_vowels("xyz") == 0

    def test_all_vowels(self) -> None:
        assert count_vowels("aeiou") == 5

    def test_uppercase_vowels(self) -> None:
        assert count_vowels("AEIOU") == 5


class TestDoubleAll:
    """double_all 関数のテスト"""

    def test_basic(self) -> None:
        assert double_all([1, 2, 3]) == [2, 4, 6]

    def test_empty(self) -> None:
        assert double_all([]) == []

    def test_negative(self) -> None:
        assert double_all([-1, -2]) == [-2, -4]


class TestFilterPositive:
    """filter_positive 関数のテスト"""

    def test_mixed(self) -> None:
        assert filter_positive([1, -2, 3, -4, 5]) == [1, 3, 5]

    def test_all_negative(self) -> None:
        assert filter_positive([-1, -2]) == []

    def test_all_positive(self) -> None:
        assert filter_positive([1, 2, 3]) == [1, 2, 3]

    def test_with_zero(self) -> None:
        assert filter_positive([0, 1, -1]) == [1]


class TestFindLongest:
    """find_longest 関数のテスト"""

    def test_basic(self) -> None:
        assert find_longest(["apple", "banana", "cherry"]) == "banana"

    def test_ascending_length(self) -> None:
        assert find_longest(["a", "bb", "ccc"]) == "ccc"

    def test_empty_list(self) -> None:
        assert find_longest([]) == ""

    def test_single_element(self) -> None:
        assert find_longest(["only"]) == "only"


class TestAllPositive:
    """all_positive 関数のテスト"""

    def test_all_positive(self) -> None:
        assert all_positive([1, 2, 3]) is True

    def test_some_negative(self) -> None:
        assert all_positive([1, -2, 3]) is False

    def test_empty(self) -> None:
        assert all_positive([]) is True

    def test_with_zero(self) -> None:
        assert all_positive([0, 1, 2]) is False


class TestAnyNegative:
    """any_negative 関数のテスト"""

    def test_with_negative(self) -> None:
        assert any_negative([1, -2, 3]) is True

    def test_all_positive(self) -> None:
        assert any_negative([1, 2, 3]) is False

    def test_empty(self) -> None:
        assert any_negative([]) is False


class TestSumList:
    """sum_list 関数のテスト"""

    def test_basic(self) -> None:
        assert sum_list([1, 2, 3, 4, 5]) == 15

    def test_empty(self) -> None:
        assert sum_list([]) == 0

    def test_negative(self) -> None:
        assert sum_list([-1, 1]) == 0


class TestAverage:
    """average 関数のテスト"""

    def test_basic(self) -> None:
        assert average([1.0, 2.0, 3.0, 4.0, 5.0]) == 3.0

    def test_empty(self) -> None:
        assert average([]) is None

    def test_single(self) -> None:
        assert average([5.0]) == 5.0


class TestWordScoreNoA:
    """word_score_no_a 関数のテスト"""

    def test_with_a(self) -> None:
        assert word_score_no_a("Scala") == 3  # "Scl"

    def test_without_a(self) -> None:
        assert word_score_no_a("function") == 8

    def test_empty(self) -> None:
        assert word_score_no_a("") == 0

    def test_only_a(self) -> None:
        assert word_score_no_a("aaa") == 0

    def test_uppercase_a(self) -> None:
        assert word_score_no_a("AAA") == 0


class TestApplyTwice:
    """apply_twice 関数のテスト"""

    def test_increment(self) -> None:
        assert apply_twice(lambda x: x + 1, 5) == 7

    def test_double(self) -> None:
        assert apply_twice(lambda x: x * 2, 3) == 12


class TestCompose:
    """compose 関数のテスト"""

    def test_basic_composition(self) -> None:
        double = lambda x: x * 2
        add_one = lambda x: x + 1
        double_then_add = compose(add_one, double)
        assert double_then_add(5) == 11  # (5 * 2) + 1

    def test_reverse_composition(self) -> None:
        double = lambda x: x * 2
        add_one = lambda x: x + 1
        add_then_double = compose(double, add_one)
        assert add_then_double(5) == 12  # (5 + 1) * 2


class TestFormatTimestamp:
    """format_timestamp 関数のテスト"""

    def test_basic(self) -> None:
        assert format_timestamp(2024, 1, 15) == "2024-01-15"

    def test_december(self) -> None:
        assert format_timestamp(2024, 12, 31) == "2024-12-31"

    def test_single_digit_padding(self) -> None:
        assert format_timestamp(2024, 1, 1) == "2024-01-01"
