"""第4章: 関数を値として扱うのテスト"""

from grokking_fp.ch04_higher_order import (
    bonus,
    contains_s_more_than,
    cumulative_score,
    divisible_by,
    double_all,
    filter_larger_than,
    filter_odd,
    filter_short_words,
    find_max,
    get_lengths,
    get_scores,
    high_scoring_words,
    high_scoring_words_with_threshold,
    larger_than,
    my_filter,
    my_map,
    my_reduce,
    negate_all,
    number_of_s,
    penalty,
    ranked_words,
    score,
    shorter_than,
    sort_by_length,
    sort_by_length_desc,
    sort_by_number_of_s,
    sum_all,
    total_length,
    total_s_count,
    word_score_with_bonus,
    word_score_with_bonus_and_penalty,
)
import pytest


class TestScore:
    """score 関数のテスト"""

    def test_java(self) -> None:
        assert score("java") == 2

    def test_rust(self) -> None:
        assert score("rust") == 4

    def test_empty(self) -> None:
        assert score("") == 0


class TestRankedWords:
    """ranked_words 関数のテスト"""

    def test_basic(self) -> None:
        assert ranked_words(["rust", "java"], score) == ["rust", "java"]

    def test_multiple_words(self) -> None:
        words = ["ada", "haskell", "scala", "java", "rust"]
        result = ranked_words(words, score)
        assert result == ["haskell", "rust", "scala", "java", "ada"]


class TestSortByLength:
    """sort_by_length 関数のテスト"""

    def test_basic(self) -> None:
        assert sort_by_length(["scala", "rust", "ada"]) == ["ada", "rust", "scala"]


class TestSortByLengthDesc:
    """sort_by_length_desc 関数のテスト"""

    def test_basic(self) -> None:
        assert sort_by_length_desc(["scala", "rust", "ada"]) == ["scala", "rust", "ada"]


class TestNumberOfS:
    """number_of_s 関数のテスト"""

    def test_rust(self) -> None:
        assert number_of_s("rust") == 1

    def test_ada(self) -> None:
        assert number_of_s("ada") == 0

    def test_mississippi(self) -> None:
        assert number_of_s("mississippi") == 4


class TestSortByNumberOfS:
    """sort_by_number_of_s 関数のテスト"""

    def test_basic(self) -> None:
        assert sort_by_number_of_s(["rust", "ada"]) == ["ada", "rust"]


class TestGetLengths:
    """get_lengths 関数のテスト"""

    def test_basic(self) -> None:
        assert get_lengths(["scala", "rust", "ada"]) == [5, 4, 3]


class TestGetScores:
    """get_scores 関数のテスト"""

    def test_basic(self) -> None:
        assert get_scores(["rust", "java"], score) == [4, 2]


class TestDoubleAll:
    """double_all 関数のテスト"""

    def test_basic(self) -> None:
        assert double_all([5, 1, 2, 4, 0]) == [10, 2, 4, 8, 0]


class TestNegateAll:
    """negate_all 関数のテスト"""

    def test_basic(self) -> None:
        assert negate_all([5, 1, 2, 4, 0]) == [-5, -1, -2, -4, 0]


class TestFilterShortWords:
    """filter_short_words 関数のテスト"""

    def test_basic(self) -> None:
        assert filter_short_words(["scala", "rust", "ada"], 4) == ["rust", "ada"]


class TestFilterOdd:
    """filter_odd 関数のテスト"""

    def test_basic(self) -> None:
        assert filter_odd([5, 1, 2, 4, 0]) == [5, 1]


class TestFilterLargerThan:
    """filter_larger_than 関数のテスト"""

    def test_basic(self) -> None:
        assert filter_larger_than([5, 1, 2, 4, 0], 4) == [5]


class TestHighScoringWords:
    """high_scoring_words 関数のテスト"""

    def test_basic(self) -> None:
        assert high_scoring_words(["rust", "java"], score, 3) == ["rust"]


class TestSumAll:
    """sum_all 関数のテスト"""

    def test_basic(self) -> None:
        assert sum_all([5, 1, 2, 4, 100]) == 112


class TestTotalLength:
    """total_length 関数のテスト"""

    def test_basic(self) -> None:
        assert total_length(["scala", "rust", "ada"]) == 12


class TestTotalSCount:
    """total_s_count 関数のテスト"""

    def test_basic(self) -> None:
        assert total_s_count(["scala", "haskell", "rust", "ada"]) == 3


class TestFindMax:
    """find_max 関数のテスト"""

    def test_basic(self) -> None:
        assert find_max([5, 1, 2, 4, 15]) == 15

    def test_empty_raises(self) -> None:
        with pytest.raises(ValueError):
            find_max([])


class TestCumulativeScore:
    """cumulative_score 関数のテスト"""

    def test_basic(self) -> None:
        assert cumulative_score(["rust", "java"], score) == 6


class TestLargerThan:
    """larger_than 関数のテスト"""

    def test_basic(self) -> None:
        assert larger_than(4)(5) is True
        assert larger_than(4)(3) is False

    def test_filter(self) -> None:
        assert list(filter(larger_than(4), [5, 1, 2, 4, 0])) == [5]


class TestDivisibleBy:
    """divisible_by 関数のテスト"""

    def test_basic(self) -> None:
        assert divisible_by(5)(15) is True
        assert divisible_by(5)(7) is False

    def test_filter(self) -> None:
        assert list(filter(divisible_by(5), [5, 1, 2, 4, 15])) == [5, 15]


class TestShorterThan:
    """shorter_than 関数のテスト"""

    def test_basic(self) -> None:
        assert shorter_than(4)("ada") is True
        assert shorter_than(4)("scala") is False

    def test_filter(self) -> None:
        assert list(filter(shorter_than(4), ["scala", "ada"])) == ["ada"]


class TestContainsSMoreThan:
    """contains_s_more_than 関数のテスト"""

    def test_basic(self) -> None:
        assert contains_s_more_than(0)("rust") is True
        assert contains_s_more_than(0)("ada") is False

    def test_filter(self) -> None:
        assert list(filter(contains_s_more_than(0), ["rust", "ada"])) == ["rust"]


class TestBonus:
    """bonus 関数のテスト"""

    def test_with_c(self) -> None:
        assert bonus("scala") == 5

    def test_without_c(self) -> None:
        assert bonus("java") == 0


class TestPenalty:
    """penalty 関数のテスト"""

    def test_with_s(self) -> None:
        assert penalty("rust") == 7

    def test_without_s(self) -> None:
        assert penalty("java") == 0


class TestWordScoreWithBonus:
    """word_score_with_bonus 関数のテスト"""

    def test_scala(self) -> None:
        # score(scala)=3 + bonus(scala)=5 = 8
        assert word_score_with_bonus("scala") == 8

    def test_java(self) -> None:
        assert word_score_with_bonus("java") == 2


class TestWordScoreWithBonusAndPenalty:
    """word_score_with_bonus_and_penalty 関数のテスト"""

    def test_java(self) -> None:
        assert word_score_with_bonus_and_penalty("java") == 2

    def test_scala(self) -> None:
        # score(scala)=3 + bonus(scala)=5 - penalty(scala)=7 = 1
        assert word_score_with_bonus_and_penalty("scala") == 1

    def test_rust(self) -> None:
        assert word_score_with_bonus_and_penalty("rust") == -3


class TestHighScoringWordsWithThreshold:
    """high_scoring_words_with_threshold 関数のテスト"""

    def test_basic(self) -> None:
        words = ["ada", "haskell", "scala", "java", "rust"]
        scorer = high_scoring_words_with_threshold(word_score_with_bonus_and_penalty)
        assert scorer(1)(words) == ["java"]
        assert scorer(0)(words) == ["ada", "scala", "java"]


class TestMyMap:
    """my_map 関数のテスト"""

    def test_double(self) -> None:
        assert my_map(lambda x: x * 2, [1, 2, 3]) == [2, 4, 6]

    def test_len(self) -> None:
        assert my_map(len, ["a", "bb", "ccc"]) == [1, 2, 3]


class TestMyFilter:
    """my_filter 関数のテスト"""

    def test_positive(self) -> None:
        assert my_filter(lambda x: x > 0, [-1, 0, 1, 2]) == [1, 2]

    def test_long_strings(self) -> None:
        assert my_filter(lambda s: len(s) > 2, ["a", "bb", "ccc"]) == ["ccc"]


class TestMyReduce:
    """my_reduce 関数のテスト"""

    def test_sum(self) -> None:
        assert my_reduce(lambda acc, x: acc + x, [1, 2, 3], 0) == 6

    def test_total_length(self) -> None:
        assert my_reduce(lambda acc, s: acc + len(s), ["a", "bb"], 0) == 3
