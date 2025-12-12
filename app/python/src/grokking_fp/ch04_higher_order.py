"""第4章: 関数を値として扱う

関数を他の関数に渡したり、関数から関数を返したりする方法を学びます。
Python では関数は第一級オブジェクトなので、変数に代入したり、
引数として渡したりすることができます。
"""

from collections.abc import Callable
from functools import reduce
from typing import TypeVar

T = TypeVar("T")
U = TypeVar("U")


# =============================================================================
# 4.1 関数を引数として渡す（sortBy、map、filter）
# =============================================================================


def score(word: str) -> int:
    """単語のスコアを計算する（'a'を除いた文字数）。

    Examples:
        >>> score("java")
        2
        >>> score("rust")
        4
    """
    return len(word.replace("a", ""))


def ranked_words(words: list[str], word_score: Callable[[str], int]) -> list[str]:
    """単語をスコア順（降順）にソートする。

    Examples:
        >>> ranked_words(["rust", "java"], score)
        ['rust', 'java']
        >>> ranked_words(["ada", "haskell", "scala"], score)
        ['haskell', 'scala', 'ada']
    """
    return sorted(words, key=word_score, reverse=True)


def sort_by_length(words: list[str]) -> list[str]:
    """文字列を長さ順（昇順）にソートする。

    Examples:
        >>> sort_by_length(["scala", "rust", "ada"])
        ['ada', 'rust', 'scala']
    """
    return sorted(words, key=len)


def sort_by_length_desc(words: list[str]) -> list[str]:
    """文字列を長さ順（降順）にソートする。

    Examples:
        >>> sort_by_length_desc(["scala", "rust", "ada"])
        ['scala', 'rust', 'ada']
    """
    return sorted(words, key=len, reverse=True)


def number_of_s(s: str) -> int:
    """文字列中の's'の数を数える。

    Examples:
        >>> number_of_s("rust")
        1
        >>> number_of_s("haskell")
        1
        >>> number_of_s("scala")
        1
        >>> number_of_s("ada")
        0
    """
    return s.count("s")


def sort_by_number_of_s(words: list[str]) -> list[str]:
    """文字列を's'の数順（昇順）にソートする。

    Examples:
        >>> sort_by_number_of_s(["rust", "ada"])
        ['ada', 'rust']
    """
    return sorted(words, key=number_of_s)


# =============================================================================
# 4.2 map - 各要素に関数を適用
# =============================================================================


def get_lengths(words: list[str]) -> list[int]:
    """各文字列の長さを取得する。

    Examples:
        >>> get_lengths(["scala", "rust", "ada"])
        [5, 4, 3]
    """
    return [len(word) for word in words]


def get_scores(words: list[str], word_score: Callable[[str], int]) -> list[int]:
    """各単語のスコアを取得する。

    Examples:
        >>> get_scores(["rust", "java"], score)
        [4, 2]
    """
    return [word_score(word) for word in words]


def double_all(numbers: list[int]) -> list[int]:
    """全ての数値を2倍にする。

    Examples:
        >>> double_all([5, 1, 2, 4, 0])
        [10, 2, 4, 8, 0]
    """
    return [n * 2 for n in numbers]


def negate_all(numbers: list[int]) -> list[int]:
    """全ての数値を負にする。

    Examples:
        >>> negate_all([5, 1, 2, 4, 0])
        [-5, -1, -2, -4, 0]
    """
    return [-n for n in numbers]


# =============================================================================
# 4.3 filter - 条件に合う要素を抽出
# =============================================================================


def filter_short_words(words: list[str], max_length: int) -> list[str]:
    """指定した長さ以下の単語をフィルタする。

    Examples:
        >>> filter_short_words(["scala", "rust", "ada"], 4)
        ['rust', 'ada']
    """
    return [word for word in words if len(word) <= max_length]


def filter_odd(numbers: list[int]) -> list[int]:
    """奇数のみをフィルタする。

    Examples:
        >>> filter_odd([5, 1, 2, 4, 0])
        [5, 1]
    """
    return [n for n in numbers if n % 2 == 1]


def filter_larger_than(numbers: list[int], threshold: int) -> list[int]:
    """閾値より大きい数値をフィルタする。

    Examples:
        >>> filter_larger_than([5, 1, 2, 4, 0], 4)
        [5]
    """
    return [n for n in numbers if n > threshold]


def high_scoring_words(
    words: list[str],
    word_score: Callable[[str], int],
    higher_than: int,
) -> list[str]:
    """指定したスコアより高い単語をフィルタする。

    Examples:
        >>> high_scoring_words(["rust", "java"], score, 3)
        ['rust']
    """
    return [word for word in words if word_score(word) > higher_than]


# =============================================================================
# 4.4 foldLeft/reduce - 累積演算
# =============================================================================


def sum_all(numbers: list[int]) -> int:
    """数値の合計を計算する。

    Examples:
        >>> sum_all([5, 1, 2, 4, 100])
        112
    """
    return reduce(lambda acc, n: acc + n, numbers, 0)


def total_length(words: list[str]) -> int:
    """全ての文字列の長さの合計を計算する。

    Examples:
        >>> total_length(["scala", "rust", "ada"])
        12
    """
    return reduce(lambda acc, word: acc + len(word), words, 0)


def total_s_count(words: list[str]) -> int:
    """全ての文字列中の's'の数の合計を計算する。

    Examples:
        >>> total_s_count(["scala", "haskell", "rust", "ada"])
        3
    """
    return reduce(lambda acc, word: acc + number_of_s(word), words, 0)


def find_max(numbers: list[int]) -> int:
    """リスト内の最大値を見つける。

    Examples:
        >>> find_max([5, 1, 2, 4, 15])
        15
    """
    if not numbers:
        raise ValueError("Cannot find max of empty list")
    return reduce(lambda acc, n: n if n > acc else acc, numbers)


def cumulative_score(words: list[str], word_score: Callable[[str], int]) -> int:
    """全ての単語のスコアの合計を計算する。

    Examples:
        >>> cumulative_score(["rust", "java"], score)
        6
    """
    return reduce(lambda acc, word: acc + word_score(word), words, 0)


# =============================================================================
# 4.5 関数を返す関数
# =============================================================================


def larger_than(n: int) -> Callable[[int], bool]:
    """n より大きいかを判定する関数を返す。

    Examples:
        >>> larger_than(4)(5)
        True
        >>> larger_than(4)(3)
        False
        >>> list(filter(larger_than(4), [5, 1, 2, 4, 0]))
        [5]
    """
    return lambda i: i > n


def divisible_by(n: int) -> Callable[[int], bool]:
    """n で割り切れるかを判定する関数を返す。

    Examples:
        >>> divisible_by(5)(15)
        True
        >>> divisible_by(5)(7)
        False
        >>> list(filter(divisible_by(5), [5, 1, 2, 4, 15]))
        [5, 15]
    """
    return lambda i: i % n == 0


def shorter_than(n: int) -> Callable[[str], bool]:
    """長さが n より短いかを判定する関数を返す。

    Examples:
        >>> shorter_than(4)("ada")
        True
        >>> shorter_than(4)("scala")
        False
        >>> list(filter(shorter_than(4), ["scala", "ada"]))
        ['ada']
    """
    return lambda s: len(s) < n


def contains_s_more_than(count: int) -> Callable[[str], bool]:
    """'s' が count より多く含まれるかを判定する関数を返す。

    Examples:
        >>> contains_s_more_than(0)("rust")
        True
        >>> contains_s_more_than(0)("ada")
        False
        >>> list(filter(contains_s_more_than(0), ["rust", "ada"]))
        ['rust']
    """
    return lambda s: number_of_s(s) > count


# =============================================================================
# 4.6 Word Scoring の実践的な例
# =============================================================================


def bonus(word: str) -> int:
    """'c' を含む単語にボーナスを与える。

    Examples:
        >>> bonus("scala")
        5
        >>> bonus("java")
        0
    """
    return 5 if "c" in word else 0


def penalty(word: str) -> int:
    """'s' を含む単語にペナルティを与える。

    Examples:
        >>> penalty("rust")
        7
        >>> penalty("java")
        0
    """
    return 7 if "s" in word else 0


def word_score_with_bonus(word: str) -> int:
    """スコア + ボーナス を計算する。

    Examples:
        >>> word_score_with_bonus("scala")
        8
        >>> word_score_with_bonus("java")
        2
    """
    return score(word) + bonus(word)


def word_score_with_bonus_and_penalty(word: str) -> int:
    """スコア + ボーナス - ペナルティ を計算する。

    Examples:
        >>> word_score_with_bonus_and_penalty("java")
        2
        >>> word_score_with_bonus_and_penalty("scala")
        1
        >>> word_score_with_bonus_and_penalty("rust")
        -3
    """
    return score(word) + bonus(word) - penalty(word)


def high_scoring_words_with_threshold(
    word_score_fn: Callable[[str], int],
) -> Callable[[int], Callable[[list[str]], list[str]]]:
    """スコア関数を受け取り、閾値と単語リストを受け取る関数を返す。

    カリー化された関数の例。

    Examples:
        >>> words = ["ada", "haskell", "scala", "java", "rust"]
        >>> scorer = high_scoring_words_with_threshold(word_score_with_bonus_and_penalty)
        >>> scorer(1)(words)
        ['java']
        >>> scorer(0)(words)
        ['ada', 'scala', 'java']
    """
    def with_threshold(higher_than: int) -> Callable[[list[str]], list[str]]:
        def filter_words(words: list[str]) -> list[str]:
            return [word for word in words if word_score_fn(word) > higher_than]
        return filter_words
    return with_threshold


# =============================================================================
# 4.7 汎用的な高階関数
# =============================================================================


def my_map(func: Callable[[T], U], lst: list[T]) -> list[U]:
    """map 関数の実装。

    Examples:
        >>> my_map(lambda x: x * 2, [1, 2, 3])
        [2, 4, 6]
        >>> my_map(len, ["a", "bb", "ccc"])
        [1, 2, 3]
    """
    return [func(item) for item in lst]


def my_filter(predicate: Callable[[T], bool], lst: list[T]) -> list[T]:
    """filter 関数の実装。

    Examples:
        >>> my_filter(lambda x: x > 0, [-1, 0, 1, 2])
        [1, 2]
        >>> my_filter(lambda s: len(s) > 2, ["a", "bb", "ccc"])
        ['ccc']
    """
    return [item for item in lst if predicate(item)]


def my_reduce(
    func: Callable[[U, T], U],
    lst: list[T],
    initial: U,
) -> U:
    """reduce 関数の実装。

    Examples:
        >>> my_reduce(lambda acc, x: acc + x, [1, 2, 3], 0)
        6
        >>> my_reduce(lambda acc, s: acc + len(s), ["a", "bb"], 0)
        3
    """
    result = initial
    for item in lst:
        result = func(result, item)
    return result
