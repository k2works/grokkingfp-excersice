"""第1章: 関数型プログラミング入門

関数型プログラミングの基本概念を Python で学びます。
命令型プログラミングとの違いを理解し、純粋関数の利点を実感することが目標です。
"""

from typing import Final


# =============================================================================
# 1.1 命令型 vs 関数型
# =============================================================================


def calculate_score_imperative(word: str) -> int:
    """命令型でワードスコアを計算する。

    命令型プログラミングは「どうやるか（HOW）」を記述します。

    Examples:
        >>> calculate_score_imperative("hello")
        5
        >>> calculate_score_imperative("")
        0
    """
    score = 0
    for _ in word:
        score += 1
    return score


def word_score(word: str) -> int:
    """関数型でワードスコアを計算する。

    関数型プログラミングは「何をするか（WHAT）」を記述します。

    Examples:
        >>> word_score("hello")
        5
        >>> word_score("Scala")
        5
        >>> word_score("")
        0
    """
    return len(word)


# =============================================================================
# 1.2 基本的な関数定義
# =============================================================================


def increment(x: int) -> int:
    """値を1増加させる。

    Examples:
        >>> increment(5)
        6
        >>> increment(0)
        1
        >>> increment(-1)
        0
    """
    return x + 1


def get_first_character(s: str) -> str:
    """文字列の最初の文字を取得する。

    Examples:
        >>> get_first_character("hello")
        'h'
        >>> get_first_character("Python")
        'P'
    """
    return s[0]


def add(a: int, b: int) -> int:
    """2つの数値を加算する。

    Examples:
        >>> add(2, 3)
        5
        >>> add(-1, 1)
        0
    """
    return a + b


def double(x: int) -> int:
    """値を2倍にする。

    Examples:
        >>> double(5)
        10
        >>> double(0)
        0
        >>> double(-3)
        -6
    """
    return x * 2


def greet(name: str) -> str:
    """挨拶メッセージを生成する。

    Examples:
        >>> greet("World")
        'Hello, World!'
        >>> greet("Python")
        'Hello, Python!'
    """
    return f"Hello, {name}!"


def to_uppercase(s: str) -> str:
    """文字列を大文字に変換する。

    Examples:
        >>> to_uppercase("hello")
        'HELLO'
        >>> to_uppercase("Python")
        'PYTHON'
    """
    return s.upper()


def reverse_string(s: str) -> str:
    """文字列を反転する。

    Examples:
        >>> reverse_string("hello")
        'olleh'
        >>> reverse_string("Python")
        'nohtyP'
    """
    return s[::-1]


# =============================================================================
# 1.3 条件分岐を含む関数
# =============================================================================


def absolute(x: int) -> int:
    """絶対値を返す。

    Examples:
        >>> absolute(5)
        5
        >>> absolute(-5)
        5
        >>> absolute(0)
        0
    """
    return x if x >= 0 else -x


def max_value(a: int, b: int) -> int:
    """2つの値のうち大きい方を返す。

    Examples:
        >>> max_value(3, 5)
        5
        >>> max_value(5, 3)
        5
        >>> max_value(3, 3)
        3
    """
    return a if a >= b else b


def min_value(a: int, b: int) -> int:
    """2つの値のうち小さい方を返す。

    Examples:
        >>> min_value(3, 5)
        3
        >>> min_value(5, 3)
        3
        >>> min_value(3, 3)
        3
    """
    return a if a <= b else b


def clamp(value: int, min_val: int, max_val: int) -> int:
    """値を指定された範囲内に収める。

    Examples:
        >>> clamp(5, 0, 10)
        5
        >>> clamp(-5, 0, 10)
        0
        >>> clamp(15, 0, 10)
        10
    """
    return max_value(min_val, min_value(value, max_val))


# =============================================================================
# 1.4 述語関数（真偽値を返す関数）
# =============================================================================


def is_even(n: int) -> bool:
    """偶数かどうかを判定する。

    Examples:
        >>> is_even(4)
        True
        >>> is_even(3)
        False
        >>> is_even(0)
        True
        >>> is_even(-2)
        True
    """
    return n % 2 == 0


def is_positive(n: int) -> bool:
    """正の数かどうかを判定する。

    Examples:
        >>> is_positive(5)
        True
        >>> is_positive(0)
        False
        >>> is_positive(-3)
        False
    """
    return n > 0


def is_empty(s: str) -> bool:
    """文字列が空かどうかを判定する。

    Examples:
        >>> is_empty("")
        True
        >>> is_empty("hello")
        False
    """
    return len(s) == 0


# =============================================================================
# 1.5 定数と型ヒント
# =============================================================================

# Final を使用した定数定義
MAX_SCORE: Final[int] = 100
MIN_SCORE: Final[int] = 0
DEFAULT_GREETING: Final[str] = "Hello"


def get_bounded_score(score: int) -> int:
    """スコアを有効な範囲内に収める。

    Examples:
        >>> get_bounded_score(50)
        50
        >>> get_bounded_score(150)
        100
        >>> get_bounded_score(-10)
        0
    """
    return clamp(score, MIN_SCORE, MAX_SCORE)
