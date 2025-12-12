"""第2章: 純粋関数とテスト

純粋関数（Pure Function）の概念を学びます。
- 同じ入力には常に同じ出力を返す
- 副作用がない（外部状態を変更しない）

純粋関数はテストが容易で、コードの予測可能性が向上します。
"""

from typing import Callable


# =============================================================================
# 2.1 純粋関数の基本
# =============================================================================


def add(a: int, b: int) -> int:
    """2つの数値を加算する純粋関数。

    Examples:
        >>> add(2, 3)
        5
        >>> add(-1, 1)
        0
    """
    return a + b


def string_length(s: str) -> int:
    """文字列の長さを返す純粋関数。

    Examples:
        >>> string_length("hello")
        5
        >>> string_length("")
        0
    """
    return len(s)


def word_score(word: str) -> int:
    """単語のスコア（文字数）を返す純粋関数。

    Examples:
        >>> word_score("Scala")
        5
        >>> word_score("Python")
        6
    """
    return len(word)


# =============================================================================
# 2.2 ボーナススコア計算
# =============================================================================


def bonus_score(word: str) -> int:
    """文字 'c' を含む場合にボーナスを付与する。

    - 'c' を含む場合: スコア + 5
    - 'c' を含まない場合: スコアのみ

    Examples:
        >>> bonus_score("Scala")
        10
        >>> bonus_score("Python")
        6
        >>> bonus_score("cat")
        8
    """
    base = len(word)
    return base + 5 if "c" in word.lower() else base


# =============================================================================
# 2.3 ショッピングカートの割引計算
# =============================================================================


def get_discount_percentage(items: list[str]) -> int:
    """アイテムリストから割引率を計算する純粋関数。

    - Book を含む場合: 5% 割引
    - Book を含まない場合: 0% 割引

    Examples:
        >>> get_discount_percentage(["Apple", "Book"])
        5
        >>> get_discount_percentage(["Apple", "Orange"])
        0
        >>> get_discount_percentage([])
        0
    """
    return 5 if "Book" in items else 0


def calculate_discount(price: float, discount_percent: int) -> float:
    """割引額を計算する純粋関数。

    Examples:
        >>> calculate_discount(100.0, 10)
        10.0
        >>> calculate_discount(100.0, 0)
        0.0
    """
    return price * discount_percent / 100


def calculate_final_price(price: float, items: list[str]) -> float:
    """最終価格を計算する純粋関数。

    Examples:
        >>> calculate_final_price(100.0, ["Apple", "Book"])
        95.0
        >>> calculate_final_price(100.0, ["Apple"])
        100.0
    """
    discount_percent = get_discount_percentage(items)
    discount = calculate_discount(price, discount_percent)
    return price - discount


# =============================================================================
# 2.4 チップ計算
# =============================================================================


def get_tip_percentage(names: list[str]) -> int:
    """グループの人数からチップ率を計算する純粋関数。

    - 6人以上: 20%
    - 1-5人: 10%
    - 0人: 0%

    Examples:
        >>> get_tip_percentage(["Alice", "Bob", "Charlie", "Dave", "Eve", "Frank"])
        20
        >>> get_tip_percentage(["Alice", "Bob"])
        10
        >>> get_tip_percentage([])
        0
    """
    size = len(names)
    if size > 5:
        return 20
    elif size > 0:
        return 10
    else:
        return 0


def calculate_tip(bill: float, names: list[str]) -> float:
    """チップ額を計算する純粋関数。

    Examples:
        >>> calculate_tip(100.0, ["Alice", "Bob"])
        10.0
        >>> calculate_tip(100.0, ["A", "B", "C", "D", "E", "F"])
        20.0
    """
    tip_percent = get_tip_percentage(names)
    return bill * tip_percent / 100


# =============================================================================
# 2.5 参照透過性の例
# =============================================================================


def referential_transparency_example() -> bool:
    """参照透過性の例を示す。

    純粋関数は参照透過性を持つ：
    式をその評価結果で置き換えても、プログラムの意味が変わらない。

    Examples:
        >>> referential_transparency_example()
        True
    """
    # 以下の2つは同等
    score1 = word_score("Scala")
    score2 = word_score("Scala")

    # word_score("Scala") は常に 5 を返すので、
    # 以下のように置き換えても結果は同じ
    total1 = word_score("Scala") + word_score("Java")
    total2 = 5 + 4  # word_score の結果で置き換え

    return score1 == score2 == 5 and total1 == total2 == 9


# =============================================================================
# 2.6 文字列操作の純粋関数
# =============================================================================


def append_exclamation(s: str) -> str:
    """文字列に感嘆符を追加する。

    Examples:
        >>> append_exclamation("Hello")
        'Hello!'
        >>> append_exclamation("")
        '!'
    """
    return s + "!"


def count_vowels(s: str) -> int:
    """母音の数をカウントする。

    Examples:
        >>> count_vowels("hello")
        2
        >>> count_vowels("Python")
        1
        >>> count_vowels("xyz")
        0
    """
    vowels = "aeiouAEIOU"
    return sum(1 for c in s if c in vowels)


# =============================================================================
# 2.7 リスト操作の純粋関数
# =============================================================================


def double_all(numbers: list[int]) -> list[int]:
    """リストの全ての要素を2倍にする。

    Examples:
        >>> double_all([1, 2, 3])
        [2, 4, 6]
        >>> double_all([])
        []
    """
    return [n * 2 for n in numbers]


def filter_positive(numbers: list[int]) -> list[int]:
    """正の数のみをフィルタリングする。

    Examples:
        >>> filter_positive([1, -2, 3, -4, 5])
        [1, 3, 5]
        >>> filter_positive([-1, -2])
        []
    """
    return [n for n in numbers if n > 0]


def find_longest(words: list[str]) -> str:
    """最も長い文字列を見つける。

    空のリストの場合は空文字列を返す。

    Examples:
        >>> find_longest(["apple", "banana", "cherry"])
        'banana'
        >>> find_longest(["a", "bb", "ccc"])
        'ccc'
        >>> find_longest([])
        ''
    """
    if not words:
        return ""
    return max(words, key=len)


def all_positive(numbers: list[int]) -> bool:
    """全ての要素が正の数かどうかを判定する。

    空リストの場合は True を返す（vacuous truth）。

    Examples:
        >>> all_positive([1, 2, 3])
        True
        >>> all_positive([1, -2, 3])
        False
        >>> all_positive([])
        True
    """
    return all(n > 0 for n in numbers)


def any_negative(numbers: list[int]) -> bool:
    """負の数が含まれているかどうかを判定する。

    Examples:
        >>> any_negative([1, -2, 3])
        True
        >>> any_negative([1, 2, 3])
        False
        >>> any_negative([])
        False
    """
    return any(n < 0 for n in numbers)


def sum_list(numbers: list[int]) -> int:
    """リストの合計を計算する。

    Examples:
        >>> sum_list([1, 2, 3, 4, 5])
        15
        >>> sum_list([])
        0
    """
    return sum(numbers)


def average(numbers: list[float]) -> float | None:
    """リストの平均を計算する。

    空リストの場合は None を返す。

    Examples:
        >>> average([1.0, 2.0, 3.0, 4.0, 5.0])
        3.0
        >>> average([]) is None
        True
    """
    if not numbers:
        return None
    return sum(numbers) / len(numbers)


# =============================================================================
# 2.8 文字 'a' を除外するワードスコア
# =============================================================================


def word_score_no_a(word: str) -> int:
    """文字 'a' を除外してワードスコアを計算する。

    Examples:
        >>> word_score_no_a("Scala")
        3
        >>> word_score_no_a("function")
        8
        >>> word_score_no_a("")
        0
        >>> word_score_no_a("aaa")
        0
    """
    return len(word.replace("a", "").replace("A", ""))


# =============================================================================
# 2.9 高階関数の基本
# =============================================================================


def apply_twice(f: Callable[[int], int], x: int) -> int:
    """関数を2回適用する。

    Examples:
        >>> apply_twice(lambda x: x + 1, 5)
        7
        >>> apply_twice(lambda x: x * 2, 3)
        12
    """
    return f(f(x))


def compose(f: Callable[[int], int], g: Callable[[int], int]) -> Callable[[int], int]:
    """2つの関数を合成する。

    compose(f, g)(x) = f(g(x))

    Examples:
        >>> double = lambda x: x * 2
        >>> add_one = lambda x: x + 1
        >>> double_then_add = compose(add_one, double)
        >>> double_then_add(5)
        11
    """
    return lambda x: f(g(x))


# =============================================================================
# 2.10 タイムスタンプフォーマット（純粋な部分のみ）
# =============================================================================


def format_timestamp(year: int, month: int, day: int) -> str:
    """タイムスタンプを文字列にフォーマットする純粋関数。

    Examples:
        >>> format_timestamp(2024, 1, 15)
        '2024-01-15'
        >>> format_timestamp(2024, 12, 31)
        '2024-12-31'
    """
    return f"{year:04d}-{month:02d}-{day:02d}"
