"""第3章: イミュータブルなデータ操作

イミュータブル（不変）なデータの操作方法を学びます。
データを「変更」する代わりに、新しいデータを「作成」します。
"""

from typing import NamedTuple


# =============================================================================
# 3.1 イミュータブルなリスト操作
# =============================================================================


def appended(lst: list[str], element: str) -> list[str]:
    """リストに要素を追加した新しいリストを返す。

    元のリストは変更されない。

    Examples:
        >>> original = ["Apple", "Book"]
        >>> new_list = appended(original, "Mango")
        >>> original
        ['Apple', 'Book']
        >>> new_list
        ['Apple', 'Book', 'Mango']
    """
    return lst + [element]


def appended_all(lst: list[str], elements: list[str]) -> list[str]:
    """リストに複数の要素を追加した新しいリストを返す。

    Examples:
        >>> appended_all(["a", "b"], ["c", "d"])
        ['a', 'b', 'c', 'd']
    """
    return lst + elements


# =============================================================================
# 3.2 スライス操作
# =============================================================================


def first_two(lst: list[str]) -> list[str]:
    """リストの最初の2要素を取得する。

    Examples:
        >>> first_two(["a", "b", "c"])
        ['a', 'b']
        >>> first_two(["a"])
        ['a']
        >>> first_two([])
        []
    """
    return lst[:2]


def last_two(lst: list[str]) -> list[str]:
    """リストの最後の2要素を取得する。

    Examples:
        >>> last_two(["a", "b", "c"])
        ['b', 'c']
        >>> last_two(["a"])
        ['a']
        >>> last_two([])
        []
    """
    if len(lst) < 2:
        return lst[:]
    return lst[-2:]


def move_first_two_to_end(lst: list[str]) -> list[str]:
    """最初の2要素を末尾に移動した新しいリストを返す。

    Examples:
        >>> move_first_two_to_end(["a", "b", "c"])
        ['c', 'a', 'b']
        >>> move_first_two_to_end(["a", "b", "c", "d"])
        ['c', 'd', 'a', 'b']
    """
    first = lst[:2]
    rest = lst[2:]
    return rest + first


def insert_before_last(lst: list[str], element: str) -> list[str]:
    """最後の要素の前に要素を挿入した新しいリストを返す。

    Examples:
        >>> insert_before_last(["a", "b"], "c")
        ['a', 'c', 'b']
        >>> insert_before_last(["a"], "b")
        ['b', 'a']
    """
    if not lst:
        return [element]
    return lst[:-1] + [element] + lst[-1:]


def insert_at_middle(lst: list[str], element: str) -> list[str]:
    """リストの中央に要素を挿入した新しいリストを返す。

    Examples:
        >>> insert_at_middle(["a", "b", "c", "d"], "X")
        ['a', 'b', 'X', 'c', 'd']
        >>> insert_at_middle(["a", "b"], "X")
        ['a', 'X', 'b']
    """
    middle = len(lst) // 2
    return lst[:middle] + [element] + lst[middle:]


# =============================================================================
# 3.3 旅程の再計画
# =============================================================================


def replan(plan: list[str], new_city: str, before_city: str) -> list[str]:
    """指定した都市の前に新しい都市を挿入した旅程を返す。

    Examples:
        >>> plan = ["Paris", "Berlin", "Kraków"]
        >>> replan(plan, "Vienna", "Kraków")
        ['Paris', 'Berlin', 'Vienna', 'Kraków']
        >>> plan  # 元の計画は変わらない
        ['Paris', 'Berlin', 'Kraków']
    """
    try:
        index = plan.index(before_city)
        return plan[:index] + [new_city] + plan[index:]
    except ValueError:
        return plan + [new_city]


# =============================================================================
# 3.4 文字列操作（String もイミュータブル）
# =============================================================================


def abbreviate(name: str) -> str:
    """名前を省略形にする。

    Examples:
        >>> abbreviate("Alonzo Church")
        'A. Church'
        >>> abbreviate("A. Church")
        'A. Church'
    """
    separator = name.find(" ")
    if separator == -1:
        return name
    initial = name[0]
    last_name = name[separator + 1:]
    return f"{initial}. {last_name}"


def substring(s: str, start: int, end: int) -> str:
    """部分文字列を取得する。

    Examples:
        >>> substring("hello", 1, 4)
        'ell'
        >>> substring("Python", 0, 2)
        'Py'
    """
    return s[start:end]


# =============================================================================
# 3.5 タプル（イミュータブルなシーケンス）
# =============================================================================


class Point(NamedTuple):
    """2次元の点（イミュータブル）。

    Examples:
        >>> p = Point(1, 2)
        >>> p.x
        1
        >>> p.y
        2
        >>> p2 = Point(p.x + 1, p.y)  # 新しいPointを作成
        >>> p2
        Point(x=2, y=2)
    """

    x: int
    y: int


class City(NamedTuple):
    """都市（イミュータブル）。

    Examples:
        >>> city = City("Tokyo", 13960000)
        >>> city.name
        'Tokyo'
        >>> city.population
        13960000
    """

    name: str
    population: int


def with_population(city: City, new_population: int) -> City:
    """人口を更新した新しい City を返す。

    Examples:
        >>> tokyo = City("Tokyo", 13960000)
        >>> tokyo_updated = with_population(tokyo, 14000000)
        >>> tokyo.population
        13960000
        >>> tokyo_updated.population
        14000000
    """
    return City(city.name, new_population)


# =============================================================================
# 3.6 frozenset（イミュータブルな集合）
# =============================================================================


def add_to_set(s: frozenset[str], element: str) -> frozenset[str]:
    """frozenset に要素を追加した新しい frozenset を返す。

    Examples:
        >>> s = frozenset({"a", "b"})
        >>> s2 = add_to_set(s, "c")
        >>> "c" in s
        False
        >>> "c" in s2
        True
    """
    return s | {element}


def remove_from_set(s: frozenset[str], element: str) -> frozenset[str]:
    """frozenset から要素を削除した新しい frozenset を返す。

    Examples:
        >>> s = frozenset({"a", "b", "c"})
        >>> s2 = remove_from_set(s, "b")
        >>> "b" in s
        True
        >>> "b" in s2
        False
    """
    return s - {element}


# =============================================================================
# 3.7 辞書のイミュータブル操作
# =============================================================================


def update_dict(d: dict[str, int], key: str, value: int) -> dict[str, int]:
    """辞書のキーを更新した新しい辞書を返す。

    Examples:
        >>> d = {"a": 1, "b": 2}
        >>> d2 = update_dict(d, "c", 3)
        >>> d
        {'a': 1, 'b': 2}
        >>> d2
        {'a': 1, 'b': 2, 'c': 3}
    """
    return {**d, key: value}


def remove_key(d: dict[str, int], key: str) -> dict[str, int]:
    """辞書からキーを削除した新しい辞書を返す。

    Examples:
        >>> d = {"a": 1, "b": 2, "c": 3}
        >>> d2 = remove_key(d, "b")
        >>> d
        {'a': 1, 'b': 2, 'c': 3}
        >>> d2
        {'a': 1, 'c': 3}
    """
    return {k: v for k, v in d.items() if k != key}
