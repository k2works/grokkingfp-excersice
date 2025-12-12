"""第6章: Maybe 型による安全なエラーハンドリング

Python の returns ライブラリを使って、Option 型（Maybe）による
安全なエラーハンドリングを学びます。
null や例外に頼らず、型安全にエラーを扱う方法を習得します。
"""

from dataclasses import dataclass
from typing import TypeVar

from returns.maybe import Maybe, Nothing, Some

T = TypeVar("T")


# =============================================================================
# 6.1 なぜ Maybe が必要か
# =============================================================================


# 従来の方法（問題あり）- 例外を使う
def unsafe_parse_int(s: str) -> int:
    """文字列を整数に変換する（例外をスローする可能性あり）。

    Examples:
        >>> unsafe_parse_int("42")
        42
        >>> # unsafe_parse_int("abc")  # ValueError が発生!
    """
    return int(s)


# Maybe を使う方法（安全）
def parse_int(s: str) -> Maybe[int]:
    """文字列を整数に安全に変換する。

    Examples:
        >>> parse_int("42")
        <Some: 42>
        >>> parse_int("abc")
        <Nothing>
    """
    try:
        return Some(int(s))
    except ValueError:
        return Nothing


# =============================================================================
# 6.2 TV番組のパース例
# =============================================================================


@dataclass(frozen=True)
class TvShow:
    """TV番組を表すイミュータブルなデータクラス。

    Examples:
        >>> show = TvShow("Breaking Bad", 2008, 2013)
        >>> show.title
        'Breaking Bad'
    """

    title: str
    start: int
    end: int


def extract_name(raw_show: str) -> Maybe[str]:
    """番組名を抽出する。

    Examples:
        >>> extract_name("Breaking Bad (2008-2013)")
        <Some: Breaking Bad>
        >>> extract_name("(2008-2013)")
        <Nothing>
    """
    bracket_open = raw_show.find("(")
    if bracket_open > 0:
        return Some(raw_show[:bracket_open].strip())
    return Nothing


def extract_year_start(raw_show: str) -> Maybe[int]:
    """開始年を抽出する。

    Examples:
        >>> extract_year_start("Breaking Bad (2008-2013)")
        <Some: 2008>
        >>> extract_year_start("Breaking Bad ()")
        <Nothing>
    """
    bracket_open = raw_show.find("(")
    dash = raw_show.find("-")
    if bracket_open != -1 and dash > bracket_open + 1:
        year_str = raw_show[bracket_open + 1 : dash]
        return parse_int(year_str)
    return Nothing


def extract_year_end(raw_show: str) -> Maybe[int]:
    """終了年を抽出する。

    Examples:
        >>> extract_year_end("Breaking Bad (2008-2013)")
        <Some: 2013>
        >>> extract_year_end("Breaking Bad (2008-)")
        <Nothing>
    """
    dash = raw_show.find("-")
    bracket_close = raw_show.find(")")
    if dash != -1 and bracket_close > dash + 1:
        year_str = raw_show[dash + 1 : bracket_close]
        return parse_int(year_str)
    return Nothing


def extract_single_year(raw_show: str) -> Maybe[int]:
    """単年の番組から年を抽出する。

    Examples:
        >>> extract_single_year("Chernobyl (2019)")
        <Some: 2019>
        >>> extract_single_year("Breaking Bad (2008-2013)")
        <Nothing>
    """
    dash = raw_show.find("-")
    bracket_open = raw_show.find("(")
    bracket_close = raw_show.find(")")
    if dash == -1 and bracket_open != -1 and bracket_close > bracket_open + 1:
        year_str = raw_show[bracket_open + 1 : bracket_close]
        return parse_int(year_str)
    return Nothing


def parse_show(raw_show: str) -> Maybe[TvShow]:
    """TV番組の文字列をパースする。

    Examples:
        >>> parse_show("Breaking Bad (2008-2013)")
        <Some: TvShow(title='Breaking Bad', start=2008, end=2013)>
        >>> parse_show("Chernobyl (2019)")
        <Some: TvShow(title='Chernobyl', start=2019, end=2019)>
        >>> parse_show("Invalid")
        <Nothing>
    """
    name = extract_name(raw_show)
    year_start = extract_year_start(raw_show).lash(lambda _: extract_single_year(raw_show))
    year_end = extract_year_end(raw_show).lash(lambda _: extract_single_year(raw_show))

    return name.bind(
        lambda n: year_start.bind(
            lambda s: year_end.map(lambda e: TvShow(n, s, e))
        )
    )


# =============================================================================
# 6.3 Maybe の主要メソッド
# =============================================================================


def safe_divide(a: int, b: int) -> Maybe[int]:
    """安全な除算。

    Examples:
        >>> safe_divide(10, 2)
        <Some: 5>
        >>> safe_divide(10, 0)
        <Nothing>
    """
    if b == 0:
        return Nothing
    return Some(a // b)


def safe_head(lst: list[T]) -> Maybe[T]:
    """リストの最初の要素を安全に取得する。

    Examples:
        >>> safe_head([1, 2, 3])
        <Some: 1>
        >>> safe_head([])
        <Nothing>
    """
    if not lst:
        return Nothing
    return Some(lst[0])


def safe_find(lst: list[T], predicate) -> Maybe[T]:
    """条件を満たす最初の要素を安全に見つける。

    Examples:
        >>> safe_find([1, 2, 3, 4], lambda x: x > 2)
        <Some: 3>
        >>> safe_find([1, 2, 3, 4], lambda x: x > 10)
        <Nothing>
    """
    for item in lst:
        if predicate(item):
            return Some(item)
    return Nothing


# =============================================================================
# 6.4 Maybe の合成
# =============================================================================


def add_strings(a: str, b: str) -> Maybe[int]:
    """2つの数値文字列を加算する。

    Examples:
        >>> add_strings("10", "20")
        <Some: 30>
        >>> add_strings("10", "abc")
        <Nothing>
    """
    return parse_int(a).bind(lambda x: parse_int(b).map(lambda y: x + y))


def chain_operations(value: int) -> Maybe[int]:
    """複数の操作を連鎖させる。

    Examples:
        >>> chain_operations(100)
        <Some: 10>
        >>> chain_operations(7)
        <Nothing>
    """
    return (
        safe_divide(value, 2)
        .bind(lambda x: safe_divide(x, 5) if x > 10 else Nothing)
    )


# =============================================================================
# 6.5 エラーハンドリング戦略
# =============================================================================


def parse_shows_best_effort(raw_shows: list[str]) -> list[TvShow]:
    """パースできたものだけ返す（Best-effort戦略）。

    Examples:
        >>> shows = ["Breaking Bad (2008-2013)", "Invalid", "Mad Men (2007-2015)"]
        >>> result = parse_shows_best_effort(shows)
        >>> len(result)
        2
        >>> result[0].title
        'Breaking Bad'
    """
    results = []
    for raw in raw_shows:
        parsed = parse_show(raw)
        if isinstance(parsed, Some):
            results.append(parsed.unwrap())
    return results


def parse_shows_all_or_nothing(raw_shows: list[str]) -> Maybe[list[TvShow]]:
    """全部成功するか、全部失敗するか（All-or-nothing戦略）。

    Examples:
        >>> shows = ["Breaking Bad (2008-2013)", "Mad Men (2007-2015)"]
        >>> result = parse_shows_all_or_nothing(shows)
        >>> isinstance(result, Some)
        True
        >>> shows = ["Breaking Bad (2008-2013)", "Invalid"]
        >>> result = parse_shows_all_or_nothing(shows)
        >>> result == Nothing
        True
    """
    results: list[TvShow] = []
    for raw in raw_shows:
        parsed = parse_show(raw)
        if isinstance(parsed, Some):
            results.append(parsed.unwrap())
        else:
            return Nothing
    return Some(results)


# =============================================================================
# 6.6 getOrElse と orElse
# =============================================================================


def get_value_or_default(maybe: Maybe[int], default: int) -> int:
    """Maybe から値を取得するか、デフォルト値を返す。

    Examples:
        >>> get_value_or_default(Some(42), 0)
        42
        >>> get_value_or_default(Nothing, 0)
        0
    """
    return maybe.value_or(default)


def try_first_then_second(first: Maybe[int], second: Maybe[int]) -> Maybe[int]:
    """最初の Maybe が Nothing なら、2番目を試す。

    Examples:
        >>> try_first_then_second(Some(1), Some(2))
        <Some: 1>
        >>> try_first_then_second(Nothing, Some(2))
        <Some: 2>
        >>> try_first_then_second(Nothing, Nothing)
        <Nothing>
    """
    return first.lash(lambda _: second)


# =============================================================================
# 6.7 Maybe と例外の比較
# =============================================================================


def safe_get_nested_value(
    data: dict, key1: str, key2: str, key3: str
) -> Maybe[str]:
    """ネストした辞書から値を安全に取得する。

    Examples:
        >>> data = {"a": {"b": {"c": "value"}}}
        >>> safe_get_nested_value(data, "a", "b", "c")
        <Some: value>
        >>> safe_get_nested_value(data, "a", "x", "c")
        <Nothing>
    """

    def safe_get(d: dict, key: str) -> Maybe:
        if isinstance(d, dict) and key in d:
            return Some(d[key])
        return Nothing

    return (
        safe_get(data, key1)
        .bind(lambda v1: safe_get(v1, key2))
        .bind(lambda v2: safe_get(v2, key3))
    )


# =============================================================================
# 6.8 実用的なユーティリティ
# =============================================================================


def filter_some(maybes: list[Maybe[T]]) -> list[T]:
    """Maybe のリストから Some の値だけを抽出する。

    Examples:
        >>> filter_some([Some(1), Nothing, Some(3), Nothing])
        [1, 3]
    """
    return [m.unwrap() for m in maybes if isinstance(m, Some)]


def first_some(maybes: list[Maybe[T]]) -> Maybe[T]:
    """最初の Some を返す。

    Examples:
        >>> first_some([Nothing, Nothing, Some(3), Some(4)])
        <Some: 3>
        >>> first_some([Nothing, Nothing])
        <Nothing>
    """
    for m in maybes:
        if isinstance(m, Some):
            return m
    return Nothing


def sequence_maybes(maybes: list[Maybe[T]]) -> Maybe[list[T]]:
    """全ての Maybe が Some なら、値のリストを返す。

    Examples:
        >>> sequence_maybes([Some(1), Some(2), Some(3)])
        <Some: [1, 2, 3]>
        >>> sequence_maybes([Some(1), Nothing, Some(3)])
        <Nothing>
    """
    results: list[T] = []
    for m in maybes:
        if isinstance(m, Some):
            results.append(m.unwrap())
        else:
            return Nothing
    return Some(results)
