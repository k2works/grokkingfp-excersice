"""第9章: ストリーム処理

Python のジェネレータと itertools を使った遅延評価ストリーム処理を学びます。
無限シーケンス、スライディングウィンドウ、リアルタイムデータ処理を習得します。
"""

import random
import time
from collections import deque
from collections.abc import Callable, Generator, Iterator
from dataclasses import dataclass
from decimal import Decimal
from enum import Enum
from itertools import count, islice, repeat, takewhile
from typing import TypeVar

T = TypeVar("T")
U = TypeVar("U")


# =============================================================================
# 9.1 ストリームの基本
# =============================================================================


def stream_of(*args: T) -> Generator[T, None, None]:
    """有限ストリームを作成する。

    Examples:
        >>> list(stream_of(1, 2, 3))
        [1, 2, 3]
    """
    yield from args


def stream_from_list(lst: list[T]) -> Generator[T, None, None]:
    """リストからストリームを作成する。

    Examples:
        >>> list(stream_from_list([1, 2, 3]))
        [1, 2, 3]
    """
    yield from lst


def stream_range(start: int, end: int) -> Generator[int, None, None]:
    """範囲のストリームを作成する。

    Examples:
        >>> list(stream_range(1, 5))
        [1, 2, 3, 4]
    """
    yield from range(start, end)


# =============================================================================
# 9.2 無限ストリーム
# =============================================================================


def infinite_stream(start: int = 0) -> Generator[int, None, None]:
    """無限の整数ストリームを作成する。

    Examples:
        >>> list(islice(infinite_stream(5), 3))
        [5, 6, 7]
    """
    yield from count(start)


def repeat_stream(value: T) -> Generator[T, None, None]:
    """同じ値を無限に繰り返すストリーム。

    Examples:
        >>> list(islice(repeat_stream(42), 3))
        [42, 42, 42]
    """
    yield from repeat(value)


def cycle_stream(items: list[T]) -> Generator[T, None, None]:
    """リストの要素を無限に繰り返すストリーム。

    Examples:
        >>> list(islice(cycle_stream([1, 2, 3]), 8))
        [1, 2, 3, 1, 2, 3, 1, 2]
    """
    while True:
        yield from items


def alternating_stream() -> Generator[bool, None, None]:
    """True と False を交互に返す無限ストリーム。

    Examples:
        >>> list(islice(alternating_stream(), 5))
        [True, False, True, False, True]
    """
    yield from cycle_stream([True, False])


# =============================================================================
# 9.3 ストリームの操作
# =============================================================================


def stream_take(stream: Iterator[T], n: int) -> Generator[T, None, None]:
    """ストリームから最初の n 要素を取得する。

    Examples:
        >>> list(stream_take(infinite_stream(), 5))
        [0, 1, 2, 3, 4]
    """
    yield from islice(stream, n)


def stream_filter(stream: Iterator[T], predicate: Callable[[T], bool]) -> Generator[T, None, None]:
    """条件を満たす要素だけを返すストリーム。

    Examples:
        >>> list(stream_take(stream_filter(infinite_stream(), lambda x: x % 2 == 0), 5))
        [0, 2, 4, 6, 8]
    """
    for item in stream:
        if predicate(item):
            yield item


def stream_map(stream: Iterator[T], f: Callable[[T], U]) -> Generator[U, None, None]:
    """各要素を変換するストリーム。

    Examples:
        >>> list(stream_take(stream_map(infinite_stream(), lambda x: x * 2), 5))
        [0, 2, 4, 6, 8]
    """
    for item in stream:
        yield f(item)


def stream_take_while(stream: Iterator[T], predicate: Callable[[T], bool]) -> Generator[T, None, None]:
    """条件を満たす間だけ要素を返すストリーム。

    Examples:
        >>> list(stream_take_while(infinite_stream(), lambda x: x < 5))
        [0, 1, 2, 3, 4]
    """
    yield from takewhile(predicate, stream)


def stream_drop(stream: Iterator[T], n: int) -> Generator[T, None, None]:
    """最初の n 要素をスキップするストリーム。

    Examples:
        >>> list(stream_take(stream_drop(infinite_stream(), 5), 3))
        [5, 6, 7]
    """
    for i, item in enumerate(stream):
        if i >= n:
            yield item


def stream_drop_while(stream: Iterator[T], predicate: Callable[[T], bool]) -> Generator[T, None, None]:
    """条件を満たす間スキップし、その後の要素を返すストリーム。

    Examples:
        >>> list(stream_take(stream_drop_while(infinite_stream(), lambda x: x < 5), 3))
        [5, 6, 7]
    """
    dropping = True
    for item in stream:
        if dropping and predicate(item):
            continue
        dropping = False
        yield item


# =============================================================================
# 9.4 ストリームの結合
# =============================================================================


def stream_append(stream1: Iterator[T], stream2: Iterator[T]) -> Generator[T, None, None]:
    """2つのストリームを連結する。

    Examples:
        >>> list(stream_append(stream_of(1, 2), stream_of(3, 4)))
        [1, 2, 3, 4]
    """
    yield from stream1
    yield from stream2


def stream_zip(stream1: Iterator[T], stream2: Iterator[U]) -> Generator[tuple[T, U], None, None]:
    """2つのストリームを zip する。

    Examples:
        >>> list(stream_zip(stream_of(1, 2, 3), stream_of('a', 'b', 'c')))
        [(1, 'a'), (2, 'b'), (3, 'c')]
    """
    yield from zip(stream1, stream2)


def stream_zip_with_index(stream: Iterator[T]) -> Generator[tuple[int, T], None, None]:
    """ストリームにインデックスを付ける。

    Examples:
        >>> list(stream_zip_with_index(stream_of('a', 'b', 'c')))
        [(0, 'a'), (1, 'b'), (2, 'c')]
    """
    yield from enumerate(stream)


def stream_interleave(stream1: Iterator[T], stream2: Iterator[T]) -> Generator[T, None, None]:
    """2つのストリームを交互に結合する。

    Examples:
        >>> list(stream_take(stream_interleave(stream_of(1, 2, 3), stream_of(10, 20, 30)), 6))
        [1, 10, 2, 20, 3, 30]
    """
    for a, b in zip(stream1, stream2):
        yield a
        yield b


# =============================================================================
# 9.5 スライディングウィンドウ
# =============================================================================


def stream_sliding(stream: Iterator[T], window_size: int) -> Generator[list[T], None, None]:
    """スライディングウィンドウを生成する。

    Examples:
        >>> list(stream_sliding(stream_of(1, 2, 3, 4, 5), 3))
        [[1, 2, 3], [2, 3, 4], [3, 4, 5]]
    """
    window: deque[T] = deque(maxlen=window_size)
    for item in stream:
        window.append(item)
        if len(window) == window_size:
            yield list(window)


def stream_chunked(stream: Iterator[T], chunk_size: int) -> Generator[list[T], None, None]:
    """ストリームをチャンクに分割する。

    Examples:
        >>> list(stream_chunked(stream_of(1, 2, 3, 4, 5, 6, 7), 3))
        [[1, 2, 3], [4, 5, 6], [7]]
    """
    chunk: list[T] = []
    for item in stream:
        chunk.append(item)
        if len(chunk) == chunk_size:
            yield chunk
            chunk = []
    if chunk:
        yield chunk


# =============================================================================
# 9.6 サイコロの例
# =============================================================================


def cast_die_stream() -> Generator[int, None, None]:
    """サイコロを無限に振るストリーム。

    Examples:
        >>> results = list(stream_take(cast_die_stream(), 10))
        >>> all(1 <= r <= 6 for r in results)
        True
    """
    while True:
        yield random.randint(1, 6)


def first_six() -> Generator[int, None, None]:
    """6 が出るまでサイコロを振り続ける。

    Examples:
        >>> results = list(first_six())
        >>> results[-1] == 6
        True
    """
    for die in cast_die_stream():
        yield die
        if die == 6:
            break


def die_sum_until(target: int) -> Generator[tuple[int, int], None, None]:
    """合計が target に達するまでサイコロを振る。

    Examples:
        >>> results = list(die_sum_until(20))
        >>> results[-1][1] >= 20
        True
    """
    total = 0
    for die in cast_die_stream():
        total += die
        yield (die, total)
        if total >= target:
            break


# =============================================================================
# 9.7 通貨交換レートの例
# =============================================================================


class Currency(Enum):
    """通貨を表す列挙型。"""

    USD = "USD"
    EUR = "EUR"
    JPY = "JPY"
    GBP = "GBP"


@dataclass(frozen=True)
class ExchangeRate:
    """為替レートを表すデータクラス。"""

    from_currency: Currency
    to_currency: Currency
    rate: Decimal


# シミュレーション用の為替レートデータ
_base_rates: dict[tuple[Currency, Currency], Decimal] = {
    (Currency.USD, Currency.EUR): Decimal("0.85"),
    (Currency.USD, Currency.JPY): Decimal("110.0"),
    (Currency.USD, Currency.GBP): Decimal("0.73"),
    (Currency.EUR, Currency.USD): Decimal("1.18"),
    (Currency.EUR, Currency.JPY): Decimal("130.0"),
    (Currency.EUR, Currency.GBP): Decimal("0.86"),
}


def get_exchange_rate(from_currency: Currency, to_currency: Currency) -> Decimal:
    """為替レートを取得する（シミュレーション）。

    Examples:
        >>> rate = get_exchange_rate(Currency.USD, Currency.EUR)
        >>> rate > 0
        True
    """
    base = _base_rates.get((from_currency, to_currency), Decimal("1.0"))
    # ランダムな変動を加える
    variation = Decimal(str(random.uniform(-0.02, 0.02)))
    return base + variation


def exchange_rate_stream(
    from_currency: Currency, to_currency: Currency
) -> Generator[Decimal, None, None]:
    """為替レートを継続的に取得するストリーム。

    Examples:
        >>> rates = list(stream_take(exchange_rate_stream(Currency.USD, Currency.EUR), 3))
        >>> all(isinstance(r, Decimal) for r in rates)
        True
    """
    while True:
        yield get_exchange_rate(from_currency, to_currency)


def trending(rates: list[Decimal]) -> bool:
    """レートが上昇トレンドかどうかを判定する。

    Examples:
        >>> trending([Decimal('0.81'), Decimal('0.82'), Decimal('0.83')])
        True
        >>> trending([Decimal('0.81'), Decimal('0.84'), Decimal('0.83')])
        False
    """
    if len(rates) < 2:
        return False
    return all(rates[i] < rates[i + 1] for i in range(len(rates) - 1))


def is_stable(values: list[Decimal]) -> bool:
    """レートが安定しているかどうかを判定する。

    Examples:
        >>> is_stable([Decimal('0.81'), Decimal('0.81'), Decimal('0.81')])
        True
        >>> is_stable([Decimal('0.81'), Decimal('0.82'), Decimal('0.81')])
        False
    """
    if len(values) < 2:
        return False
    return len(set(values)) == 1


def find_trending_window(
    from_currency: Currency, to_currency: Currency, window_size: int = 3
) -> list[Decimal]:
    """上昇トレンドのウィンドウを見つける。

    Examples:
        >>> result = find_trending_window(Currency.USD, Currency.EUR, 3)
        >>> len(result) >= 0  # トレンドが見つからない場合もある
        True
    """
    rate_stream = exchange_rate_stream(from_currency, to_currency)
    windows = stream_sliding(rate_stream, window_size)

    for window in stream_take(windows, 100):  # 最大100回試行
        if trending(window):
            return window
    return []


def exchange_if_trending(
    amount: Decimal, from_currency: Currency, to_currency: Currency, window_size: int = 3
) -> Decimal | None:
    """上昇トレンドを検出したら交換する。

    Examples:
        >>> result = exchange_if_trending(Decimal('100'), Currency.USD, Currency.EUR)
        >>> result is None or result > 0
        True
    """
    trending_window = find_trending_window(from_currency, to_currency, window_size)
    if trending_window:
        return amount * trending_window[-1]
    return None


# =============================================================================
# 9.8 遅延付きストリーム
# =============================================================================


def stream_with_delay(stream: Iterator[T], delay_seconds: float) -> Generator[T, None, None]:
    """各要素の間に遅延を挿入するストリーム。

    Examples:
        >>> import time
        >>> start = time.time()
        >>> list(stream_take(stream_with_delay(stream_of(1, 2, 3), 0.01), 3))
        [1, 2, 3]
    """
    for item in stream:
        yield item
        time.sleep(delay_seconds)


def ticks(interval_seconds: float) -> Generator[int, None, None]:
    """一定間隔で tick を発生させるストリーム。

    Examples:
        >>> import time
        >>> start = time.time()
        >>> list(stream_take(ticks(0.01), 3))
        [0, 1, 2]
    """
    tick = 0
    while True:
        yield tick
        tick += 1
        time.sleep(interval_seconds)


def rate_limited_stream(stream: Iterator[T], interval_seconds: float) -> Generator[T, None, None]:
    """レート制限付きストリーム。

    Examples:
        >>> list(stream_take(rate_limited_stream(infinite_stream(), 0.01), 3))
        [0, 1, 2]
    """
    for item, _ in zip(stream, ticks(interval_seconds)):
        yield item


# =============================================================================
# 9.9 ストリームの集約
# =============================================================================


def stream_sum(stream: Iterator[int | float | Decimal]) -> int | float | Decimal:
    """ストリームの合計を計算する。

    Examples:
        >>> stream_sum(stream_of(1, 2, 3, 4, 5))
        15
    """
    return sum(stream)


def stream_reduce(
    stream: Iterator[T], f: Callable[[U, T], U], initial: U
) -> U:
    """ストリームを畳み込む。

    Examples:
        >>> stream_reduce(stream_of(1, 2, 3, 4), lambda acc, x: acc + x, 0)
        10
    """
    result = initial
    for item in stream:
        result = f(result, item)
    return result


def stream_count(stream: Iterator[T]) -> int:
    """ストリームの要素数をカウントする。

    Examples:
        >>> stream_count(stream_of(1, 2, 3, 4, 5))
        5
    """
    return sum(1 for _ in stream)


def stream_find(stream: Iterator[T], predicate: Callable[[T], bool]) -> T | None:
    """条件を満たす最初の要素を見つける。

    Examples:
        >>> stream_find(stream_of(1, 2, 3, 4, 5), lambda x: x > 3)
        4
        >>> stream_find(stream_of(1, 2, 3), lambda x: x > 10)
    """
    for item in stream:
        if predicate(item):
            return item
    return None


def stream_exists(stream: Iterator[T], predicate: Callable[[T], bool]) -> bool:
    """条件を満たす要素が存在するかを判定する。

    Examples:
        >>> stream_exists(stream_of(1, 2, 3, 4, 5), lambda x: x > 3)
        True
        >>> stream_exists(stream_of(1, 2, 3), lambda x: x > 10)
        False
    """
    return stream_find(stream, predicate) is not None


def stream_forall(stream: Iterator[T], predicate: Callable[[T], bool]) -> bool:
    """全ての要素が条件を満たすかを判定する。

    Examples:
        >>> stream_forall(stream_of(2, 4, 6), lambda x: x % 2 == 0)
        True
        >>> stream_forall(stream_of(2, 3, 6), lambda x: x % 2 == 0)
        False
    """
    for item in stream:
        if not predicate(item):
            return False
    return True
