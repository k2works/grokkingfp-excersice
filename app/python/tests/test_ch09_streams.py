"""第9章: ストリーム処理のテスト"""

from decimal import Decimal

from grokking_fp.ch09_streams import (
    Currency,
    alternating_stream,
    cast_die_stream,
    cycle_stream,
    die_sum_until,
    exchange_rate_stream,
    first_six,
    infinite_stream,
    is_stable,
    repeat_stream,
    stream_append,
    stream_chunked,
    stream_count,
    stream_drop,
    stream_drop_while,
    stream_exists,
    stream_filter,
    stream_find,
    stream_forall,
    stream_from_list,
    stream_interleave,
    stream_map,
    stream_of,
    stream_range,
    stream_reduce,
    stream_sliding,
    stream_sum,
    stream_take,
    stream_take_while,
    stream_zip,
    stream_zip_with_index,
    trending,
)


class TestStreamBasics:
    """ストリーム基本操作のテスト"""

    def test_stream_of(self) -> None:
        assert list(stream_of(1, 2, 3)) == [1, 2, 3]

    def test_stream_from_list(self) -> None:
        assert list(stream_from_list([1, 2, 3])) == [1, 2, 3]

    def test_stream_range(self) -> None:
        assert list(stream_range(1, 5)) == [1, 2, 3, 4]


class TestInfiniteStreams:
    """無限ストリームのテスト"""

    def test_infinite_stream(self) -> None:
        result = list(stream_take(infinite_stream(5), 3))
        assert result == [5, 6, 7]

    def test_repeat_stream(self) -> None:
        result = list(stream_take(repeat_stream(42), 3))
        assert result == [42, 42, 42]

    def test_cycle_stream(self) -> None:
        result = list(stream_take(cycle_stream([1, 2, 3]), 8))
        assert result == [1, 2, 3, 1, 2, 3, 1, 2]

    def test_alternating_stream(self) -> None:
        result = list(stream_take(alternating_stream(), 5))
        assert result == [True, False, True, False, True]


class TestStreamOperations:
    """ストリーム操作のテスト"""

    def test_stream_take(self) -> None:
        result = list(stream_take(infinite_stream(), 5))
        assert result == [0, 1, 2, 3, 4]

    def test_stream_filter(self) -> None:
        result = list(stream_take(stream_filter(infinite_stream(), lambda x: x % 2 == 0), 5))
        assert result == [0, 2, 4, 6, 8]

    def test_stream_map(self) -> None:
        result = list(stream_take(stream_map(infinite_stream(), lambda x: x * 2), 5))
        assert result == [0, 2, 4, 6, 8]

    def test_stream_take_while(self) -> None:
        result = list(stream_take_while(infinite_stream(), lambda x: x < 5))
        assert result == [0, 1, 2, 3, 4]

    def test_stream_drop(self) -> None:
        result = list(stream_take(stream_drop(infinite_stream(), 5), 3))
        assert result == [5, 6, 7]

    def test_stream_drop_while(self) -> None:
        result = list(stream_take(stream_drop_while(infinite_stream(), lambda x: x < 5), 3))
        assert result == [5, 6, 7]


class TestStreamCombination:
    """ストリーム結合のテスト"""

    def test_stream_append(self) -> None:
        result = list(stream_append(stream_of(1, 2), stream_of(3, 4)))
        assert result == [1, 2, 3, 4]

    def test_stream_zip(self) -> None:
        result = list(stream_zip(stream_of(1, 2, 3), stream_of("a", "b", "c")))
        assert result == [(1, "a"), (2, "b"), (3, "c")]

    def test_stream_zip_with_index(self) -> None:
        result = list(stream_zip_with_index(stream_of("a", "b", "c")))
        assert result == [(0, "a"), (1, "b"), (2, "c")]

    def test_stream_interleave(self) -> None:
        result = list(stream_take(stream_interleave(stream_of(1, 2, 3), stream_of(10, 20, 30)), 6))
        assert result == [1, 10, 2, 20, 3, 30]


class TestSlidingWindow:
    """スライディングウィンドウのテスト"""

    def test_stream_sliding(self) -> None:
        result = list(stream_sliding(stream_of(1, 2, 3, 4, 5), 3))
        assert result == [[1, 2, 3], [2, 3, 4], [3, 4, 5]]

    def test_stream_sliding_window_larger_than_stream(self) -> None:
        result = list(stream_sliding(stream_of(1, 2), 5))
        assert result == []

    def test_stream_chunked(self) -> None:
        result = list(stream_chunked(stream_of(1, 2, 3, 4, 5, 6, 7), 3))
        assert result == [[1, 2, 3], [4, 5, 6], [7]]


class TestCastDieStream:
    """サイコロストリームのテスト"""

    def test_cast_die_stream_valid_values(self) -> None:
        results = list(stream_take(cast_die_stream(), 100))
        assert all(1 <= r <= 6 for r in results)

    def test_first_six_ends_with_six(self) -> None:
        results = list(first_six())
        assert results[-1] == 6

    def test_die_sum_until(self) -> None:
        results = list(die_sum_until(20))
        assert results[-1][1] >= 20


class TestTrending:
    """トレンド判定のテスト"""

    def test_trending_true(self) -> None:
        assert trending([Decimal("0.81"), Decimal("0.82"), Decimal("0.83")]) is True

    def test_trending_false(self) -> None:
        assert trending([Decimal("0.81"), Decimal("0.84"), Decimal("0.83")]) is False

    def test_trending_insufficient_data(self) -> None:
        assert trending([Decimal("0.81")]) is False

    def test_is_stable_true(self) -> None:
        assert is_stable([Decimal("0.81"), Decimal("0.81"), Decimal("0.81")]) is True

    def test_is_stable_false(self) -> None:
        assert is_stable([Decimal("0.81"), Decimal("0.82"), Decimal("0.81")]) is False


class TestExchangeRateStream:
    """為替レートストリームのテスト"""

    def test_exchange_rate_stream_returns_decimals(self) -> None:
        rates = list(stream_take(exchange_rate_stream(Currency.USD, Currency.EUR), 3))
        assert all(isinstance(r, Decimal) for r in rates)

    def test_exchange_rate_stream_positive_rates(self) -> None:
        rates = list(stream_take(exchange_rate_stream(Currency.USD, Currency.EUR), 10))
        assert all(r > 0 for r in rates)


class TestStreamAggregation:
    """ストリーム集約のテスト"""

    def test_stream_sum(self) -> None:
        result = stream_sum(stream_of(1, 2, 3, 4, 5))
        assert result == 15

    def test_stream_reduce(self) -> None:
        result = stream_reduce(stream_of(1, 2, 3, 4), lambda acc, x: acc + x, 0)
        assert result == 10

    def test_stream_count(self) -> None:
        result = stream_count(stream_of(1, 2, 3, 4, 5))
        assert result == 5

    def test_stream_find_found(self) -> None:
        result = stream_find(stream_of(1, 2, 3, 4, 5), lambda x: x > 3)
        assert result == 4

    def test_stream_find_not_found(self) -> None:
        result = stream_find(stream_of(1, 2, 3), lambda x: x > 10)
        assert result is None

    def test_stream_exists_true(self) -> None:
        result = stream_exists(stream_of(1, 2, 3, 4, 5), lambda x: x > 3)
        assert result is True

    def test_stream_exists_false(self) -> None:
        result = stream_exists(stream_of(1, 2, 3), lambda x: x > 10)
        assert result is False

    def test_stream_forall_true(self) -> None:
        result = stream_forall(stream_of(2, 4, 6), lambda x: x % 2 == 0)
        assert result is True

    def test_stream_forall_false(self) -> None:
        result = stream_forall(stream_of(2, 3, 6), lambda x: x % 2 == 0)
        assert result is False
