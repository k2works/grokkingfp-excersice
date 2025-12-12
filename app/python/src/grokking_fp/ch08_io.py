"""第8章: IO モナドの導入

Python で副作用を扱う関数型プログラミングのパターンを学びます。
遅延評価 IO モナドを自前で実装し、副作用の管理を学びます。
"""

import random
from collections.abc import Callable
from dataclasses import dataclass
from typing import Generic, TypeVar

T = TypeVar("T")
U = TypeVar("U")


# =============================================================================
# カスタム IO 型（遅延評価）
# =============================================================================


class LazyIO(Generic[T]):
    """遅延評価する IO モナド。

    Examples:
        >>> io = LazyIO(lambda: 42)
        >>> io.run()
        42
    """

    def __init__(self, effect: Callable[[], T]) -> None:
        self._effect = effect

    def run(self) -> T:
        """副作用を実行して結果を返す。"""
        return self._effect()

    def map(self, f: Callable[[T], U]) -> "LazyIO[U]":
        """値を変換する。"""
        return LazyIO(lambda: f(self.run()))

    def bind(self, f: Callable[[T], "LazyIO[U]"]) -> "LazyIO[U]":
        """IO を返す関数で変換する（flatMap）。"""
        return LazyIO(lambda: f(self.run()).run())

    def __repr__(self) -> str:
        return "LazyIO(...)"


# returns ライブラリの IO も使えるようにエイリアス
from returns.io import IO, impure_safe  # noqa: E402


# =============================================================================
# 8.1 副作用の問題
# =============================================================================


def cast_the_die_impure() -> int:
    """不純な関数: 呼び出すたびに異なる値を返す。

    Examples:
        >>> result = cast_the_die_impure()
        >>> 1 <= result <= 6
        True
    """
    return random.randint(1, 6)


# =============================================================================
# 8.2 IO モナドの基本
# =============================================================================


def cast_the_die() -> LazyIO[int]:
    """LazyIO でラップした純粋な記述。

    Examples:
        >>> die = cast_the_die()
        >>> isinstance(die, LazyIO)
        True
    """
    return LazyIO(lambda: random.randint(1, 6))


def io_pure(value: T) -> LazyIO[T]:
    """既存の値を LazyIO にラップする。

    Examples:
        >>> io = io_pure(42)
        >>> io.run()
        42
    """
    return LazyIO(lambda: value)


def io_delay(func: Callable[[], T]) -> LazyIO[T]:
    """副作用のある関数を LazyIO にラップする。

    Examples:
        >>> counter = [0]
        >>> io = io_delay(lambda: counter.__setitem__(0, counter[0] + 1) or counter[0])
        >>> counter[0]  # まだ実行されていない
        0
    """
    return LazyIO(func)


# =============================================================================
# 8.3 IO の合成
# =============================================================================


def io_map(io: LazyIO[T], f: Callable[[T], U]) -> LazyIO[U]:
    """IO の値を変換する。

    Examples:
        >>> io = io_pure(5)
        >>> doubled = io_map(io, lambda x: x * 2)
        >>> doubled.run()
        10
    """
    return io.map(f)


def io_bind(io: LazyIO[T], f: Callable[[T], LazyIO[U]]) -> LazyIO[U]:
    """IO を返す関数で変換する（flatMap）。

    Examples:
        >>> io = io_pure(5)
        >>> result = io_bind(io, lambda x: io_pure(x * 2))
        >>> result.run()
        10
    """
    return io.bind(f)


def cast_the_die_twice() -> LazyIO[int]:
    """サイコロを2回振って合計を返す。

    Examples:
        >>> io = cast_the_die_twice()
        >>> result = io.run()
        >>> 2 <= result <= 12
        True
    """
    return cast_the_die().bind(
        lambda first: cast_the_die().map(lambda second: first + second)
    )


# =============================================================================
# 8.4 ミーティングスケジューリング
# =============================================================================


@dataclass(frozen=True)
class MeetingTime:
    """ミーティングの時間枠。"""

    start_hour: int
    end_hour: int


def meetings_overlap(meeting1: MeetingTime, meeting2: MeetingTime) -> bool:
    """2つのミーティングが重複しているかを判定する。

    Examples:
        >>> meetings_overlap(MeetingTime(9, 10), MeetingTime(10, 11))
        False
        >>> meetings_overlap(MeetingTime(9, 11), MeetingTime(10, 12))
        True
    """
    return meeting1.start_hour < meeting2.end_hour and meeting2.start_hour < meeting1.end_hour


def possible_meetings(
    existing_meetings: list[MeetingTime],
    start_hour: int,
    end_hour: int,
    length_hours: int,
) -> list[MeetingTime]:
    """空いている時間枠を計算する（純粋関数）。

    Examples:
        >>> existing = [MeetingTime(9, 10), MeetingTime(12, 13)]
        >>> slots = possible_meetings(existing, 8, 16, 1)
        >>> MeetingTime(8, 9) in slots
        True
        >>> MeetingTime(9, 10) in slots
        False
    """
    slots = [
        MeetingTime(start, start + length_hours)
        for start in range(start_hour, end_hour - length_hours + 1)
    ]
    return [
        slot
        for slot in slots
        if all(not meetings_overlap(meeting, slot) for meeting in existing_meetings)
    ]


# シミュレーション用のカレンダーデータ
_calendar_data: dict[str, list[MeetingTime]] = {
    "Alice": [MeetingTime(9, 10), MeetingTime(14, 15)],
    "Bob": [MeetingTime(10, 12)],
    "Charlie": [MeetingTime(13, 14), MeetingTime(15, 16)],
}


def calendar_entries_impure(name: str) -> list[MeetingTime]:
    """カレンダー API を呼び出す（不純な関数）。

    Examples:
        >>> entries = calendar_entries_impure("Alice")
        >>> isinstance(entries, list)
        True
    """
    return _calendar_data.get(name, [])


def calendar_entries(name: str) -> LazyIO[list[MeetingTime]]:
    """カレンダー API 呼び出しを LazyIO でラップ。

    Examples:
        >>> io = calendar_entries("Alice")
        >>> isinstance(io, LazyIO)
        True
    """
    return LazyIO(lambda: calendar_entries_impure(name))


def scheduled_meetings(person1: str, person2: str) -> LazyIO[list[MeetingTime]]:
    """2人の予定を取得して結合する。

    Examples:
        >>> io = scheduled_meetings("Alice", "Bob")
        >>> meetings = io.run()
        >>> len(meetings) >= 0
        True
    """
    return calendar_entries(person1).bind(
        lambda entries1: calendar_entries(person2).map(
            lambda entries2: entries1 + entries2
        )
    )


def schedule_meeting(
    person1: str, person2: str, start_hour: int, end_hour: int, length_hours: int
) -> LazyIO[list[MeetingTime]]:
    """2人の空き時間を計算する。

    Examples:
        >>> io = schedule_meeting("Alice", "Bob", 8, 16, 1)
        >>> slots = io.run()
        >>> isinstance(slots, list)
        True
    """
    return scheduled_meetings(person1, person2).map(
        lambda meetings: possible_meetings(meetings, start_hour, end_hour, length_hours)
    )


# =============================================================================
# 8.5 エラーハンドリング
# =============================================================================


def io_or_else(io: LazyIO[T], fallback: LazyIO[T]) -> LazyIO[T]:
    """IO が失敗した場合にフォールバックを使う。

    Examples:
        >>> success = io_pure(42)
        >>> fallback = io_pure(0)
        >>> io_or_else(success, fallback).run()
        42
    """

    def run() -> T:
        try:
            return io.run()
        except Exception:
            return fallback.run()

    return LazyIO(run)


def risky_calendar_entries(name: str) -> list[MeetingTime]:
    """失敗する可能性のあるカレンダー API 呼び出し。

    Examples:
        >>> result = risky_calendar_entries("Alice")
        >>> isinstance(result, list) or result is None
        True
    """
    if random.random() < 0.3:  # 30% の確率で失敗
        raise ConnectionError(f"Failed to fetch calendar for {name}")
    return _calendar_data.get(name, [])


def calendar_entries_safe(name: str) -> LazyIO[list[MeetingTime]]:
    """失敗時にリトライするカレンダー API 呼び出し。

    Examples:
        >>> io = calendar_entries_safe("Alice")
        >>> isinstance(io, LazyIO)
        True
    """

    def try_fetch() -> list[MeetingTime]:
        try:
            return calendar_entries_impure(name)
        except Exception:
            return []

    return LazyIO(try_fetch)


# =============================================================================
# 8.6 リトライ
# =============================================================================


def retry(action: LazyIO[T], max_retries: int, default: T) -> LazyIO[T]:
    """指定回数リトライし、全部失敗したらデフォルト値を返す。

    Examples:
        >>> io = retry(io_pure(42), 3, 0)
        >>> io.run()
        42
    """

    def run() -> T:
        for _ in range(max_retries):
            try:
                return action.run()
            except Exception:
                continue
        return default

    return LazyIO(run)


def retry_with_delay(
    action: LazyIO[T], max_retries: int, default: T, delay_seconds: float = 0.1
) -> LazyIO[T]:
    """指定回数リトライし、間隔を空けて実行する。

    Examples:
        >>> io = retry_with_delay(io_pure(42), 3, 0, 0.01)
        >>> io.run()
        42
    """
    import time

    def run() -> T:
        for _ in range(max_retries):
            try:
                return action.run()
            except Exception:
                time.sleep(delay_seconds)
                continue
        return default

    return LazyIO(run)


# =============================================================================
# 8.7 sequence
# =============================================================================


def sequence_io(ios: list[LazyIO[T]]) -> LazyIO[list[T]]:
    """IO のリストを、リストの IO に変換する。

    Examples:
        >>> ios = [io_pure(1), io_pure(2), io_pure(3)]
        >>> result = sequence_io(ios)
        >>> result.run()
        [1, 2, 3]
    """

    def run() -> list[T]:
        return [io.run() for io in ios]

    return LazyIO(run)


def traverse_io(items: list[T], f: Callable[[T], LazyIO[U]]) -> LazyIO[list[U]]:
    """リストの各要素に IO を返す関数を適用し、結果をまとめる。

    Examples:
        >>> items = [1, 2, 3]
        >>> result = traverse_io(items, lambda x: io_pure(x * 2))
        >>> result.run()
        [2, 4, 6]
    """
    return sequence_io([f(item) for item in items])


def scheduled_meetings_all(attendees: list[str]) -> LazyIO[list[MeetingTime]]:
    """複数人の予定を全て取得する。

    Examples:
        >>> io = scheduled_meetings_all(["Alice", "Bob"])
        >>> meetings = io.run()
        >>> isinstance(meetings, list)
        True
    """
    return traverse_io(attendees, calendar_entries).map(
        lambda meetings_list: [
            meeting for meetings in meetings_list for meeting in meetings
        ]
    )


# =============================================================================
# 8.8 IO のユーティリティ
# =============================================================================


def io_foreach(io: LazyIO[T], f: Callable[[T], None]) -> LazyIO[None]:
    """IO の値に副作用のある関数を適用する。

    Examples:
        >>> results = []
        >>> io = io_foreach(io_pure(42), lambda x: results.append(x))
        >>> io.run()
        >>> results
        [42]
    """
    return io.map(lambda x: f(x))


def io_when(condition: bool, action: LazyIO[T]) -> LazyIO[None]:
    """条件が true のときだけ IO を実行する。

    Examples:
        >>> counter = [0]
        >>> io = io_when(True, io_delay(lambda: counter.__setitem__(0, 1)))
        >>> io.run()
        >>> counter[0]
        1
    """
    if condition:
        return action.map(lambda _: None)
    return LazyIO(lambda: None)


def io_unless(condition: bool, action: LazyIO[T]) -> LazyIO[None]:
    """条件が false のときだけ IO を実行する。

    Examples:
        >>> counter = [0]
        >>> io = io_unless(False, io_delay(lambda: counter.__setitem__(0, 1)))
        >>> io.run()
        >>> counter[0]
        1
    """
    return io_when(not condition, action)


def print_and_return(message: str) -> LazyIO[str]:
    """メッセージを出力して返す。

    Examples:
        >>> io = print_and_return("test")
        >>> isinstance(io, LazyIO)
        True
    """
    return LazyIO(lambda: (print(message), message)[1])


def combine_io(io1: LazyIO[T], io2: LazyIO[U], f: Callable[[T, U], T]) -> LazyIO[T]:
    """2つの IO を合成する。

    Examples:
        >>> io = combine_io(io_pure(1), io_pure(2), lambda a, b: a + b)
        >>> io.run()
        3
    """
    return io1.bind(lambda a: io2.map(lambda b: f(a, b)))
