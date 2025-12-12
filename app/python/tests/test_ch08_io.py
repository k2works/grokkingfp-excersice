"""第8章: IO モナドのテスト"""

from grokking_fp.ch08_io import (
    LazyIO,
    MeetingTime,
    calendar_entries,
    cast_the_die,
    cast_the_die_impure,
    cast_the_die_twice,
    combine_io,
    io_bind,
    io_delay,
    io_foreach,
    io_map,
    io_or_else,
    io_pure,
    io_unless,
    io_when,
    meetings_overlap,
    possible_meetings,
    print_and_return,
    retry,
    schedule_meeting,
    scheduled_meetings,
    scheduled_meetings_all,
    sequence_io,
    traverse_io,
)


class TestCastTheDie:
    """サイコロを振る関数のテスト"""

    def test_impure_returns_valid_value(self) -> None:
        result = cast_the_die_impure()
        assert 1 <= result <= 6

    def test_io_returns_io_type(self) -> None:
        io = cast_the_die()
        assert isinstance(io, LazyIO)

    def test_io_value_is_valid(self) -> None:
        io = cast_the_die()
        result = io.run()
        assert 1 <= result <= 6

    def test_cast_twice_returns_valid_sum(self) -> None:
        io = cast_the_die_twice()
        result = io.run()
        assert 2 <= result <= 12


class TestIOBasics:
    """IO 基本操作のテスト"""

    def test_io_pure(self) -> None:
        io = io_pure(42)
        assert io.run() == 42

    def test_io_delay_defers_execution(self) -> None:
        counter = [0]

        def increment() -> int:
            counter[0] += 1
            return counter[0]

        io = io_delay(increment)
        assert counter[0] == 0  # まだ実行されていない

        result = io.run()
        assert counter[0] == 1  # 実行された
        assert result == 1

    def test_io_map(self) -> None:
        io = io_pure(5)
        doubled = io_map(io, lambda x: x * 2)
        assert doubled.run() == 10

    def test_io_bind(self) -> None:
        io = io_pure(5)
        result = io_bind(io, lambda x: io_pure(x * 2))
        assert result.run() == 10


class TestMeetingTime:
    """MeetingTime 関連のテスト"""

    def test_meetings_overlap_no_overlap(self) -> None:
        m1 = MeetingTime(9, 10)
        m2 = MeetingTime(10, 11)
        assert meetings_overlap(m1, m2) is False

    def test_meetings_overlap_has_overlap(self) -> None:
        m1 = MeetingTime(9, 11)
        m2 = MeetingTime(10, 12)
        assert meetings_overlap(m1, m2) is True

    def test_meetings_overlap_complete_overlap(self) -> None:
        m1 = MeetingTime(9, 12)
        m2 = MeetingTime(10, 11)
        assert meetings_overlap(m1, m2) is True


class TestPossibleMeetings:
    """空き時間計算のテスト"""

    def test_no_existing_meetings(self) -> None:
        slots = possible_meetings([], 9, 17, 1)
        assert len(slots) == 8  # 9-10, 10-11, ..., 16-17

    def test_with_existing_meetings(self) -> None:
        existing = [MeetingTime(10, 11), MeetingTime(14, 15)]
        slots = possible_meetings(existing, 9, 17, 1)
        assert MeetingTime(10, 11) not in slots
        assert MeetingTime(14, 15) not in slots
        assert MeetingTime(9, 10) in slots

    def test_longer_meeting(self) -> None:
        existing = [MeetingTime(10, 11)]
        slots = possible_meetings(existing, 9, 17, 2)
        # 9-11 は 10-11 と重複するので不可
        assert MeetingTime(9, 11) not in slots
        assert MeetingTime(11, 13) in slots


class TestCalendarEntries:
    """カレンダーAPI のテスト"""

    def test_calendar_entries_returns_io(self) -> None:
        io = calendar_entries("Alice")
        assert isinstance(io, LazyIO)

    def test_calendar_entries_alice(self) -> None:
        io = calendar_entries("Alice")
        entries = io.run()
        assert len(entries) == 2

    def test_calendar_entries_unknown(self) -> None:
        io = calendar_entries("Unknown")
        entries = io.run()
        assert len(entries) == 0


class TestScheduledMeetings:
    """予定取得のテスト"""

    def test_scheduled_meetings_two_people(self) -> None:
        io = scheduled_meetings("Alice", "Bob")
        meetings = io.run()
        assert len(meetings) == 3  # Alice: 2, Bob: 1

    def test_scheduled_meetings_all(self) -> None:
        io = scheduled_meetings_all(["Alice", "Bob", "Charlie"])
        meetings = io.run()
        assert len(meetings) == 5  # Alice: 2, Bob: 1, Charlie: 2


class TestScheduleMeeting:
    """ミーティングスケジュールのテスト"""

    def test_schedule_meeting(self) -> None:
        io = schedule_meeting("Alice", "Bob", 8, 16, 1)
        slots = io.run()
        assert isinstance(slots, list)
        # Alice: 9-10, 14-15, Bob: 10-12 が埋まっている
        assert MeetingTime(9, 10) not in slots
        assert MeetingTime(10, 11) not in slots
        assert MeetingTime(11, 12) not in slots
        assert MeetingTime(14, 15) not in slots


class TestIOOrElse:
    """IO のエラーハンドリングテスト"""

    def test_or_else_success(self) -> None:
        success = io_pure(42)
        fallback = io_pure(0)
        result = io_or_else(success, fallback)
        assert result.run() == 42

    def test_or_else_failure(self) -> None:
        def fail() -> int:
            raise ValueError("error")

        failure = LazyIO(fail)
        fallback = io_pure(0)
        result = io_or_else(failure, fallback)
        assert result.run() == 0


class TestRetry:
    """リトライのテスト"""

    def test_retry_success(self) -> None:
        io = retry(io_pure(42), 3, 0)
        assert io.run() == 42

    def test_retry_with_failures(self) -> None:
        counter = [0]

        def maybe_fail() -> int:
            counter[0] += 1
            if counter[0] < 3:
                raise ValueError("not yet")
            return 42

        io = retry(LazyIO(maybe_fail), 5, 0)
        assert io.run() == 42
        assert counter[0] >= 3

    def test_retry_all_failures(self) -> None:
        def always_fail() -> int:
            raise ValueError("always")

        io = retry(LazyIO(always_fail), 3, 0)
        assert io.run() == 0


class TestSequenceIO:
    """sequence のテスト"""

    def test_sequence_io(self) -> None:
        ios = [io_pure(1), io_pure(2), io_pure(3)]
        result = sequence_io(ios)
        assert result.run() == [1, 2, 3]

    def test_traverse_io(self) -> None:
        items = [1, 2, 3]
        result = traverse_io(items, lambda x: io_pure(x * 2))
        assert result.run() == [2, 4, 6]


class TestIOUtilities:
    """IO ユーティリティのテスト"""

    def test_io_foreach(self) -> None:
        results: list[int] = []
        io = io_foreach(io_pure(42), lambda x: results.append(x))
        io.run()
        assert results == [42]

    def test_io_when_true(self) -> None:
        counter = [0]
        io = io_when(True, io_delay(lambda: counter.__setitem__(0, 1)))
        io.run()
        assert counter[0] == 1

    def test_io_when_false(self) -> None:
        counter = [0]
        io = io_when(False, io_delay(lambda: counter.__setitem__(0, 1)))
        io.run()
        assert counter[0] == 0

    def test_io_unless_false(self) -> None:
        counter = [0]
        io = io_unless(False, io_delay(lambda: counter.__setitem__(0, 1)))
        io.run()
        assert counter[0] == 1

    def test_io_unless_true(self) -> None:
        counter = [0]
        io = io_unless(True, io_delay(lambda: counter.__setitem__(0, 1)))
        io.run()
        assert counter[0] == 0

    def test_print_and_return(self) -> None:
        io = print_and_return("test")
        assert isinstance(io, LazyIO)

    def test_combine_io(self) -> None:
        io = combine_io(io_pure(1), io_pure(2), lambda a, b: a + b)
        assert io.run() == 3
