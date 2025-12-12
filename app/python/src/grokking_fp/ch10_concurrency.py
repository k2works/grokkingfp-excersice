"""第10章: 並行・並列処理

Python で並行処理を扱う関数型プログラミングのパターンを学びます。
asyncio を使った非同期処理、Ref（アトミック参照）の実装を学びます。
"""

import asyncio
import random
import threading
from collections.abc import Callable, Coroutine
from dataclasses import dataclass
from typing import Any, Generic, TypeVar

T = TypeVar("T")
U = TypeVar("U")


# =============================================================================
# 10.1 Ref - アトミックな共有状態
# =============================================================================


class Ref(Generic[T]):
    """スレッドセーフなアトミック参照。

    複数の並行処理から安全にアクセスできる共有状態を提供します。

    Examples:
        >>> ref = Ref(0)
        >>> ref.get()
        0
        >>> ref.update(lambda x: x + 1)
        >>> ref.get()
        1
    """

    def __init__(self, initial: T) -> None:
        self._value = initial
        self._lock = threading.Lock()

    def get(self) -> T:
        """現在の値を取得する。"""
        with self._lock:
            return self._value

    def set(self, value: T) -> None:
        """値を設定する。"""
        with self._lock:
            self._value = value

    def update(self, f: Callable[[T], T]) -> None:
        """アトミックに値を更新する。"""
        with self._lock:
            self._value = f(self._value)

    def modify(self, f: Callable[[T], tuple[T, U]]) -> U:
        """アトミックに値を更新し、結果を返す。"""
        with self._lock:
            new_value, result = f(self._value)
            self._value = new_value
            return result

    def get_and_set(self, value: T) -> T:
        """値を設定し、古い値を返す。"""
        with self._lock:
            old_value = self._value
            self._value = value
            return old_value


class AsyncRef(Generic[T]):
    """非同期コンテキスト用のアトミック参照。

    asyncio との組み合わせで使用します。

    Examples:
        >>> import asyncio
        >>> async def example():
        ...     ref = AsyncRef(0)
        ...     await ref.update(lambda x: x + 1)
        ...     return await ref.get()
        >>> asyncio.run(example())
        1
    """

    def __init__(self, initial: T) -> None:
        self._value = initial
        self._lock = asyncio.Lock()

    async def get(self) -> T:
        """現在の値を取得する。"""
        async with self._lock:
            return self._value

    async def set(self, value: T) -> None:
        """値を設定する。"""
        async with self._lock:
            self._value = value

    async def update(self, f: Callable[[T], T]) -> None:
        """アトミックに値を更新する。"""
        async with self._lock:
            self._value = f(self._value)

    async def modify(self, f: Callable[[T], tuple[T, U]]) -> U:
        """アトミックに値を更新し、結果を返す。"""
        async with self._lock:
            new_value, result = f(self._value)
            self._value = new_value
            return result


# =============================================================================
# 10.2 チェックインのリアルタイム集計
# =============================================================================


@dataclass(frozen=True)
class City:
    """都市を表すデータクラス。"""

    name: str


@dataclass(frozen=True)
class CityStats:
    """都市のチェックイン統計。"""

    city: City
    check_ins: int


def top_cities(city_check_ins: dict[City, int], n: int = 3) -> list[CityStats]:
    """チェックイン数上位の都市を取得する（純粋関数）。

    Examples:
        >>> cities = {City("Tokyo"): 100, City("Osaka"): 50, City("Kyoto"): 75}
        >>> result = top_cities(cities, 2)
        >>> [s.city.name for s in result]
        ['Tokyo', 'Kyoto']
    """
    stats = [CityStats(city, count) for city, count in city_check_ins.items()]
    return sorted(stats, key=lambda s: s.check_ins, reverse=True)[:n]


def update_check_ins(
    city_check_ins: dict[City, int], city: City
) -> dict[City, int]:
    """チェックインを追加する（純粋関数）。

    Examples:
        >>> cities = {City("Tokyo"): 10}
        >>> result = update_check_ins(cities, City("Tokyo"))
        >>> result[City("Tokyo")]
        11
    """
    new_check_ins = dict(city_check_ins)
    new_check_ins[city] = new_check_ins.get(city, 0) + 1
    return new_check_ins


# =============================================================================
# 10.3 並行処理 - asyncio
# =============================================================================


async def async_sleep(seconds: float) -> None:
    """非同期スリープ（スレッドをブロックしない）。

    Examples:
        >>> import asyncio
        >>> asyncio.run(async_sleep(0.01))
    """
    await asyncio.sleep(seconds)


async def par_sequence(
    tasks: list[Coroutine[Any, Any, T]],
) -> list[T]:
    """複数の非同期タスクを並列実行する。

    Examples:
        >>> import asyncio
        >>> async def example():
        ...     async def task(n):
        ...         return n * 2
        ...     return await par_sequence([task(1), task(2), task(3)])
        >>> asyncio.run(example())
        [2, 4, 6]
    """
    return list(await asyncio.gather(*tasks))


async def par_traverse(
    items: list[T],
    f: Callable[[T], Coroutine[Any, Any, U]],
) -> list[U]:
    """リストの各要素に非同期関数を並列適用する。

    Examples:
        >>> import asyncio
        >>> async def example():
        ...     async def double(n):
        ...         return n * 2
        ...     return await par_traverse([1, 2, 3], double)
        >>> asyncio.run(example())
        [2, 4, 6]
    """
    return await par_sequence([f(item) for item in items])


# =============================================================================
# 10.4 サイコロを並行して振る
# =============================================================================


def cast_the_die_impure() -> int:
    """不純な関数: サイコロを振る。

    Examples:
        >>> result = cast_the_die_impure()
        >>> 1 <= result <= 6
        True
    """
    return random.randint(1, 6)


async def cast_the_die() -> int:
    """非同期でサイコロを振る。

    Examples:
        >>> import asyncio
        >>> result = asyncio.run(cast_the_die())
        >>> 1 <= result <= 6
        True
    """
    await asyncio.sleep(0)  # yield control
    return cast_the_die_impure()


async def cast_the_die_twice() -> int:
    """2つのサイコロを並行して振り、合計を返す。

    Examples:
        >>> import asyncio
        >>> result = asyncio.run(cast_the_die_twice())
        >>> 2 <= result <= 12
        True
    """
    results = await par_sequence([cast_the_die(), cast_the_die()])
    return sum(results)


async def cast_dice_parallel(n: int) -> list[int]:
    """n個のサイコロを並行して振る。

    Examples:
        >>> import asyncio
        >>> results = asyncio.run(cast_dice_parallel(3))
        >>> len(results)
        3
        >>> all(1 <= r <= 6 for r in results)
        True
    """
    return await par_sequence([cast_the_die() for _ in range(n)])


# =============================================================================
# 10.5 Ref を使った並行カウント
# =============================================================================


async def increment_three_times() -> int:
    """カウンターを3回インクリメントする。

    Examples:
        >>> import asyncio
        >>> asyncio.run(increment_three_times())
        3
    """
    counter = AsyncRef(0)
    await counter.update(lambda x: x + 1)
    await counter.update(lambda x: x + 1)
    await counter.update(lambda x: x + 1)
    return await counter.get()


async def sum_parallel(
    task1: Coroutine[Any, Any, int],
    task2: Coroutine[Any, Any, int],
    task3: Coroutine[Any, Any, int],
) -> int:
    """3つの非同期タスクを並列実行し、結果の合計を返す。

    Examples:
        >>> import asyncio
        >>> async def example():
        ...     async def pure(n):
        ...         return n
        ...     return await sum_parallel(pure(1), pure(2), pure(3))
        >>> asyncio.run(example())
        6
    """
    results = await par_sequence([task1, task2, task3])
    return sum(results)


async def count_evens(tasks: list[Coroutine[Any, Any, int]]) -> int:
    """複数の非同期タスクを並列実行し、偶数の結果をカウントする。

    Examples:
        >>> import asyncio
        >>> async def example():
        ...     async def pure(n):
        ...         return n
        ...     tasks = [pure(1), pure(2), pure(3), pure(4)]
        ...     return await count_evens(tasks)
        >>> asyncio.run(example())
        2
    """
    counter = AsyncRef(0)

    async def count_if_even(task: Coroutine[Any, Any, int]) -> None:
        result = await task
        if result % 2 == 0:
            await counter.update(lambda x: x + 1)

    await par_sequence([count_if_even(t) for t in tasks])
    return await counter.get()


# =============================================================================
# 10.6 チェックイン処理の並行版
# =============================================================================


async def store_check_in(
    stored_check_ins: AsyncRef[dict[City, int]], city: City
) -> None:
    """チェックインを保存する。

    Examples:
        >>> import asyncio
        >>> async def example():
        ...     ref = AsyncRef({})
        ...     await store_check_in(ref, City("Tokyo"))
        ...     return await ref.get()
        >>> asyncio.run(example())
        {City(name='Tokyo'): 1}
    """
    await stored_check_ins.update(lambda m: update_check_ins(m, city))


async def update_ranking(
    stored_check_ins: AsyncRef[dict[City, int]],
    stored_ranking: AsyncRef[list[CityStats]],
) -> None:
    """ランキングを更新する（1回）。

    Examples:
        >>> import asyncio
        >>> async def example():
        ...     check_ins = AsyncRef({City("Tokyo"): 10, City("Osaka"): 5})
        ...     ranking = AsyncRef([])
        ...     await update_ranking(check_ins, ranking)
        ...     result = await ranking.get()
        ...     return [s.city.name for s in result]
        >>> asyncio.run(example())
        ['Tokyo', 'Osaka']
    """
    check_ins = await stored_check_ins.get()
    await stored_ranking.set(top_cities(check_ins))


# =============================================================================
# 10.7 Fiber - 軽量タスク
# =============================================================================


@dataclass
class Fiber(Generic[T]):
    """軽量タスク（asyncio.Task のラッパー）。

    キャンセル可能なバックグラウンドタスクを表現します。
    """

    _task: asyncio.Task[T]

    def cancel(self) -> None:
        """タスクをキャンセルする。"""
        self._task.cancel()

    async def join(self) -> T:
        """タスクの完了を待つ。"""
        return await self._task

    @property
    def is_done(self) -> bool:
        """タスクが完了しているか。"""
        return self._task.done()


def start_fiber(coro: Coroutine[Any, Any, T]) -> Fiber[T]:
    """コルーチンを Fiber として起動する。

    Examples:
        >>> import asyncio
        >>> async def example():
        ...     async def task():
        ...         await asyncio.sleep(0.01)
        ...         return 42
        ...     fiber = start_fiber(task())
        ...     return await fiber.join()
        >>> asyncio.run(example())
        42
    """
    task = asyncio.create_task(coro)
    return Fiber(task)


async def forever(action: Callable[[], Coroutine[Any, Any, T]]) -> None:
    """アクションを永遠に繰り返す。

    Note:
        この関数は永遠に実行されるため、外部からキャンセルする必要があります。
    """
    while True:
        await action()


# =============================================================================
# 10.8 タイムアウト付き収集
# =============================================================================


async def collect_for(
    duration_seconds: float, interval_seconds: float = 0.1
) -> list[int]:
    """指定時間、定期的に乱数を収集する。

    Examples:
        >>> import asyncio
        >>> results = asyncio.run(collect_for(0.25, 0.1))
        >>> 1 <= len(results) <= 5  # 約2-3個
        True
    """
    collected: AsyncRef[list[int]] = AsyncRef([])

    async def producer() -> None:
        while True:
            await asyncio.sleep(interval_seconds)
            value = random.randint(0, 99)
            await collected.update(lambda lst: lst + [value])

    fiber = start_fiber(producer())

    await asyncio.sleep(duration_seconds)
    fiber.cancel()

    try:
        await fiber.join()
    except asyncio.CancelledError:
        pass

    return await collected.get()


# =============================================================================
# 10.9 並行マップ更新
# =============================================================================


@dataclass(frozen=True)
class Update:
    """マップ更新を表すデータクラス。"""

    key: str
    value: int


async def apply_updates(updates: list[Update]) -> dict[str, int]:
    """複数の更新を並行してマップに適用する。

    Examples:
        >>> import asyncio
        >>> updates = [Update("a", 1), Update("b", 2), Update("a", 3)]
        >>> result = asyncio.run(apply_updates(updates))
        >>> result["a"] in [1, 3]  # 並行実行のため順序は不定
        True
        >>> result["b"]
        2
    """
    map_ref: AsyncRef[dict[str, int]] = AsyncRef({})

    async def apply_update(update: Update) -> None:
        await map_ref.update(
            lambda m: {**m, update.key: update.value}
        )

    await par_sequence([apply_update(u) for u in updates])
    return await map_ref.get()


# =============================================================================
# 10.10 ProcessingCheckIns - バックグラウンド処理
# =============================================================================


@dataclass
class ProcessingCheckIns:
    """チェックイン処理の制御ハンドル。"""

    current_ranking: Callable[[], Coroutine[Any, Any, list[CityStats]]]
    stop: Callable[[], None]


async def process_check_ins_background(
    check_ins: list[City],
) -> ProcessingCheckIns:
    """チェックイン処理をバックグラウンドで開始し、制御ハンドルを返す。

    Examples:
        >>> import asyncio
        >>> async def example():
        ...     cities = [City("Tokyo"), City("Osaka"), City("Tokyo")]
        ...     processing = await process_check_ins_background(cities)
        ...     await asyncio.sleep(0.05)
        ...     ranking = await processing.current_ranking()
        ...     processing.stop()
        ...     return [s.city.name for s in ranking]
        >>> asyncio.run(example())
        ['Tokyo', 'Osaka']
    """
    stored_check_ins: AsyncRef[dict[City, int]] = AsyncRef({})
    stored_ranking: AsyncRef[list[CityStats]] = AsyncRef([])

    async def check_in_processor() -> None:
        for city in check_ins:
            await store_check_in(stored_check_ins, city)
            await asyncio.sleep(0)  # yield control

    async def ranking_updater() -> None:
        while True:
            await update_ranking(stored_check_ins, stored_ranking)
            await asyncio.sleep(0.01)

    check_in_fiber = start_fiber(check_in_processor())
    ranking_fiber = start_fiber(ranking_updater())

    def stop() -> None:
        check_in_fiber.cancel()
        ranking_fiber.cancel()

    async def get_ranking() -> list[CityStats]:
        return await stored_ranking.get()

    return ProcessingCheckIns(current_ranking=get_ranking, stop=stop)


# =============================================================================
# 10.11 並列 vs 逐次の比較
# =============================================================================


async def sequential_sleep(count: int, seconds: float) -> float:
    """逐次的にスリープする（合計時間を返す）。

    Examples:
        >>> import asyncio
        >>> import time
        >>> start = time.time()
        >>> asyncio.run(sequential_sleep(3, 0.1))
        >>> elapsed = time.time() - start
        >>> 0.3 <= elapsed < 0.5
        True
    """
    for _ in range(count):
        await asyncio.sleep(seconds)
    return count * seconds


async def parallel_sleep(count: int, seconds: float) -> float:
    """並列にスリープする（合計時間を返す）。

    Examples:
        >>> import asyncio
        >>> import time
        >>> start = time.time()
        >>> asyncio.run(parallel_sleep(3, 0.1))
        >>> elapsed = time.time() - start
        >>> elapsed < 0.2  # 並列なので約0.1秒
        True
    """
    await par_sequence([asyncio.sleep(seconds) for _ in range(count)])
    return seconds  # 並列なので最長のタスク時間のみ


# =============================================================================
# スレッドベースの並行処理
# =============================================================================


def run_in_threads(tasks: list[Callable[[], T]]) -> list[T]:
    """複数のタスクをスレッドで並列実行する。

    Examples:
        >>> def task1():
        ...     return 1
        >>> def task2():
        ...     return 2
        >>> sorted(run_in_threads([task1, task2]))
        [1, 2]
    """
    results: list[T] = []
    lock = threading.Lock()

    def run_task(task: Callable[[], T]) -> None:
        result = task()
        with lock:
            results.append(result)

    threads = [threading.Thread(target=run_task, args=(task,)) for task in tasks]
    for t in threads:
        t.start()
    for t in threads:
        t.join()

    return results


def parallel_map(items: list[T], f: Callable[[T], U]) -> list[U]:
    """リストの各要素に関数を並列適用する（スレッド使用）。

    Examples:
        >>> parallel_map([1, 2, 3], lambda x: x * 2)
        [2, 4, 6]
    """
    results: list[tuple[int, U]] = []
    lock = threading.Lock()

    def process(index: int, item: T) -> None:
        result = f(item)
        with lock:
            results.append((index, result))

    threads = [
        threading.Thread(target=process, args=(i, item))
        for i, item in enumerate(items)
    ]
    for t in threads:
        t.start()
    for t in threads:
        t.join()

    # インデックスでソートして結果を返す
    return [r for _, r in sorted(results)]
