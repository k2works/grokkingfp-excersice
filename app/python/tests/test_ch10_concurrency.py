"""第10章: 並行・並列処理のテスト"""

import asyncio
import time

import pytest

from grokking_fp.ch10_concurrency import (
    AsyncRef,
    City,
    CityStats,
    Fiber,
    ProcessingCheckIns,
    Ref,
    Update,
    apply_updates,
    async_sleep,
    cast_dice_parallel,
    cast_the_die,
    cast_the_die_impure,
    cast_the_die_twice,
    collect_for,
    count_evens,
    increment_three_times,
    par_sequence,
    par_traverse,
    parallel_map,
    parallel_sleep,
    process_check_ins_background,
    run_in_threads,
    sequential_sleep,
    start_fiber,
    store_check_in,
    sum_parallel,
    top_cities,
    update_check_ins,
    update_ranking,
)


class TestRef:
    """Ref（同期版）のテスト"""

    def test_ref_initial_value(self) -> None:
        ref = Ref(42)
        assert ref.get() == 42

    def test_ref_set(self) -> None:
        ref = Ref(0)
        ref.set(10)
        assert ref.get() == 10

    def test_ref_update(self) -> None:
        ref = Ref(5)
        ref.update(lambda x: x * 2)
        assert ref.get() == 10

    def test_ref_modify(self) -> None:
        ref = Ref(5)
        old_value = ref.modify(lambda x: (x + 1, x))
        assert old_value == 5
        assert ref.get() == 6

    def test_ref_get_and_set(self) -> None:
        ref = Ref(10)
        old_value = ref.get_and_set(20)
        assert old_value == 10
        assert ref.get() == 20


class TestAsyncRef:
    """AsyncRef のテスト"""

    @pytest.mark.asyncio
    async def test_async_ref_initial_value(self) -> None:
        ref = AsyncRef(42)
        assert await ref.get() == 42

    @pytest.mark.asyncio
    async def test_async_ref_set(self) -> None:
        ref = AsyncRef(0)
        await ref.set(10)
        assert await ref.get() == 10

    @pytest.mark.asyncio
    async def test_async_ref_update(self) -> None:
        ref = AsyncRef(5)
        await ref.update(lambda x: x * 2)
        assert await ref.get() == 10

    @pytest.mark.asyncio
    async def test_async_ref_modify(self) -> None:
        ref = AsyncRef(5)
        old_value = await ref.modify(lambda x: (x + 1, x))
        assert old_value == 5
        assert await ref.get() == 6


class TestCityStats:
    """City と CityStats のテスト"""

    def test_top_cities_basic(self) -> None:
        cities = {
            City("Tokyo"): 100,
            City("Osaka"): 50,
            City("Kyoto"): 75,
        }
        result = top_cities(cities, 2)
        assert len(result) == 2
        assert result[0].city.name == "Tokyo"
        assert result[1].city.name == "Kyoto"

    def test_top_cities_empty(self) -> None:
        result = top_cities({}, 3)
        assert result == []

    def test_update_check_ins_new_city(self) -> None:
        cities: dict[City, int] = {}
        result = update_check_ins(cities, City("Tokyo"))
        assert result[City("Tokyo")] == 1

    def test_update_check_ins_existing_city(self) -> None:
        cities = {City("Tokyo"): 10}
        result = update_check_ins(cities, City("Tokyo"))
        assert result[City("Tokyo")] == 11


class TestParallelExecution:
    """並列実行のテスト"""

    @pytest.mark.asyncio
    async def test_par_sequence(self) -> None:
        async def task(n: int) -> int:
            return n * 2

        result = await par_sequence([task(1), task(2), task(3)])
        assert result == [2, 4, 6]

    @pytest.mark.asyncio
    async def test_par_traverse(self) -> None:
        async def double(n: int) -> int:
            return n * 2

        result = await par_traverse([1, 2, 3], double)
        assert result == [2, 4, 6]

    @pytest.mark.asyncio
    async def test_async_sleep(self) -> None:
        start = time.time()
        await async_sleep(0.05)
        elapsed = time.time() - start
        assert elapsed >= 0.04


class TestCastTheDie:
    """サイコロを振る関数のテスト"""

    def test_cast_the_die_impure_valid_range(self) -> None:
        for _ in range(100):
            result = cast_the_die_impure()
            assert 1 <= result <= 6

    @pytest.mark.asyncio
    async def test_cast_the_die_valid_range(self) -> None:
        for _ in range(100):
            result = await cast_the_die()
            assert 1 <= result <= 6

    @pytest.mark.asyncio
    async def test_cast_the_die_twice_valid_range(self) -> None:
        for _ in range(100):
            result = await cast_the_die_twice()
            assert 2 <= result <= 12

    @pytest.mark.asyncio
    async def test_cast_dice_parallel(self) -> None:
        results = await cast_dice_parallel(5)
        assert len(results) == 5
        assert all(1 <= r <= 6 for r in results)


class TestRefCounters:
    """Ref を使ったカウンターのテスト"""

    @pytest.mark.asyncio
    async def test_increment_three_times(self) -> None:
        result = await increment_three_times()
        assert result == 3

    @pytest.mark.asyncio
    async def test_sum_parallel(self) -> None:
        async def pure(n: int) -> int:
            return n

        result = await sum_parallel(pure(1), pure(2), pure(3))
        assert result == 6

    @pytest.mark.asyncio
    async def test_count_evens(self) -> None:
        async def pure(n: int) -> int:
            return n

        tasks = [pure(1), pure(2), pure(3), pure(4), pure(5), pure(6)]
        result = await count_evens(tasks)
        assert result == 3  # 2, 4, 6


class TestCheckInProcessing:
    """チェックイン処理のテスト"""

    @pytest.mark.asyncio
    async def test_store_check_in(self) -> None:
        ref: AsyncRef[dict[City, int]] = AsyncRef({})
        await store_check_in(ref, City("Tokyo"))
        await store_check_in(ref, City("Tokyo"))
        await store_check_in(ref, City("Osaka"))
        result = await ref.get()
        assert result[City("Tokyo")] == 2
        assert result[City("Osaka")] == 1

    @pytest.mark.asyncio
    async def test_update_ranking(self) -> None:
        check_ins: AsyncRef[dict[City, int]] = AsyncRef({
            City("Tokyo"): 10,
            City("Osaka"): 5,
            City("Kyoto"): 8,
        })
        ranking: AsyncRef[list[CityStats]] = AsyncRef([])
        await update_ranking(check_ins, ranking)
        result = await ranking.get()
        assert len(result) == 3
        assert result[0].city.name == "Tokyo"


class TestFiber:
    """Fiber のテスト"""

    @pytest.mark.asyncio
    async def test_start_fiber_and_join(self) -> None:
        async def task() -> int:
            await asyncio.sleep(0.01)
            return 42

        fiber = start_fiber(task())
        result = await fiber.join()
        assert result == 42

    @pytest.mark.asyncio
    async def test_fiber_cancel(self) -> None:
        async def infinite_task() -> None:
            while True:
                await asyncio.sleep(0.01)

        fiber = start_fiber(infinite_task())
        await asyncio.sleep(0.05)
        fiber.cancel()

        with pytest.raises(asyncio.CancelledError):
            await fiber.join()

    @pytest.mark.asyncio
    async def test_fiber_is_done(self) -> None:
        async def quick_task() -> int:
            return 1

        fiber = start_fiber(quick_task())
        await asyncio.sleep(0.01)
        assert fiber.is_done


class TestCollectFor:
    """タイムアウト付き収集のテスト"""

    @pytest.mark.asyncio
    async def test_collect_for_basic(self) -> None:
        results = await collect_for(0.25, 0.05)
        assert len(results) >= 1
        assert all(0 <= r <= 99 for r in results)


class TestApplyUpdates:
    """並行マップ更新のテスト"""

    @pytest.mark.asyncio
    async def test_apply_updates_basic(self) -> None:
        updates = [Update("a", 1), Update("b", 2), Update("c", 3)]
        result = await apply_updates(updates)
        assert result["a"] == 1
        assert result["b"] == 2
        assert result["c"] == 3

    @pytest.mark.asyncio
    async def test_apply_updates_with_overwrite(self) -> None:
        updates = [Update("a", 1), Update("b", 2), Update("a", 3)]
        result = await apply_updates(updates)
        assert result["a"] in [1, 3]  # 並行実行のため順序は不定
        assert result["b"] == 2


class TestProcessingCheckIns:
    """バックグラウンド処理のテスト"""

    @pytest.mark.asyncio
    async def test_process_check_ins_background(self) -> None:
        cities = [City("Tokyo"), City("Osaka"), City("Tokyo"), City("Kyoto")]
        processing = await process_check_ins_background(cities)

        await asyncio.sleep(0.1)
        ranking = await processing.current_ranking()
        processing.stop()

        city_names = [s.city.name for s in ranking]
        assert "Tokyo" in city_names


class TestSequentialVsParallel:
    """逐次 vs 並列の比較テスト"""

    @pytest.mark.asyncio
    async def test_sequential_sleep_time(self) -> None:
        start = time.time()
        await sequential_sleep(3, 0.05)
        elapsed = time.time() - start
        assert elapsed >= 0.15  # 0.05 * 3 = 0.15

    @pytest.mark.asyncio
    async def test_parallel_sleep_time(self) -> None:
        start = time.time()
        await parallel_sleep(3, 0.05)
        elapsed = time.time() - start
        assert elapsed < 0.1  # 並列なので約0.05秒


class TestThreadBasedParallel:
    """スレッドベースの並列処理のテスト"""

    def test_run_in_threads(self) -> None:
        def task1() -> int:
            return 1

        def task2() -> int:
            return 2

        def task3() -> int:
            return 3

        results = run_in_threads([task1, task2, task3])
        assert sorted(results) == [1, 2, 3]

    def test_parallel_map(self) -> None:
        result = parallel_map([1, 2, 3, 4, 5], lambda x: x * 2)
        assert result == [2, 4, 6, 8, 10]

    def test_parallel_map_preserves_order(self) -> None:
        result = parallel_map(list(range(10)), lambda x: x * x)
        assert result == [0, 1, 4, 9, 16, 25, 36, 49, 64, 81]
