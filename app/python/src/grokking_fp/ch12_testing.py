"""第12章: テスト戦略

関数型プログラミングにおけるテスト戦略を学びます。
SearchReport、プロパティベーステスト、スタブ/モックの使い方を習得します。
"""

from dataclasses import dataclass, field
from typing import Callable, Generic, TypeVar

from result import Err, Ok, Result

from grokking_fp.ch11_travel_guide import (
    Attraction,
    AttractionOrdering,
    DataAccess,
    Hotel,
    Location,
    LocationId,
    Movie,
    MusicArtist,
    TestDataAccess,
)

T = TypeVar("T")


# =============================================================================
# 12.1 SearchReport - 検索結果のメタデータ
# =============================================================================


@dataclass(frozen=True)
class SearchReport:
    """検索結果のメタデータ。

    Examples:
        >>> report = SearchReport(10, ["Error 1", "Error 2"])
        >>> report.attractions_searched
        10
        >>> len(report.errors)
        2
    """

    attractions_searched: int
    errors: list[str] = field(default_factory=list)

    @property
    def has_errors(self) -> bool:
        """エラーがあるか。"""
        return len(self.errors) > 0

    @property
    def error_count(self) -> int:
        """エラーの数。"""
        return len(self.errors)


@dataclass(frozen=True)
class TravelGuideWithReport:
    """SearchReport 付きの旅行ガイド。

    Examples:
        >>> from grokking_fp.ch11_travel_guide import TEST_LOCATIONS, Attraction
        >>> loc = TEST_LOCATIONS["tokyo"]
        >>> attr = Attraction("Tokyo Tower", "A famous tower", loc)
        >>> report = SearchReport(5, [])
        >>> guide = TravelGuideWithReport(attr, ["Artist1"], report)
        >>> guide.search_report.attractions_searched
        5
    """

    attraction: Attraction
    subjects: list[str]
    search_report: SearchReport


# =============================================================================
# 12.2 SearchReport 付きの旅行ガイド生成
# =============================================================================


def travel_guide_with_report(
    data: DataAccess,
    attraction_name: str,
    limit: int = 3,
) -> TravelGuideWithReport | None:
    """SearchReport 付きの旅行ガイドを生成する。

    Examples:
        >>> da = TestDataAccess()
        >>> guide = travel_guide_with_report(da, "Tokyo", 3)
        >>> guide is not None
        True
        >>> guide.search_report.attractions_searched if guide else 0
        2
    """
    attractions = data.find_attractions(
        attraction_name,
        AttractionOrdering.BY_LOCATION_POPULATION,
        limit,
    )

    if not attractions:
        return None

    attraction = attractions[0]
    location_id = attraction.location.id

    # アーティストと映画を取得
    artists_result = data.find_artists_from_location(location_id, 2)
    movies_result = data.find_movies_about_location(location_id, 2)

    # エラーを収集
    errors: list[str] = []
    if artists_result.is_err():
        errors.append(artists_result.err())  # type: ignore
    if movies_result.is_err():
        errors.append(movies_result.err())  # type: ignore

    # 成功した結果のみを使用
    artists = artists_result.ok() if artists_result.is_ok() else []
    movies = movies_result.ok() if movies_result.is_ok() else []

    subjects = [a.name for a in (artists or [])] + [m.name for m in (movies or [])]
    search_report = SearchReport(len(attractions), errors)

    return TravelGuideWithReport(attraction, subjects, search_report)


def travel_guides_for_all(
    data: DataAccess,
    attraction_name: str,
    limit: int = 3,
) -> list[TravelGuideWithReport]:
    """複数のアトラクションに対する旅行ガイドを生成する。

    Examples:
        >>> da = TestDataAccess()
        >>> guides = travel_guides_for_all(da, "Tokyo", 2)
        >>> len(guides)
        2
    """
    attractions = data.find_attractions(
        attraction_name,
        AttractionOrdering.BY_LOCATION_POPULATION,
        limit,
    )

    results: list[TravelGuideWithReport] = []

    for attraction in attractions:
        location_id = attraction.location.id

        artists_result = data.find_artists_from_location(location_id, 2)
        movies_result = data.find_movies_about_location(location_id, 2)

        errors: list[str] = []
        if artists_result.is_err():
            errors.append(artists_result.err())  # type: ignore
        if movies_result.is_err():
            errors.append(movies_result.err())  # type: ignore

        artists = artists_result.ok() if artists_result.is_ok() else []
        movies = movies_result.ok() if movies_result.is_ok() else []

        subjects = [a.name for a in (artists or [])] + [m.name for m in (movies or [])]
        search_report = SearchReport(len(attractions), errors)

        results.append(TravelGuideWithReport(attraction, subjects, search_report))

    return results


# =============================================================================
# 12.3 プロパティベーステスト用のジェネレータ
# =============================================================================


def generate_location_id() -> LocationId:
    """ランダムな LocationId を生成する。

    Examples:
        >>> lid = generate_location_id()
        >>> lid.startswith("Q")
        True
    """
    import random
    return LocationId(f"Q{random.randint(1, 1000000)}")


def generate_location(
    min_population: int = 0,
    max_population: int = 10000000,
) -> Location:
    """ランダムな Location を生成する。

    Examples:
        >>> loc = generate_location(100, 1000)
        >>> 100 <= loc.population <= 1000
        True
    """
    import random
    import string

    name = "".join(random.choices(string.ascii_letters, k=random.randint(3, 10)))
    return Location(
        id=generate_location_id(),
        name=name,
        population=random.randint(min_population, max_population),
    )


def generate_attraction(location: Location | None = None) -> Attraction:
    """ランダムな Attraction を生成する。

    Examples:
        >>> attr = generate_attraction()
        >>> len(attr.name) > 0
        True
    """
    import random
    import string

    if location is None:
        location = generate_location()

    name = "".join(random.choices(string.ascii_letters, k=random.randint(5, 20)))
    description = (
        "".join(random.choices(string.ascii_letters + " ", k=random.randint(10, 50)))
        if random.random() > 0.3
        else None
    )

    return Attraction(name=name, description=description, location=location)


def generate_locations(count: int) -> list[Location]:
    """複数のランダムな Location を生成する。

    Examples:
        >>> locs = generate_locations(5)
        >>> len(locs)
        5
    """
    return [generate_location() for _ in range(count)]


def generate_attractions(count: int) -> list[Attraction]:
    """複数のランダムな Attraction を生成する。

    Examples:
        >>> attrs = generate_attractions(5)
        >>> len(attrs)
        5
    """
    return [generate_attraction() for _ in range(count)]


# =============================================================================
# 12.4 プロパティ検証関数
# =============================================================================


def check_filter_result_size(
    locations: list[Location],
    min_population: int,
    filtered: list[Location],
) -> bool:
    """フィルタ結果のサイズが入力以下であることを検証する。

    Examples:
        >>> locs = [Location(LocationId("1"), "A", 100)]
        >>> filtered = [l for l in locs if l.population >= 50]
        >>> check_filter_result_size(locs, 50, filtered)
        True
    """
    return len(filtered) <= len(locations)


def check_filter_all_meet_condition(
    filtered: list[Location],
    min_population: int,
) -> bool:
    """フィルタ結果のすべての要素が条件を満たすことを検証する。

    Examples:
        >>> locs = [Location(LocationId("1"), "A", 100)]
        >>> check_filter_all_meet_condition(locs, 50)
        True
        >>> check_filter_all_meet_condition(locs, 150)
        False
    """
    return all(loc.population >= min_population for loc in filtered)


def check_filter_no_false_negatives(
    locations: list[Location],
    min_population: int,
    filtered: list[Location],
) -> bool:
    """条件を満たす要素がすべて結果に含まれることを検証する。

    Examples:
        >>> locs = [Location(LocationId("1"), "A", 100)]
        >>> filtered = [l for l in locs if l.population >= 50]
        >>> check_filter_no_false_negatives(locs, 50, filtered)
        True
    """
    expected = {loc for loc in locations if loc.population >= min_population}
    actual = set(filtered)
    return expected == actual


def check_search_report_errors_bounded(
    report: SearchReport,
    max_errors: int,
) -> bool:
    """SearchReport のエラー数が上限以下であることを検証する。

    Examples:
        >>> report = SearchReport(10, ["error1"])
        >>> check_search_report_errors_bounded(report, 2)
        True
    """
    return report.error_count <= max_errors


# =============================================================================
# 12.5 Assertion ヘルパー
# =============================================================================


class AssertionResult:
    """アサーション結果を表す。"""

    def __init__(self, success: bool, message: str = "") -> None:
        self.success = success
        self.message = message

    def __bool__(self) -> bool:
        return self.success


def assert_equals(actual: T, expected: T, name: str = "value") -> AssertionResult:
    """等値をアサートする。

    Examples:
        >>> result = assert_equals(1, 1)
        >>> result.success
        True
    """
    if actual == expected:
        return AssertionResult(True)
    return AssertionResult(False, f"{name}: expected {expected}, got {actual}")


def assert_true(condition: bool, message: str = "") -> AssertionResult:
    """条件が True であることをアサートする。

    Examples:
        >>> result = assert_true(1 < 2)
        >>> result.success
        True
    """
    if condition:
        return AssertionResult(True)
    return AssertionResult(False, message or "Condition was False")


def assert_false(condition: bool, message: str = "") -> AssertionResult:
    """条件が False であることをアサートする。

    Examples:
        >>> result = assert_false(1 > 2)
        >>> result.success
        True
    """
    return assert_true(not condition, message or "Condition was True")


def assert_none(value: T | None, name: str = "value") -> AssertionResult:
    """値が None であることをアサートする。

    Examples:
        >>> result = assert_none(None)
        >>> result.success
        True
    """
    if value is None:
        return AssertionResult(True)
    return AssertionResult(False, f"{name}: expected None, got {value}")


def assert_not_none(value: T | None, name: str = "value") -> AssertionResult:
    """値が None でないことをアサートする。

    Examples:
        >>> result = assert_not_none(42)
        >>> result.success
        True
    """
    if value is not None:
        return AssertionResult(True)
    return AssertionResult(False, f"{name}: expected not None")


def assert_list_size(lst: list[T], expected_size: int, name: str = "list") -> AssertionResult:
    """リストのサイズをアサートする。

    Examples:
        >>> result = assert_list_size([1, 2, 3], 3)
        >>> result.success
        True
    """
    if len(lst) == expected_size:
        return AssertionResult(True)
    return AssertionResult(False, f"{name}: expected size {expected_size}, got {len(lst)}")


def assert_list_not_empty(lst: list[T], name: str = "list") -> AssertionResult:
    """リストが空でないことをアサートする。

    Examples:
        >>> result = assert_list_not_empty([1, 2])
        >>> result.success
        True
    """
    if len(lst) > 0:
        return AssertionResult(True)
    return AssertionResult(False, f"{name}: expected non-empty list")


# =============================================================================
# 12.6 テストランナー
# =============================================================================


@dataclass
class TestResult:
    """テスト結果。"""

    name: str
    passed: bool
    message: str = ""


class TestSuite:
    """テストスイート。

    Examples:
        >>> suite = TestSuite("example")
        >>> suite.add_test("test1", lambda: AssertionResult(True))
        >>> results = suite.run()
        >>> results[0].passed
        True
    """

    def __init__(self, name: str) -> None:
        self.name = name
        self._tests: list[tuple[str, Callable[[], AssertionResult]]] = []

    def add_test(self, name: str, test_fn: Callable[[], AssertionResult]) -> None:
        """テストを追加する。"""
        self._tests.append((name, test_fn))

    def run(self) -> list[TestResult]:
        """すべてのテストを実行する。"""
        results: list[TestResult] = []
        for name, test_fn in self._tests:
            try:
                result = test_fn()
                results.append(TestResult(name, result.success, result.message))
            except Exception as e:
                results.append(TestResult(name, False, str(e)))
        return results

    def run_and_report(self) -> tuple[int, int]:
        """テストを実行して結果を報告する。"""
        results = self.run()
        passed = sum(1 for r in results if r.passed)
        failed = sum(1 for r in results if not r.passed)
        return passed, failed


# =============================================================================
# 12.7 プロパティベーステストランナー
# =============================================================================


def run_property_test(
    name: str,
    property_fn: Callable[[], bool],
    iterations: int = 100,
) -> TestResult:
    """プロパティベーステストを実行する。

    Examples:
        >>> result = run_property_test("always true", lambda: True, 10)
        >>> result.passed
        True
    """
    for i in range(iterations):
        try:
            if not property_fn():
                return TestResult(name, False, f"Failed at iteration {i + 1}")
        except Exception as e:
            return TestResult(name, False, f"Exception at iteration {i + 1}: {e}")
    return TestResult(name, True, f"Passed {iterations} iterations")


def property_filter_result_size() -> bool:
    """フィルタ結果のサイズプロパティをテストする。"""
    import random

    locations = generate_locations(random.randint(0, 20))
    min_pop = random.randint(0, 10000000)
    filtered = [loc for loc in locations if loc.population >= min_pop]
    return check_filter_result_size(locations, min_pop, filtered)


def property_filter_all_meet_condition() -> bool:
    """フィルタ結果の条件充足プロパティをテストする。"""
    import random

    locations = generate_locations(random.randint(0, 20))
    min_pop = random.randint(0, 10000000)
    filtered = [loc for loc in locations if loc.population >= min_pop]
    return check_filter_all_meet_condition(filtered, min_pop)


def property_filter_no_false_negatives() -> bool:
    """フィルタの完全性プロパティをテストする。"""
    import random

    locations = generate_locations(random.randint(0, 20))
    min_pop = random.randint(0, 10000000)
    filtered = [loc for loc in locations if loc.population >= min_pop]
    return check_filter_no_false_negatives(locations, min_pop, filtered)


# =============================================================================
# 12.8 統合テスト用のヘルパー
# =============================================================================


def create_test_scenario(
    attraction_count: int = 3,
    artist_count: int = 2,
    movie_count: int = 2,
) -> DataAccess:
    """テストシナリオ用のデータアクセスを作成する。

    Examples:
        >>> da = create_test_scenario(2, 1, 1)
        >>> attrs = da.find_attractions("", AttractionOrdering.BY_NAME, 10)
        >>> len(attrs) == 2
        True
    """

    class ScenarioDataAccess(DataAccess):
        def __init__(self) -> None:
            self._attractions = generate_attractions(attraction_count)
            self._location_id = self._attractions[0].location.id if self._attractions else generate_location_id()

        def find_attractions(
            self,
            name: str,
            ordering: AttractionOrdering,
            limit: int,
        ) -> list[Attraction]:
            return self._attractions[:limit]

        def find_artists_from_location(
            self,
            location_id: LocationId,
            limit: int,
        ) -> Result[list[MusicArtist], str]:
            artists = [MusicArtist(f"Artist{i}") for i in range(artist_count)]
            return Ok(artists[:limit])

        def find_movies_about_location(
            self,
            location_id: LocationId,
            limit: int,
        ) -> Result[list[Movie], str]:
            movies = [Movie(f"Movie{i}") for i in range(movie_count)]
            return Ok(movies[:limit])

        def find_hotels_near_location(
            self,
            location_id: LocationId,
            limit: int,
        ) -> Result[list[Hotel], str]:
            return Ok([])

    return ScenarioDataAccess()


def verify_travel_guide(
    guide: TravelGuideWithReport,
) -> list[AssertionResult]:
    """旅行ガイドを検証する。

    Examples:
        >>> from grokking_fp.ch11_travel_guide import TEST_LOCATIONS, Attraction
        >>> loc = TEST_LOCATIONS["tokyo"]
        >>> attr = Attraction("Test", None, loc)
        >>> report = SearchReport(1, [])
        >>> guide = TravelGuideWithReport(attr, [], report)
        >>> results = verify_travel_guide(guide)
        >>> all(r.success for r in results)
        True
    """
    results: list[AssertionResult] = []

    # アトラクション名が空でないこと
    results.append(
        assert_true(
            len(guide.attraction.name) > 0,
            "Attraction name should not be empty"
        )
    )

    # ロケーションが有効であること
    results.append(
        assert_true(
            guide.attraction.location.population >= 0,
            "Population should be non-negative"
        )
    )

    # 検索数が正であること
    results.append(
        assert_true(
            guide.search_report.attractions_searched > 0,
            "Attractions searched should be positive"
        )
    )

    # エラー数が検索数（アーティスト + 映画）以下であること
    results.append(
        assert_true(
            guide.search_report.error_count <= 2,
            "Error count should be at most 2 (artists + movies)"
        )
    )

    return results
