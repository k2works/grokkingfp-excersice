"""第12章: テスト戦略のテスト"""

import pytest

from grokking_fp.ch11_travel_guide import (
    Attraction,
    AttractionOrdering,
    FailingDataAccess,
    Location,
    LocationId,
    TestDataAccess,
)
from grokking_fp.ch12_testing import (
    AssertionResult,
    SearchReport,
    TestResult,
    TestSuite,
    TravelGuideWithReport,
    assert_equals,
    assert_false,
    assert_list_not_empty,
    assert_list_size,
    assert_none,
    assert_not_none,
    assert_true,
    check_filter_all_meet_condition,
    check_filter_no_false_negatives,
    check_filter_result_size,
    check_search_report_errors_bounded,
    create_test_scenario,
    generate_attraction,
    generate_attractions,
    generate_location,
    generate_location_id,
    generate_locations,
    property_filter_all_meet_condition,
    property_filter_no_false_negatives,
    property_filter_result_size,
    run_property_test,
    travel_guide_with_report,
    travel_guides_for_all,
    verify_travel_guide,
)


class TestSearchReport:
    """SearchReport のテスト"""

    def test_search_report_creation(self) -> None:
        report = SearchReport(10, ["error1", "error2"])
        assert report.attractions_searched == 10
        assert report.error_count == 2

    def test_search_report_no_errors(self) -> None:
        report = SearchReport(5, [])
        assert not report.has_errors
        assert report.error_count == 0

    def test_search_report_has_errors(self) -> None:
        report = SearchReport(5, ["error"])
        assert report.has_errors
        assert report.error_count == 1


class TestTravelGuideWithReport:
    """TravelGuideWithReport のテスト"""

    def test_travel_guide_with_report_creation(self) -> None:
        loc = Location(LocationId("Q1"), "Tokyo", 1000000)
        attr = Attraction("Test", None, loc)
        report = SearchReport(5, [])
        guide = TravelGuideWithReport(attr, ["Subject1"], report)
        assert guide.attraction.name == "Test"
        assert len(guide.subjects) == 1
        assert guide.search_report.attractions_searched == 5

    def test_travel_guide_with_report_function(self) -> None:
        da = TestDataAccess()
        guide = travel_guide_with_report(da, "Tokyo", 3)
        assert guide is not None
        assert guide.search_report.attractions_searched > 0

    def test_travel_guide_with_report_not_found(self) -> None:
        da = TestDataAccess()
        guide = travel_guide_with_report(da, "NonExistent", 3)
        assert guide is None

    def test_travel_guide_with_report_with_errors(self) -> None:
        da = FailingDataAccess()
        guide = travel_guide_with_report(da, "Tokyo", 3)
        assert guide is not None
        assert guide.search_report.has_errors
        assert guide.search_report.error_count == 2  # artists + movies


class TestTravelGuidesForAll:
    """travel_guides_for_all のテスト"""

    def test_returns_multiple_guides(self) -> None:
        da = TestDataAccess()
        guides = travel_guides_for_all(da, "Tokyo", 2)
        assert len(guides) == 2

    def test_each_guide_has_report(self) -> None:
        da = TestDataAccess()
        guides = travel_guides_for_all(da, "Tokyo", 2)
        for guide in guides:
            assert guide.search_report.attractions_searched == 2


class TestGenerators:
    """ジェネレータ関数のテスト"""

    def test_generate_location_id(self) -> None:
        lid = generate_location_id()
        assert lid.startswith("Q")

    def test_generate_location(self) -> None:
        loc = generate_location(100, 1000)
        assert 100 <= loc.population <= 1000
        assert len(loc.name) > 0

    def test_generate_attraction(self) -> None:
        attr = generate_attraction()
        assert len(attr.name) > 0
        assert attr.location is not None

    def test_generate_locations(self) -> None:
        locs = generate_locations(5)
        assert len(locs) == 5

    def test_generate_attractions(self) -> None:
        attrs = generate_attractions(5)
        assert len(attrs) == 5


class TestPropertyVerification:
    """プロパティ検証関数のテスト"""

    def test_check_filter_result_size(self) -> None:
        locations = [Location(LocationId("1"), "A", 100)]
        filtered = locations  # No filtering
        assert check_filter_result_size(locations, 0, filtered)

    def test_check_filter_all_meet_condition(self) -> None:
        locations = [
            Location(LocationId("1"), "A", 100),
            Location(LocationId("2"), "B", 200),
        ]
        assert check_filter_all_meet_condition(locations, 50)
        assert not check_filter_all_meet_condition(locations, 150)

    def test_check_filter_no_false_negatives(self) -> None:
        locations = [
            Location(LocationId("1"), "A", 100),
            Location(LocationId("2"), "B", 200),
        ]
        filtered = [l for l in locations if l.population >= 150]
        assert check_filter_no_false_negatives(locations, 150, filtered)

    def test_check_search_report_errors_bounded(self) -> None:
        report = SearchReport(10, ["error1"])
        assert check_search_report_errors_bounded(report, 2)
        assert not check_search_report_errors_bounded(report, 0)


class TestAssertionHelpers:
    """アサーションヘルパーのテスト"""

    def test_assert_equals_success(self) -> None:
        result = assert_equals(1, 1)
        assert result.success

    def test_assert_equals_failure(self) -> None:
        result = assert_equals(1, 2)
        assert not result.success

    def test_assert_true_success(self) -> None:
        result = assert_true(1 < 2)
        assert result.success

    def test_assert_true_failure(self) -> None:
        result = assert_true(1 > 2)
        assert not result.success

    def test_assert_false_success(self) -> None:
        result = assert_false(1 > 2)
        assert result.success

    def test_assert_none_success(self) -> None:
        result = assert_none(None)
        assert result.success

    def test_assert_none_failure(self) -> None:
        result = assert_none(42)
        assert not result.success

    def test_assert_not_none_success(self) -> None:
        result = assert_not_none(42)
        assert result.success

    def test_assert_list_size_success(self) -> None:
        result = assert_list_size([1, 2, 3], 3)
        assert result.success

    def test_assert_list_not_empty_success(self) -> None:
        result = assert_list_not_empty([1, 2])
        assert result.success

    def test_assert_list_not_empty_failure(self) -> None:
        result = assert_list_not_empty([])
        assert not result.success


class TestTestSuite:
    """TestSuite のテスト"""

    def test_run_passing_tests(self) -> None:
        suite = TestSuite("example")
        suite.add_test("test1", lambda: AssertionResult(True))
        suite.add_test("test2", lambda: AssertionResult(True))
        results = suite.run()
        assert len(results) == 2
        assert all(r.passed for r in results)

    def test_run_failing_tests(self) -> None:
        suite = TestSuite("example")
        suite.add_test("test1", lambda: AssertionResult(False, "Failed"))
        results = suite.run()
        assert len(results) == 1
        assert not results[0].passed
        assert results[0].message == "Failed"

    def test_run_test_with_exception(self) -> None:
        def failing_test() -> AssertionResult:
            raise ValueError("Test error")

        suite = TestSuite("example")
        suite.add_test("test1", failing_test)
        results = suite.run()
        assert len(results) == 1
        assert not results[0].passed
        assert "Test error" in results[0].message

    def test_run_and_report(self) -> None:
        suite = TestSuite("example")
        suite.add_test("pass1", lambda: AssertionResult(True))
        suite.add_test("fail1", lambda: AssertionResult(False))
        suite.add_test("pass2", lambda: AssertionResult(True))
        passed, failed = suite.run_and_report()
        assert passed == 2
        assert failed == 1


class TestPropertyTests:
    """プロパティベーステストのテスト"""

    def test_run_property_test_success(self) -> None:
        result = run_property_test("always true", lambda: True, 10)
        assert result.passed

    def test_run_property_test_failure(self) -> None:
        counter = [0]

        def sometimes_false() -> bool:
            counter[0] += 1
            return counter[0] < 5

        result = run_property_test("sometimes false", sometimes_false, 10)
        assert not result.passed

    def test_property_filter_result_size(self) -> None:
        # ランダムテストを複数回実行
        for _ in range(10):
            assert property_filter_result_size()

    def test_property_filter_all_meet_condition(self) -> None:
        for _ in range(10):
            assert property_filter_all_meet_condition()

    def test_property_filter_no_false_negatives(self) -> None:
        for _ in range(10):
            assert property_filter_no_false_negatives()


class TestTestScenario:
    """テストシナリオのテスト"""

    def test_create_test_scenario(self) -> None:
        da = create_test_scenario(3, 2, 2)
        attrs = da.find_attractions("", AttractionOrdering.BY_NAME, 10)
        assert len(attrs) == 3

    def test_scenario_artists(self) -> None:
        da = create_test_scenario(1, 3, 0)
        result = da.find_artists_from_location(LocationId("Q1"), 10)
        assert result.is_ok()
        artists = result.ok()
        assert artists is not None
        assert len(artists) == 3

    def test_scenario_movies(self) -> None:
        da = create_test_scenario(1, 0, 4)
        result = da.find_movies_about_location(LocationId("Q1"), 10)
        assert result.is_ok()
        movies = result.ok()
        assert movies is not None
        assert len(movies) == 4


class TestVerifyTravelGuide:
    """verify_travel_guide のテスト"""

    def test_verify_valid_guide(self) -> None:
        loc = Location(LocationId("Q1"), "Tokyo", 1000000)
        attr = Attraction("Test Attraction", None, loc)
        report = SearchReport(5, [])
        guide = TravelGuideWithReport(attr, ["Subject1"], report)
        results = verify_travel_guide(guide)
        assert all(r.success for r in results)

    def test_verify_guide_with_errors(self) -> None:
        loc = Location(LocationId("Q1"), "Tokyo", 1000000)
        attr = Attraction("Test", None, loc)
        report = SearchReport(5, ["error1", "error2"])
        guide = TravelGuideWithReport(attr, [], report)
        results = verify_travel_guide(guide)
        assert all(r.success for r in results)  # 2 errors is still valid (artists + movies)
