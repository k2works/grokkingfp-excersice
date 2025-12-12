"""第11章: 実践的なアプリケーション構築のテスト"""

import pytest

from grokking_fp.ch11_travel_guide import (
    Attraction,
    AttractionOrdering,
    CachedDataAccess,
    DataAccess,
    FailingDataAccess,
    Hotel,
    Location,
    LocationId,
    Movie,
    MusicArtist,
    Resource,
    TEST_ATTRACTIONS,
    TEST_LOCATIONS,
    TestDataAccess,
    TravelGuide,
    filter_popular_locations,
    group_attractions_by_location,
    make_resource,
    sort_attractions_by_popularity,
    travel_guide,
    travel_guide_with_hotels,
)


class TestDomainModels:
    """ドメインモデルのテスト"""

    def test_location_creation(self) -> None:
        loc = Location(LocationId("Q123"), "Tokyo", 14000000)
        assert loc.name == "Tokyo"
        assert loc.population == 14000000

    def test_attraction_creation(self) -> None:
        loc = Location(LocationId("Q123"), "Tokyo", 14000000)
        attr = Attraction("Tokyo Tower", "A famous tower", loc)
        assert attr.name == "Tokyo Tower"
        assert attr.location.name == "Tokyo"

    def test_attraction_without_description(self) -> None:
        loc = Location(LocationId("Q123"), "Tokyo", 14000000)
        attr = Attraction("Tokyo Tower", None, loc)
        assert attr.description is None

    def test_travel_guide_creation(self) -> None:
        loc = Location(LocationId("Q123"), "Tokyo", 14000000)
        attr = Attraction("Tokyo Tower", "A famous tower", loc)
        guide = TravelGuide(attr, ["Artist1", "Movie1"])
        assert guide.attraction.name == "Tokyo Tower"
        assert len(guide.subjects) == 2

    def test_hotel_creation(self) -> None:
        loc = Location(LocationId("Q123"), "Tokyo", 14000000)
        hotel = Hotel("Grand Hotel", 4.5, loc)
        assert hotel.name == "Grand Hotel"
        assert hotel.rating == 4.5

    def test_music_artist_creation(self) -> None:
        artist = MusicArtist("The Beatles")
        assert artist.name == "The Beatles"

    def test_movie_creation(self) -> None:
        movie = Movie("Lost in Translation")
        assert movie.name == "Lost in Translation"


class TestTestDataAccess:
    """TestDataAccess のテスト"""

    def test_find_attractions_by_name(self) -> None:
        da = TestDataAccess()
        attractions = da.find_attractions("Tokyo", AttractionOrdering.BY_NAME, 10)
        assert len(attractions) > 0
        assert all("Tokyo" in a.name or "Tokyo" in a.location.name for a in attractions)

    def test_find_attractions_limit(self) -> None:
        da = TestDataAccess()
        attractions = da.find_attractions("Tokyo", AttractionOrdering.BY_NAME, 1)
        assert len(attractions) <= 1

    def test_find_attractions_by_population(self) -> None:
        da = TestDataAccess()
        attractions = da.find_attractions("", AttractionOrdering.BY_LOCATION_POPULATION, 10)
        if len(attractions) >= 2:
            assert attractions[0].location.population >= attractions[1].location.population

    def test_find_artists_from_location(self) -> None:
        da = TestDataAccess()
        result = da.find_artists_from_location(LocationId("Q1490"), 10)
        assert result.is_ok()
        artists = result.ok()
        assert artists is not None
        assert len(artists) > 0

    def test_find_movies_about_location(self) -> None:
        da = TestDataAccess()
        result = da.find_movies_about_location(LocationId("Q1490"), 10)
        assert result.is_ok()
        movies = result.ok()
        assert movies is not None
        assert len(movies) > 0

    def test_find_hotels_near_location(self) -> None:
        da = TestDataAccess()
        result = da.find_hotels_near_location(LocationId("Q1490"), 10)
        assert result.is_ok()
        hotels = result.ok()
        assert hotels is not None
        assert len(hotels) > 0


class TestFailingDataAccess:
    """FailingDataAccess のテスト"""

    def test_artists_error(self) -> None:
        da = FailingDataAccess(artists_error="Network error")
        result = da.find_artists_from_location(LocationId("Q1"), 10)
        assert result.is_err()
        assert result.err() == "Network error"

    def test_movies_error(self) -> None:
        da = FailingDataAccess(movies_error="Timeout")
        result = da.find_movies_about_location(LocationId("Q1"), 10)
        assert result.is_err()
        assert result.err() == "Timeout"

    def test_no_error_when_none(self) -> None:
        da = FailingDataAccess(artists_error=None, movies_error=None, hotels_error=None)
        result = da.find_artists_from_location(LocationId("Q1490"), 10)
        assert result.is_ok()


class TestResource:
    """Resource のテスト"""

    def test_resource_acquire_and_release(self) -> None:
        acquired = []
        released = []

        def acquire() -> str:
            acquired.append("resource")
            return "resource"

        def release(r: str) -> None:
            released.append(r)

        resource = make_resource(acquire, release)

        with resource.use() as r:
            assert r == "resource"
            assert len(acquired) == 1
            assert len(released) == 0

        assert len(released) == 1

    def test_resource_release_on_exception(self) -> None:
        released = []

        def acquire() -> str:
            return "resource"

        def release(r: str) -> None:
            released.append(r)

        resource = make_resource(acquire, release)

        with pytest.raises(ValueError):
            with resource.use():
                raise ValueError("test error")

        assert len(released) == 1

    def test_resource_map(self) -> None:
        resource = make_resource(lambda: 10, lambda r: None)
        mapped = resource.map(lambda x: x * 2)

        with mapped.use() as r:
            assert r == 20


class TestCachedDataAccess:
    """CachedDataAccess のテスト"""

    def test_cache_hit(self) -> None:
        call_count = [0]

        class CountingDataAccess(TestDataAccess):
            def find_attractions(
                self,
                name: str,
                ordering: AttractionOrdering,
                limit: int,
            ) -> list[Attraction]:
                call_count[0] += 1
                return super().find_attractions(name, ordering, limit)

        base = CountingDataAccess()
        cached = CachedDataAccess(base)

        # 最初の呼び出し
        cached.find_attractions("Tokyo", AttractionOrdering.BY_NAME, 2)
        assert call_count[0] == 1

        # 2回目の呼び出し（キャッシュから）
        cached.find_attractions("Tokyo", AttractionOrdering.BY_NAME, 2)
        assert call_count[0] == 1  # 増えていない

    def test_cache_miss_different_params(self) -> None:
        call_count = [0]

        class CountingDataAccess(TestDataAccess):
            def find_attractions(
                self,
                name: str,
                ordering: AttractionOrdering,
                limit: int,
            ) -> list[Attraction]:
                call_count[0] += 1
                return super().find_attractions(name, ordering, limit)

        base = CountingDataAccess()
        cached = CachedDataAccess(base)

        cached.find_attractions("Tokyo", AttractionOrdering.BY_NAME, 2)
        cached.find_attractions("Kyoto", AttractionOrdering.BY_NAME, 2)
        assert call_count[0] == 2

    def test_clear_cache(self) -> None:
        call_count = [0]

        class CountingDataAccess(TestDataAccess):
            def find_attractions(
                self,
                name: str,
                ordering: AttractionOrdering,
                limit: int,
            ) -> list[Attraction]:
                call_count[0] += 1
                return super().find_attractions(name, ordering, limit)

        base = CountingDataAccess()
        cached = CachedDataAccess(base)

        cached.find_attractions("Tokyo", AttractionOrdering.BY_NAME, 2)
        cached.clear_cache()
        cached.find_attractions("Tokyo", AttractionOrdering.BY_NAME, 2)
        assert call_count[0] == 2


class TestTravelGuide:
    """travel_guide 関数のテスト"""

    def test_travel_guide_found(self) -> None:
        da = TestDataAccess()
        guide = travel_guide(da, "Tokyo Tower")
        assert guide is not None
        assert guide.attraction.name == "Tokyo Tower"

    def test_travel_guide_not_found(self) -> None:
        da = TestDataAccess()
        guide = travel_guide(da, "NonExistent")
        assert guide is None

    def test_travel_guide_has_subjects(self) -> None:
        da = TestDataAccess()
        guide = travel_guide(da, "Tokyo")
        assert guide is not None
        assert len(guide.subjects) > 0

    def test_travel_guide_with_failing_data(self) -> None:
        da = FailingDataAccess()
        guide = travel_guide(da, "Tokyo")
        # エラーがあっても結果は返る（空のsubjects）
        assert guide is not None
        assert guide.subjects == []


class TestTravelGuideWithHotels:
    """travel_guide_with_hotels 関数のテスト"""

    def test_returns_guide_and_hotels(self) -> None:
        da = TestDataAccess()
        guide, hotels = travel_guide_with_hotels(da, "Tokyo Tower")
        assert guide is not None
        assert len(hotels) > 0

    def test_returns_none_when_not_found(self) -> None:
        da = TestDataAccess()
        guide, hotels = travel_guide_with_hotels(da, "NonExistent")
        assert guide is None
        assert hotels == []


class TestUtilityFunctions:
    """ユーティリティ関数のテスト"""

    def test_filter_popular_locations(self) -> None:
        locations = [
            Location(LocationId("1"), "Small", 100),
            Location(LocationId("2"), "Medium", 500),
            Location(LocationId("3"), "Large", 1000),
        ]
        result = filter_popular_locations(locations, 300)
        assert len(result) == 2
        assert all(l.population >= 300 for l in result)

    def test_filter_popular_locations_empty(self) -> None:
        result = filter_popular_locations([], 100)
        assert result == []

    def test_sort_attractions_by_popularity(self) -> None:
        loc1 = Location(LocationId("1"), "Small", 100)
        loc2 = Location(LocationId("2"), "Large", 1000)
        attrs = [
            Attraction("A", None, loc1),
            Attraction("B", None, loc2),
        ]
        sorted_attrs = sort_attractions_by_popularity(attrs)
        assert sorted_attrs[0].name == "B"
        assert sorted_attrs[1].name == "A"

    def test_group_attractions_by_location(self) -> None:
        loc1 = Location(LocationId("1"), "Tokyo", 100)
        loc2 = Location(LocationId("2"), "Kyoto", 200)
        attrs = [
            Attraction("A", None, loc1),
            Attraction("B", None, loc1),
            Attraction("C", None, loc2),
        ]
        grouped = group_attractions_by_location(attrs)
        assert len(grouped["Tokyo"]) == 2
        assert len(grouped["Kyoto"]) == 1
