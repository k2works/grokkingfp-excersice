"""第11章: 実践的なアプリケーション構築

Python で TravelGuide アプリケーションを構築します。
ドメインモデル、データアクセス層の抽象化、リソース管理、キャッシュ実装を学びます。
"""

from abc import ABC, abstractmethod
from contextlib import contextmanager
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Callable, Generator, Generic, NewType, TypeVar

from result import Err, Ok, Result

T = TypeVar("T")
U = TypeVar("U")


# =============================================================================
# 11.1 ドメインモデル
# =============================================================================


# 値オブジェクト: LocationId
LocationId = NewType("LocationId", str)


@dataclass(frozen=True)
class Location:
    """ロケーション（場所）を表す。

    Examples:
        >>> loc = Location(LocationId("Q123"), "Tokyo", 14000000)
        >>> loc.name
        'Tokyo'
    """

    id: LocationId
    name: str
    population: int


@dataclass(frozen=True)
class Attraction:
    """アトラクション（観光地）を表す。

    Examples:
        >>> loc = Location(LocationId("Q123"), "Tokyo", 14000000)
        >>> attr = Attraction("Tokyo Tower", "A famous tower", loc)
        >>> attr.name
        'Tokyo Tower'
    """

    name: str
    description: str | None
    location: Location


@dataclass(frozen=True)
class MusicArtist:
    """ミュージシャンを表す。

    Examples:
        >>> artist = MusicArtist("The Beatles")
        >>> artist.name
        'The Beatles'
    """

    name: str


@dataclass(frozen=True)
class Movie:
    """映画を表す。

    Examples:
        >>> movie = Movie("Lost in Translation")
        >>> movie.name
        'Lost in Translation'
    """

    name: str


@dataclass(frozen=True)
class Hotel:
    """ホテルを表す。

    Examples:
        >>> loc = Location(LocationId("Q123"), "Tokyo", 14000000)
        >>> hotel = Hotel("Grand Hotel", 4.5, loc)
        >>> hotel.rating
        4.5
    """

    name: str
    rating: float
    location: Location


class AttractionOrdering(Enum):
    """アトラクションのソート順。"""

    BY_NAME = auto()
    BY_LOCATION_POPULATION = auto()


@dataclass(frozen=True)
class TravelGuide:
    """旅行ガイドを表す。

    Examples:
        >>> loc = Location(LocationId("Q123"), "Tokyo", 14000000)
        >>> attr = Attraction("Tokyo Tower", "A famous tower", loc)
        >>> guide = TravelGuide(attr, ["The Beatles", "Lost in Translation"])
        >>> guide.attraction.name
        'Tokyo Tower'
    """

    attraction: Attraction
    subjects: list[str]


# =============================================================================
# 11.2 データアクセス層の抽象化
# =============================================================================


class DataAccess(ABC):
    """データアクセス層の抽象インターフェース。

    外部データソースへのアクセスを抽象化します。
    """

    @abstractmethod
    def find_attractions(
        self,
        name: str,
        ordering: AttractionOrdering,
        limit: int,
    ) -> list[Attraction]:
        """アトラクションを検索する。"""
        pass

    @abstractmethod
    def find_artists_from_location(
        self,
        location_id: LocationId,
        limit: int,
    ) -> Result[list[MusicArtist], str]:
        """ロケーションに関連するアーティストを検索する。"""
        pass

    @abstractmethod
    def find_movies_about_location(
        self,
        location_id: LocationId,
        limit: int,
    ) -> Result[list[Movie], str]:
        """ロケーションに関する映画を検索する。"""
        pass

    @abstractmethod
    def find_hotels_near_location(
        self,
        location_id: LocationId,
        limit: int,
    ) -> Result[list[Hotel], str]:
        """ロケーション近くのホテルを検索する。"""
        pass


# =============================================================================
# 11.3 テスト用スタブ実装
# =============================================================================


# テストデータ
TEST_LOCATIONS = {
    "tokyo": Location(LocationId("Q1490"), "Tokyo", 14000000),
    "kyoto": Location(LocationId("Q34600"), "Kyoto", 1500000),
    "osaka": Location(LocationId("Q35765"), "Osaka", 2700000),
}

TEST_ATTRACTIONS = [
    Attraction("Tokyo Tower", "A famous communications tower", TEST_LOCATIONS["tokyo"]),
    Attraction("Senso-ji Temple", "Ancient Buddhist temple", TEST_LOCATIONS["tokyo"]),
    Attraction("Kinkaku-ji", "Golden Pavilion temple", TEST_LOCATIONS["kyoto"]),
    Attraction("Fushimi Inari Shrine", "Famous for torii gates", TEST_LOCATIONS["kyoto"]),
    Attraction("Osaka Castle", "Historic castle", TEST_LOCATIONS["osaka"]),
]

TEST_ARTISTS = {
    LocationId("Q1490"): [MusicArtist("Utada Hikaru"), MusicArtist("YMO")],
    LocationId("Q34600"): [MusicArtist("Kyoto Jazz Massive")],
    LocationId("Q35765"): [MusicArtist("Dreams Come True")],
}

TEST_MOVIES = {
    LocationId("Q1490"): [Movie("Lost in Translation"), Movie("Tokyo Story")],
    LocationId("Q34600"): [Movie("Memoirs of a Geisha")],
    LocationId("Q35765"): [Movie("Black Rain")],
}

TEST_HOTELS = {
    LocationId("Q1490"): [
        Hotel("Park Hyatt Tokyo", 4.8, TEST_LOCATIONS["tokyo"]),
        Hotel("Aman Tokyo", 4.9, TEST_LOCATIONS["tokyo"]),
    ],
    LocationId("Q34600"): [
        Hotel("The Ritz-Carlton Kyoto", 4.7, TEST_LOCATIONS["kyoto"]),
    ],
    LocationId("Q35765"): [
        Hotel("The St. Regis Osaka", 4.6, TEST_LOCATIONS["osaka"]),
    ],
}


class TestDataAccess(DataAccess):
    """テスト用のデータアクセス実装。

    Examples:
        >>> da = TestDataAccess()
        >>> attractions = da.find_attractions("Tokyo", AttractionOrdering.BY_NAME, 2)
        >>> len(attractions) <= 2
        True
    """

    def find_attractions(
        self,
        name: str,
        ordering: AttractionOrdering,
        limit: int,
    ) -> list[Attraction]:
        # 名前でフィルタリング
        filtered = [
            a for a in TEST_ATTRACTIONS
            if name.lower() in a.name.lower() or name.lower() in a.location.name.lower()
        ]

        # ソート
        if ordering == AttractionOrdering.BY_NAME:
            filtered.sort(key=lambda a: a.name)
        else:
            filtered.sort(key=lambda a: a.location.population, reverse=True)

        return filtered[:limit]

    def find_artists_from_location(
        self,
        location_id: LocationId,
        limit: int,
    ) -> Result[list[MusicArtist], str]:
        artists = TEST_ARTISTS.get(location_id, [])
        return Ok(artists[:limit])

    def find_movies_about_location(
        self,
        location_id: LocationId,
        limit: int,
    ) -> Result[list[Movie], str]:
        movies = TEST_MOVIES.get(location_id, [])
        return Ok(movies[:limit])

    def find_hotels_near_location(
        self,
        location_id: LocationId,
        limit: int,
    ) -> Result[list[Hotel], str]:
        hotels = TEST_HOTELS.get(location_id, [])
        return Ok(hotels[:limit])


# =============================================================================
# 11.4 エラーを返すスタブ
# =============================================================================


class FailingDataAccess(DataAccess):
    """エラーを返すテスト用データアクセス。

    Examples:
        >>> da = FailingDataAccess()
        >>> result = da.find_artists_from_location(LocationId("Q1"), 10)
        >>> result.is_err()
        True
    """

    def __init__(
        self,
        artists_error: str | None = "Network error",
        movies_error: str | None = "Timeout",
        hotels_error: str | None = None,
    ) -> None:
        self._artists_error = artists_error
        self._movies_error = movies_error
        self._hotels_error = hotels_error

    def find_attractions(
        self,
        name: str,
        ordering: AttractionOrdering,
        limit: int,
    ) -> list[Attraction]:
        return TEST_ATTRACTIONS[:limit]

    def find_artists_from_location(
        self,
        location_id: LocationId,
        limit: int,
    ) -> Result[list[MusicArtist], str]:
        if self._artists_error:
            return Err(self._artists_error)
        return Ok(TEST_ARTISTS.get(location_id, [])[:limit])

    def find_movies_about_location(
        self,
        location_id: LocationId,
        limit: int,
    ) -> Result[list[Movie], str]:
        if self._movies_error:
            return Err(self._movies_error)
        return Ok(TEST_MOVIES.get(location_id, [])[:limit])

    def find_hotels_near_location(
        self,
        location_id: LocationId,
        limit: int,
    ) -> Result[list[Hotel], str]:
        if self._hotels_error:
            return Err(self._hotels_error)
        return Ok(TEST_HOTELS.get(location_id, [])[:limit])


# =============================================================================
# 11.5 Resource - 安全なリソース管理
# =============================================================================


class Resource(Generic[T]):
    """安全なリソース管理を提供するクラス。

    Python の contextlib を関数型スタイルでラップします。

    Examples:
        >>> def acquire():
        ...     return "resource"
        >>> def release(r):
        ...     pass
        >>> resource = Resource(acquire, release)
        >>> with resource.use() as r:
        ...     print(r)
        resource
    """

    def __init__(
        self,
        acquire: Callable[[], T],
        release: Callable[[T], None],
    ) -> None:
        self._acquire = acquire
        self._release = release

    @contextmanager
    def use(self) -> Generator[T, None, None]:
        """リソースを取得し、使用後に解放する。"""
        resource = self._acquire()
        try:
            yield resource
        finally:
            self._release(resource)

    def map(self, f: Callable[[T], U]) -> "Resource[U]":
        """リソースの値を変換する。"""
        original_acquire = self._acquire
        original_release = self._release

        # 変換後の値とオリジナルリソースを保持
        state: dict[str, T | None] = {"original": None}

        def new_acquire() -> U:
            state["original"] = original_acquire()
            return f(state["original"])  # type: ignore

        def new_release(_: U) -> None:
            if state["original"] is not None:
                original_release(state["original"])  # type: ignore

        return Resource(new_acquire, new_release)


def make_resource(
    acquire: Callable[[], T],
    release: Callable[[T], None],
) -> Resource[T]:
    """リソースを作成する。

    Examples:
        >>> resource = make_resource(lambda: "data", lambda r: None)
        >>> with resource.use() as r:
        ...     r
        'data'
    """
    return Resource(acquire, release)


# =============================================================================
# 11.6 キャッシュの実装
# =============================================================================


class CachedDataAccess(DataAccess):
    """キャッシュ付きのデータアクセス。

    Examples:
        >>> base = TestDataAccess()
        >>> cached = CachedDataAccess(base)
        >>> r1 = cached.find_attractions("Tokyo", AttractionOrdering.BY_NAME, 2)
        >>> r2 = cached.find_attractions("Tokyo", AttractionOrdering.BY_NAME, 2)
        >>> r1 == r2
        True
    """

    def __init__(self, data_access: DataAccess) -> None:
        self._data_access = data_access
        self._attractions_cache: dict[str, list[Attraction]] = {}
        self._artists_cache: dict[str, Result[list[MusicArtist], str]] = {}
        self._movies_cache: dict[str, Result[list[Movie], str]] = {}
        self._hotels_cache: dict[str, Result[list[Hotel], str]] = {}

    def _make_key(self, *args: object) -> str:
        return "-".join(str(arg) for arg in args)

    def find_attractions(
        self,
        name: str,
        ordering: AttractionOrdering,
        limit: int,
    ) -> list[Attraction]:
        key = self._make_key(name, ordering.name, limit)
        if key not in self._attractions_cache:
            self._attractions_cache[key] = self._data_access.find_attractions(
                name, ordering, limit
            )
        return self._attractions_cache[key]

    def find_artists_from_location(
        self,
        location_id: LocationId,
        limit: int,
    ) -> Result[list[MusicArtist], str]:
        key = self._make_key("artists", location_id, limit)
        if key not in self._artists_cache:
            self._artists_cache[key] = self._data_access.find_artists_from_location(
                location_id, limit
            )
        return self._artists_cache[key]

    def find_movies_about_location(
        self,
        location_id: LocationId,
        limit: int,
    ) -> Result[list[Movie], str]:
        key = self._make_key("movies", location_id, limit)
        if key not in self._movies_cache:
            self._movies_cache[key] = self._data_access.find_movies_about_location(
                location_id, limit
            )
        return self._movies_cache[key]

    def find_hotels_near_location(
        self,
        location_id: LocationId,
        limit: int,
    ) -> Result[list[Hotel], str]:
        key = self._make_key("hotels", location_id, limit)
        if key not in self._hotels_cache:
            self._hotels_cache[key] = self._data_access.find_hotels_near_location(
                location_id, limit
            )
        return self._hotels_cache[key]

    def clear_cache(self) -> None:
        """キャッシュをクリアする。"""
        self._attractions_cache.clear()
        self._artists_cache.clear()
        self._movies_cache.clear()
        self._hotels_cache.clear()


# =============================================================================
# 11.7 アプリケーションの組み立て
# =============================================================================


def travel_guide(
    data: DataAccess,
    attraction_name: str,
) -> TravelGuide | None:
    """旅行ガイドを生成する。

    Examples:
        >>> da = TestDataAccess()
        >>> guide = travel_guide(da, "Tokyo Tower")
        >>> guide is not None
        True
        >>> guide.attraction.name if guide else None
        'Tokyo Tower'
    """
    attractions = data.find_attractions(
        attraction_name,
        AttractionOrdering.BY_LOCATION_POPULATION,
        1,
    )

    if not attractions:
        return None

    attraction = attractions[0]
    location_id = attraction.location.id

    # アーティストと映画を取得
    artists_result = data.find_artists_from_location(location_id, 2)
    movies_result = data.find_movies_about_location(location_id, 2)

    # 成功した結果のみを使用
    artists = artists_result.ok() if artists_result.is_ok() else []
    movies = movies_result.ok() if movies_result.is_ok() else []

    subjects = [a.name for a in (artists or [])] + [m.name for m in (movies or [])]

    return TravelGuide(attraction, subjects)


def travel_guide_with_hotels(
    data: DataAccess,
    attraction_name: str,
) -> tuple[TravelGuide | None, list[Hotel]]:
    """旅行ガイドとホテル情報を生成する。

    Examples:
        >>> da = TestDataAccess()
        >>> guide, hotels = travel_guide_with_hotels(da, "Tokyo Tower")
        >>> guide is not None
        True
        >>> len(hotels) > 0
        True
    """
    guide = travel_guide(data, attraction_name)

    if guide is None:
        return None, []

    location_id = guide.attraction.location.id
    hotels_result = data.find_hotels_near_location(location_id, 3)
    hotels = hotels_result.ok() if hotels_result.is_ok() else []

    return guide, hotels or []


# =============================================================================
# 11.8 ユーティリティ関数
# =============================================================================


def filter_popular_locations(
    locations: list[Location],
    min_population: int,
) -> list[Location]:
    """指定人口以上のロケーションをフィルタリングする。

    Examples:
        >>> locs = [
        ...     Location(LocationId("1"), "A", 100),
        ...     Location(LocationId("2"), "B", 500),
        ...     Location(LocationId("3"), "C", 300),
        ... ]
        >>> result = filter_popular_locations(locs, 200)
        >>> [l.name for l in result]
        ['B', 'C']
    """
    return [loc for loc in locations if loc.population >= min_population]


def sort_attractions_by_popularity(
    attractions: list[Attraction],
) -> list[Attraction]:
    """アトラクションを人気度（ロケーションの人口）でソートする。

    Examples:
        >>> loc1 = Location(LocationId("1"), "Small", 100)
        >>> loc2 = Location(LocationId("2"), "Big", 1000)
        >>> attrs = [
        ...     Attraction("A", None, loc1),
        ...     Attraction("B", None, loc2),
        ... ]
        >>> sorted_attrs = sort_attractions_by_popularity(attrs)
        >>> sorted_attrs[0].name
        'B'
    """
    return sorted(
        attractions,
        key=lambda a: a.location.population,
        reverse=True,
    )


def group_attractions_by_location(
    attractions: list[Attraction],
) -> dict[str, list[Attraction]]:
    """アトラクションをロケーションごとにグループ化する。

    Examples:
        >>> loc1 = Location(LocationId("1"), "Tokyo", 100)
        >>> loc2 = Location(LocationId("2"), "Kyoto", 200)
        >>> attrs = [
        ...     Attraction("A", None, loc1),
        ...     Attraction("B", None, loc1),
        ...     Attraction("C", None, loc2),
        ... ]
        >>> grouped = group_attractions_by_location(attrs)
        >>> len(grouped["Tokyo"])
        2
    """
    result: dict[str, list[Attraction]] = {}
    for attraction in attractions:
        location_name = attraction.location.name
        if location_name not in result:
            result[location_name] = []
        result[location_name].append(attraction)
    return result
