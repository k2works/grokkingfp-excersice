package ch11;

import ch08.IO;
import io.vavr.collection.List;
import io.vavr.control.Option;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static ch11.TravelGuide.*;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第11章: CachedDataAccess のテスト
 */
@DisplayName("第11章: CachedDataAccess")
class CachedDataAccessTest {

    static final Location LONDON = new Location(
            new LocationId("Q84"),
            "London",
            8_908_081
    );

    static final Attraction TOWER_BRIDGE = new Attraction(
            "Tower Bridge",
            Option.none(),
            LONDON
    );

    static final Artist QUEEN = new Artist("Queen", 2_050_559);

    @Nested
    @DisplayName("キャッシュの基本動作")
    class BasicCachingTest {

        @Test
        @DisplayName("同じクエリは2回目以降キャッシュから返される")
        void cachedResultsAreReused() {
            AtomicInteger callCount = new AtomicInteger(0);

            DataAccess underlying = new DataAccess() {
                @Override
                public IO<List<Attraction>> findAttractions(String name, AttractionOrdering ordering, int limit) {
                    return IO.delay(() -> {
                        callCount.incrementAndGet();
                        return List.of(TOWER_BRIDGE);
                    });
                }

                @Override
                public IO<List<Artist>> findArtistsFromLocation(LocationId locationId, int limit) {
                    return IO.pure(List.empty());
                }

                @Override
                public IO<List<Movie>> findMoviesAboutLocation(LocationId locationId, int limit) {
                    return IO.pure(List.empty());
                }
            };

            DataAccess cached = CachedDataAccess.createUnsafe(underlying);

            // 1回目のクエリ
            cached.findAttractions("Bridge", AttractionOrdering.BY_NAME, 10).unsafeRun();
            assertThat(callCount.get()).isEqualTo(1);

            // 2回目のクエリ（同じパラメータ）
            cached.findAttractions("Bridge", AttractionOrdering.BY_NAME, 10).unsafeRun();
            assertThat(callCount.get()).isEqualTo(1); // キャッシュから返されるので呼び出し回数は増えない

            // 3回目のクエリ（異なるパラメータ）
            cached.findAttractions("Tower", AttractionOrdering.BY_NAME, 10).unsafeRun();
            assertThat(callCount.get()).isEqualTo(2); // 新しいクエリなので呼び出される
        }

        @Test
        @DisplayName("異なるパラメータは別々にキャッシュされる")
        void differentParametersAreCachedSeparately() {
            AtomicInteger callCount = new AtomicInteger(0);

            DataAccess underlying = new DataAccess() {
                @Override
                public IO<List<Attraction>> findAttractions(String name, AttractionOrdering ordering, int limit) {
                    return IO.delay(() -> {
                        callCount.incrementAndGet();
                        return List.of(TOWER_BRIDGE);
                    });
                }

                @Override
                public IO<List<Artist>> findArtistsFromLocation(LocationId locationId, int limit) {
                    return IO.pure(List.empty());
                }

                @Override
                public IO<List<Movie>> findMoviesAboutLocation(LocationId locationId, int limit) {
                    return IO.pure(List.empty());
                }
            };

            DataAccess cached = CachedDataAccess.createUnsafe(underlying);

            // 異なるパラメータでクエリ
            cached.findAttractions("Bridge", AttractionOrdering.BY_NAME, 10).unsafeRun();
            cached.findAttractions("Bridge", AttractionOrdering.BY_LOCATION_POPULATION, 10).unsafeRun();
            cached.findAttractions("Bridge", AttractionOrdering.BY_NAME, 5).unsafeRun();

            assertThat(callCount.get()).isEqualTo(3);

            // 同じパラメータで再度クエリ
            cached.findAttractions("Bridge", AttractionOrdering.BY_NAME, 10).unsafeRun();
            cached.findAttractions("Bridge", AttractionOrdering.BY_LOCATION_POPULATION, 10).unsafeRun();
            cached.findAttractions("Bridge", AttractionOrdering.BY_NAME, 5).unsafeRun();

            assertThat(callCount.get()).isEqualTo(3); // キャッシュから返される
        }
    }

    @Nested
    @DisplayName("IO での作成")
    class IOCreationTest {

        @Test
        @DisplayName("create で IO 経由でキャッシュ付き DataAccess を作成")
        void createReturnsIOOfCachedDataAccess() {
            DataAccess underlying = new DataAccess() {
                @Override
                public IO<List<Attraction>> findAttractions(String name, AttractionOrdering ordering, int limit) {
                    return IO.pure(List.of(TOWER_BRIDGE));
                }

                @Override
                public IO<List<Artist>> findArtistsFromLocation(LocationId locationId, int limit) {
                    return IO.pure(List.of(QUEEN));
                }

                @Override
                public IO<List<Movie>> findMoviesAboutLocation(LocationId locationId, int limit) {
                    return IO.pure(List.empty());
                }
            };

            DataAccess cached = CachedDataAccess.create(underlying).unsafeRun();

            List<Attraction> result = cached.findAttractions("Bridge", AttractionOrdering.BY_NAME, 10).unsafeRun();

            assertThat(result).containsExactly(TOWER_BRIDGE);
        }
    }

    @Nested
    @DisplayName("アーティストと映画のキャッシュ")
    class ArtistsAndMoviesCacheTest {

        @Test
        @DisplayName("アーティストのクエリもキャッシュされる")
        void artistsAreCached() {
            AtomicInteger callCount = new AtomicInteger(0);

            DataAccess underlying = new DataAccess() {
                @Override
                public IO<List<Attraction>> findAttractions(String name, AttractionOrdering ordering, int limit) {
                    return IO.pure(List.empty());
                }

                @Override
                public IO<List<Artist>> findArtistsFromLocation(LocationId locationId, int limit) {
                    return IO.delay(() -> {
                        callCount.incrementAndGet();
                        return List.of(QUEEN);
                    });
                }

                @Override
                public IO<List<Movie>> findMoviesAboutLocation(LocationId locationId, int limit) {
                    return IO.pure(List.empty());
                }
            };

            DataAccess cached = CachedDataAccess.createUnsafe(underlying);

            cached.findArtistsFromLocation(LONDON.id(), 2).unsafeRun();
            cached.findArtistsFromLocation(LONDON.id(), 2).unsafeRun();

            assertThat(callCount.get()).isEqualTo(1);
        }

        @Test
        @DisplayName("映画のクエリもキャッシュされる")
        void moviesAreCached() {
            AtomicInteger callCount = new AtomicInteger(0);
            Movie insideOut = new Movie("Inside Out", 857_611_174);

            DataAccess underlying = new DataAccess() {
                @Override
                public IO<List<Attraction>> findAttractions(String name, AttractionOrdering ordering, int limit) {
                    return IO.pure(List.empty());
                }

                @Override
                public IO<List<Artist>> findArtistsFromLocation(LocationId locationId, int limit) {
                    return IO.pure(List.empty());
                }

                @Override
                public IO<List<Movie>> findMoviesAboutLocation(LocationId locationId, int limit) {
                    return IO.delay(() -> {
                        callCount.incrementAndGet();
                        return List.of(insideOut);
                    });
                }
            };

            DataAccess cached = CachedDataAccess.createUnsafe(underlying);

            cached.findMoviesAboutLocation(LONDON.id(), 2).unsafeRun();
            cached.findMoviesAboutLocation(LONDON.id(), 2).unsafeRun();

            assertThat(callCount.get()).isEqualTo(1);
        }
    }
}
