package ch12;

import ch11.DataAccess;
import io.vavr.collection.List;
import io.vavr.control.Option;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static ch11.TravelGuide.*;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * 第12章: TestDataAccess のテスト
 */
@DisplayName("第12章: TestDataAccess")
class TestDataAccessTest {

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
    static final Movie INSIDE_OUT = new Movie("Inside Out", 857_611_174);

    @Nested
    @DisplayName("ビルダーパターン")
    class BuilderTest {

        @Test
        @DisplayName("空の DataAccess を作成")
        void emptyDataAccess() {
            DataAccess dataAccess = TestDataAccess.empty();

            assertThat(dataAccess.findAttractions("test", AttractionOrdering.BY_NAME, 10).unsafeRun())
                    .isEmpty();
            assertThat(dataAccess.findArtistsFromLocation(LONDON.id(), 10).unsafeRun())
                    .isEmpty();
            assertThat(dataAccess.findMoviesAboutLocation(LONDON.id(), 10).unsafeRun())
                    .isEmpty();
        }

        @Test
        @DisplayName("アトラクションを指定して作成")
        void withAttractions() {
            DataAccess dataAccess = TestDataAccess.builder()
                    .withAttractions(TOWER_BRIDGE)
                    .build();

            List<Attraction> result = dataAccess
                    .findAttractions("test", AttractionOrdering.BY_NAME, 10)
                    .unsafeRun();

            assertThat(result).containsExactly(TOWER_BRIDGE);
        }

        @Test
        @DisplayName("アーティストを指定して作成")
        void withArtists() {
            DataAccess dataAccess = TestDataAccess.builder()
                    .withArtists(QUEEN)
                    .build();

            List<Artist> result = dataAccess
                    .findArtistsFromLocation(LONDON.id(), 10)
                    .unsafeRun();

            assertThat(result).containsExactly(QUEEN);
        }

        @Test
        @DisplayName("映画を指定して作成")
        void withMovies() {
            DataAccess dataAccess = TestDataAccess.builder()
                    .withMovies(INSIDE_OUT)
                    .build();

            List<Movie> result = dataAccess
                    .findMoviesAboutLocation(LONDON.id(), 10)
                    .unsafeRun();

            assertThat(result).containsExactly(INSIDE_OUT);
        }

        @Test
        @DisplayName("すべてのデータを指定して作成")
        void withAllData() {
            DataAccess dataAccess = TestDataAccess.of(
                    List.of(TOWER_BRIDGE),
                    List.of(QUEEN),
                    List.of(INSIDE_OUT)
            );

            assertThat(dataAccess.findAttractions("test", AttractionOrdering.BY_NAME, 10).unsafeRun())
                    .containsExactly(TOWER_BRIDGE);
            assertThat(dataAccess.findArtistsFromLocation(LONDON.id(), 10).unsafeRun())
                    .containsExactly(QUEEN);
            assertThat(dataAccess.findMoviesAboutLocation(LONDON.id(), 10).unsafeRun())
                    .containsExactly(INSIDE_OUT);
        }
    }

    @Nested
    @DisplayName("エラーシミュレーション")
    class ErrorSimulationTest {

        @Test
        @DisplayName("アトラクション取得時にエラーを発生させる")
        void failOnAttractions() {
            DataAccess dataAccess = TestDataAccess.builder()
                    .failOnAttractions("Network error")
                    .build();

            assertThatThrownBy(() ->
                    dataAccess.findAttractions("test", AttractionOrdering.BY_NAME, 10).unsafeRun()
            ).isInstanceOf(RuntimeException.class)
                    .hasMessage("Network error");
        }

        @Test
        @DisplayName("アーティスト取得時にエラーを発生させる")
        void failOnArtists() {
            DataAccess dataAccess = TestDataAccess.builder()
                    .failOnArtists("API timeout")
                    .build();

            assertThatThrownBy(() ->
                    dataAccess.findArtistsFromLocation(LONDON.id(), 10).unsafeRun()
            ).isInstanceOf(RuntimeException.class)
                    .hasMessage("API timeout");
        }

        @Test
        @DisplayName("映画取得時にエラーを発生させる")
        void failOnMovies() {
            DataAccess dataAccess = TestDataAccess.builder()
                    .failOnMovies("Database error")
                    .build();

            assertThatThrownBy(() ->
                    dataAccess.findMoviesAboutLocation(LONDON.id(), 10).unsafeRun()
            ).isInstanceOf(RuntimeException.class)
                    .hasMessage("Database error");
        }
    }

    @Nested
    @DisplayName("limit の適用")
    class LimitTest {

        @Test
        @DisplayName("limit がリスト長より小さい場合は制限される")
        void limitSmaller() {
            DataAccess dataAccess = TestDataAccess.builder()
                    .withAttractions(
                            new Attraction("A1", Option.none(), LONDON),
                            new Attraction("A2", Option.none(), LONDON),
                            new Attraction("A3", Option.none(), LONDON)
                    )
                    .build();

            List<Attraction> result = dataAccess
                    .findAttractions("test", AttractionOrdering.BY_NAME, 2)
                    .unsafeRun();

            assertThat(result).hasSize(2);
        }

        @Test
        @DisplayName("limit がリスト長より大きい場合はすべて返す")
        void limitLarger() {
            DataAccess dataAccess = TestDataAccess.builder()
                    .withAttractions(TOWER_BRIDGE)
                    .build();

            List<Attraction> result = dataAccess
                    .findAttractions("test", AttractionOrdering.BY_NAME, 10)
                    .unsafeRun();

            assertThat(result).hasSize(1);
        }
    }

    @Nested
    @DisplayName("ファクトリメソッド")
    class FactoryMethodTest {

        @Test
        @DisplayName("withAttractions で簡単に作成")
        void withAttractionsFactory() {
            DataAccess dataAccess = TestDataAccess.withAttractions(TOWER_BRIDGE);

            List<Attraction> result = dataAccess
                    .findAttractions("test", AttractionOrdering.BY_NAME, 10)
                    .unsafeRun();

            assertThat(result).containsExactly(TOWER_BRIDGE);
        }
    }
}
