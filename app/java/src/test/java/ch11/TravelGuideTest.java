package ch11;

import ch08.IO;
import ch12.TestDataAccess;
import io.vavr.collection.List;
import io.vavr.control.Either;
import io.vavr.control.Option;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static ch11.TravelGuide.*;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第11章: TravelGuide のテスト
 */
@DisplayName("第11章: TravelGuide")
class TravelGuideTest {

    // ============================================
    // テストデータ
    // ============================================

    static final Location WYOMING = new Location(
            new LocationId("Q1214"),
            "Wyoming",
            586107
    );

    static final Location LONDON = new Location(
            new LocationId("Q84"),
            "London",
            8_908_081
    );

    static final Location SAN_FRANCISCO = new Location(
            new LocationId("Q62"),
            "San Francisco",
            883_963
    );

    static final Attraction YELLOWSTONE = new Attraction(
            "Yellowstone National Park",
            Option.some("first national park in the world"),
            WYOMING
    );

    static final Attraction TOWER_BRIDGE = new Attraction(
            "Tower Bridge",
            Option.none(),
            LONDON
    );

    static final Attraction GOLDEN_GATE = new Attraction(
            "Golden Gate Bridge",
            Option.some("famous suspension bridge"),
            SAN_FRANCISCO
    );

    static final Artist QUEEN = new Artist("Queen", 2_050_559);
    static final Artist CHRIS_LEDOUX = new Artist("Chris LeDoux", 100_000);

    static final Movie HATEFUL_EIGHT = new Movie("The Hateful Eight", 155_760_117);
    static final Movie HEAVENS_GATE = new Movie("Heaven's Gate", 3_484_331);
    static final Movie INSIDE_OUT = new Movie("Inside Out", 857_611_174);

    // ============================================
    // guideScore テスト
    // ============================================

    @Nested
    @DisplayName("guideScore")
    class GuideScoreTest {

        @Test
        @DisplayName("説明あり、映画2本のスコアは65")
        void scoreWithDescriptionAndTwoMovies() {
            Guide guide = new Guide(YELLOWSTONE, List.of(HATEFUL_EIGHT, HEAVENS_GATE));

            // 30 (説明) + 0 (アーティストなし) + 20 (映画2本) + 15 (興行収入)
            assertThat(guideScore(guide)).isEqualTo(65);
        }

        @Test
        @DisplayName("説明なし、ポップカルチャーなしのスコアは0")
        void scoreWithNothingShouldBeZero() {
            Attraction noDescription = new Attraction(
                    "Test",
                    Option.none(),
                    WYOMING
            );
            Guide guide = new Guide(noDescription, List.empty());

            assertThat(guideScore(guide)).isEqualTo(0);
        }

        @Test
        @DisplayName("説明なし、映画2本（興行収入0）のスコアは20")
        void scoreWithTwoMoviesNoBoxOffice() {
            Attraction noDescription = new Attraction(
                    "Test",
                    Option.none(),
                    WYOMING
            );
            Guide guide = new Guide(noDescription, List.of(
                    new Movie("Movie 1", 0),
                    new Movie("Movie 2", 0)
            ));

            // 0 (説明なし) + 0 (アーティストなし) + 20 (映画2本) + 0 (興行収入0)
            assertThat(guideScore(guide)).isEqualTo(20);
        }

        @Test
        @DisplayName("アーティスト1人のスコアは10〜25の範囲")
        void scoreWithSingleArtist() {
            Attraction noDescription = new Attraction(
                    "Test",
                    Option.none(),
                    WYOMING
            );
            Guide guide = new Guide(noDescription, List.of(new Artist("Test", 500_000)));

            int score = guideScore(guide);
            // 0 (説明なし) + 10 (アーティスト1人) + 0〜15 (フォロワー)
            assertThat(score).isBetween(10, 25);
        }

        @Test
        @DisplayName("スコアの最大値は100")
        void maxScoreIs100() {
            Guide guide = new Guide(YELLOWSTONE, List.of(
                    new Artist("A1", 1_000_000),
                    new Artist("A2", 1_000_000),
                    new Movie("M1", 100_000_000),
                    new Movie("M2", 100_000_000)
            ));

            int score = guideScore(guide);
            // 30 (説明) + 40 (4件で上限) + 15 (フォロワー上限) + 15 (興行収入上限)
            assertThat(score).isEqualTo(100);
        }
    }

    // ============================================
    // travelGuideV1 テスト
    // ============================================

    @Nested
    @DisplayName("travelGuideV1")
    class TravelGuideV1Test {

        @Test
        @DisplayName("アトラクションが見つからない場合は None")
        void noAttractionReturnsNone() {
            DataAccess dataAccess = TestDataAccess.empty();

            Option<Guide> result = travelGuideV1(dataAccess, "Unknown").unsafeRun();

            assertThat(result.isEmpty()).isTrue();
        }

        @Test
        @DisplayName("アトラクションが見つかればガイドを返す")
        void foundAttractionReturnsGuide() {
            DataAccess dataAccess = TestDataAccess.builder()
                    .withAttractions(TOWER_BRIDGE)
                    .withArtists(QUEEN)
                    .build();

            Option<Guide> result = travelGuideV1(dataAccess, "Tower Bridge").unsafeRun();

            assertThat(result.isDefined()).isTrue();
            assertThat(result.get().attraction()).isEqualTo(TOWER_BRIDGE);
            assertThat(result.get().subjects()).containsExactly(QUEEN);
        }

        @Test
        @DisplayName("映画を含むガイドを返す")
        void guideIncludesMovies() {
            DataAccess dataAccess = TestDataAccess.builder()
                    .withAttractions(GOLDEN_GATE)
                    .withMovies(INSIDE_OUT)
                    .build();

            Option<Guide> result = travelGuideV1(dataAccess, "Golden Gate").unsafeRun();

            assertThat(result.isDefined()).isTrue();
            assertThat(result.get().subjects()).containsExactly(INSIDE_OUT);
        }
    }

    // ============================================
    // travelGuideV2 テスト
    // ============================================

    @Nested
    @DisplayName("travelGuideV2")
    class TravelGuideV2Test {

        @Test
        @DisplayName("最もスコアの高いガイドを返す")
        void returnsBestScoringGuide() {
            // 複数のアトラクションを持つ DataAccess
            DataAccess dataAccess = new DataAccess() {
                @Override
                public IO<List<Attraction>> findAttractions(String name, AttractionOrdering ordering, int limit) {
                    return IO.pure(List.of(TOWER_BRIDGE, YELLOWSTONE));
                }

                @Override
                public IO<List<Artist>> findArtistsFromLocation(LocationId locationId, int limit) {
                    // WYOMING にはアーティストがいる
                    if (locationId.equals(WYOMING.id())) {
                        return IO.pure(List.of(CHRIS_LEDOUX));
                    }
                    return IO.pure(List.empty());
                }

                @Override
                public IO<List<Movie>> findMoviesAboutLocation(LocationId locationId, int limit) {
                    // WYOMING には映画がある
                    if (locationId.equals(WYOMING.id())) {
                        return IO.pure(List.of(HATEFUL_EIGHT, HEAVENS_GATE));
                    }
                    return IO.pure(List.empty());
                }
            };

            Option<Guide> result = travelGuideV2(dataAccess, "Test").unsafeRun();

            assertThat(result.isDefined()).isTrue();
            // YELLOWSTONE の方がスコアが高い（説明あり + 映画あり）
            assertThat(result.get().attraction()).isEqualTo(YELLOWSTONE);
        }
    }

    // ============================================
    // travelGuideV3 テスト（エラーハンドリング）
    // ============================================

    @Nested
    @DisplayName("travelGuideV3")
    class TravelGuideV3Test {

        @Test
        @DisplayName("アトラクション取得失敗時は SearchReport を返す")
        void failureOnAttractionsFetchReturnsSearchReport() {
            DataAccess dataAccess = TestDataAccess.builder()
                    .failOnAttractions("Network error")
                    .build();

            Either<SearchReport, Guide> result =
                    travelGuideV3(dataAccess, "Test").unsafeRun();

            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft().problems()).contains("Network error");
        }

        @Test
        @DisplayName("良いガイドが見つかれば Right を返す")
        void goodGuideReturnsRight() {
            DataAccess dataAccess = TestDataAccess.builder()
                    .withAttractions(YELLOWSTONE)
                    .withMovies(HATEFUL_EIGHT, HEAVENS_GATE)
                    .build();

            Either<SearchReport, Guide> result =
                    travelGuideV3(dataAccess, "Yellowstone").unsafeRun();

            assertThat(result.isRight()).isTrue();
            assertThat(result.get().attraction()).isEqualTo(YELLOWSTONE);
        }

        @Test
        @DisplayName("スコアが低い場合は SearchReport を返す")
        void lowScoreReturnsSearchReport() {
            DataAccess dataAccess = TestDataAccess.builder()
                    .withAttractions(TOWER_BRIDGE) // 説明なし
                    .build(); // アーティスト、映画なし

            Either<SearchReport, Guide> result =
                    travelGuideV3(dataAccess, "Tower Bridge").unsafeRun();

            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft().badGuides()).hasSize(1);
        }

        @Test
        @DisplayName("一部のガイド取得が失敗しても他のガイドで処理を続行")
        void partialFailureContinuesWithOtherGuides() {
            // LONDON のアーティスト取得は失敗するが、WYOMING は成功する
            DataAccess dataAccess = new DataAccess() {
                @Override
                public IO<List<Attraction>> findAttractions(String name, AttractionOrdering ordering, int limit) {
                    return IO.pure(List.of(TOWER_BRIDGE, YELLOWSTONE));
                }

                @Override
                public IO<List<Artist>> findArtistsFromLocation(LocationId locationId, int limit) {
                    if (locationId.equals(LONDON.id())) {
                        return IO.delay(() -> {
                            throw new RuntimeException("London artists fetch failed");
                        });
                    }
                    return IO.pure(List.of(CHRIS_LEDOUX));
                }

                @Override
                public IO<List<Movie>> findMoviesAboutLocation(LocationId locationId, int limit) {
                    if (locationId.equals(WYOMING.id())) {
                        return IO.pure(List.of(HATEFUL_EIGHT, HEAVENS_GATE));
                    }
                    return IO.pure(List.empty());
                }
            };

            Either<SearchReport, Guide> result =
                    travelGuideV3(dataAccess, "Test").unsafeRun();

            // YELLOWSTONE のガイドは成功し、スコアが高いので Right を返す
            assertThat(result.isRight()).isTrue();
            assertThat(result.get().attraction()).isEqualTo(YELLOWSTONE);
        }
    }

    // ============================================
    // findBestGuide テスト
    // ============================================

    @Nested
    @DisplayName("findBestGuide")
    class FindBestGuideTest {

        @Test
        @DisplayName("空のリストは None を返す")
        void emptyListReturnsNone() {
            Option<Guide> result = findBestGuide(List.empty());
            assertThat(result.isEmpty()).isTrue();
        }

        @Test
        @DisplayName("最もスコアの高いガイドを返す")
        void returnsHighestScoringGuide() {
            Guide lowScore = new Guide(TOWER_BRIDGE, List.empty()); // スコア 0
            Guide highScore = new Guide(YELLOWSTONE, List.of(HATEFUL_EIGHT, HEAVENS_GATE)); // スコア 65

            Option<Guide> result = findBestGuide(List.of(lowScore, highScore));

            assertThat(result.isDefined()).isTrue();
            assertThat(result.get()).isEqualTo(highScore);
        }
    }

    // ============================================
    // findGoodGuide テスト
    // ============================================

    @Nested
    @DisplayName("findGoodGuide")
    class FindGoodGuideTest {

        @Test
        @DisplayName("スコアが閾値を超えれば Right を返す")
        void aboveThresholdReturnsRight() {
            Guide guide = new Guide(YELLOWSTONE, List.of(HATEFUL_EIGHT, HEAVENS_GATE));

            Either<SearchReport, Guide> result = findGoodGuide(List.of(guide), 50);

            assertThat(result.isRight()).isTrue();
        }

        @Test
        @DisplayName("スコアが閾値以下なら Left を返す")
        void belowThresholdReturnsLeft() {
            Guide guide = new Guide(TOWER_BRIDGE, List.empty());

            Either<SearchReport, Guide> result = findGoodGuide(List.of(guide), 50);

            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft().badGuides()).contains(guide);
        }
    }
}
