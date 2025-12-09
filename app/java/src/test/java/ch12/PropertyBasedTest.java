package ch12;

import ch11.TravelGuide.*;
import io.vavr.collection.List;
import io.vavr.control.Option;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;

import java.util.Random;

import static ch11.TravelGuide.guideScore;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第12章: プロパティベーステスト
 *
 * ランダムな入力で不変条件を検証するテストです。
 */
@DisplayName("第12章: プロパティベーステスト")
class PropertyBasedTest {

    private static final Random random = new Random();

    // ============================================
    // ジェネレータ
    // ============================================

    static int nonNegativeInt() {
        return random.nextInt(Integer.MAX_VALUE);
    }

    static int nonNegativeInt(int bound) {
        return random.nextInt(bound);
    }

    static String randomString() {
        int length = random.nextInt(20) + 1;
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < length; i++) {
            sb.append((char) ('a' + random.nextInt(26)));
        }
        return sb.toString();
    }

    static LocationId randomLocationId() {
        return new LocationId("Q" + nonNegativeInt(1000000));
    }

    static Location randomLocation() {
        return new Location(
                randomLocationId(),
                randomString(),
                nonNegativeInt(10_000_000)
        );
    }

    static Attraction randomAttraction(boolean withDescription) {
        return new Attraction(
                randomString(),
                withDescription ? Option.some(randomString()) : Option.none(),
                randomLocation()
        );
    }

    static Artist randomArtist() {
        return new Artist(randomString(), nonNegativeInt(10_000_000));
    }

    static Movie randomMovie() {
        return new Movie(randomString(), nonNegativeInt(1_000_000_000));
    }

    static List<Artist> randomArtists(int maxCount) {
        int count = random.nextInt(maxCount + 1);
        return List.range(0, count).map(i -> randomArtist());
    }

    static List<Movie> randomMovies(int maxCount) {
        int count = random.nextInt(maxCount + 1);
        return List.range(0, count).map(i -> randomMovie());
    }

    static List<PopCultureSubject> randomSubjects(int maxCount) {
        List<Artist> artists = randomArtists(maxCount / 2);
        List<Movie> movies = randomMovies(maxCount / 2);
        return List.<PopCultureSubject>empty().appendAll(artists).appendAll(movies);
    }

    static Guide randomGuide(boolean withDescription, int maxSubjects) {
        return new Guide(
                randomAttraction(withDescription),
                randomSubjects(maxSubjects)
        );
    }

    // ============================================
    // guideScore のプロパティテスト
    // ============================================

    @Nested
    @DisplayName("guideScore のプロパティ")
    class GuideScorePropertyTest {

        @RepeatedTest(100)
        @DisplayName("スコアは常に 0 以上 100 以下")
        void scoreIsBetween0And100() {
            Guide guide = randomGuide(random.nextBoolean(), 10);

            int score = guideScore(guide);

            assertThat(score).isBetween(0, 100);
        }

        @RepeatedTest(100)
        @DisplayName("説明ありのスコアは説明なしより 30 点高い")
        void descriptionAdds30Points() {
            Location location = randomLocation();
            List<PopCultureSubject> subjects = randomSubjects(4);

            Attraction withDesc = new Attraction("Test", Option.some("Description"), location);
            Attraction withoutDesc = new Attraction("Test", Option.none(), location);

            Guide guideWithDesc = new Guide(withDesc, subjects);
            Guide guideWithoutDesc = new Guide(withoutDesc, subjects);

            int scoreDiff = guideScore(guideWithDesc) - guideScore(guideWithoutDesc);

            assertThat(scoreDiff).isEqualTo(30);
        }

        @RepeatedTest(100)
        @DisplayName("スコアはアトラクション名に依存しない")
        void scoreDoesNotDependOnName() {
            Location location = randomLocation();
            Option<String> description = Option.some(randomString());
            List<PopCultureSubject> subjects = randomSubjects(4);

            Attraction attr1 = new Attraction("Name1", description, location);
            Attraction attr2 = new Attraction("Name2", description, location);

            Guide guide1 = new Guide(attr1, subjects);
            Guide guide2 = new Guide(attr2, subjects);

            assertThat(guideScore(guide1)).isEqualTo(guideScore(guide2));
        }

        @RepeatedTest(100)
        @DisplayName("題材なし・説明なしのスコアは 0")
        void noSubjectsNoDescriptionIsZero() {
            Attraction attraction = randomAttraction(false);
            Guide guide = new Guide(attraction, List.empty());

            assertThat(guideScore(guide)).isEqualTo(0);
        }

        @RepeatedTest(100)
        @DisplayName("題材なし・説明ありのスコアは 30")
        void noSubjectsWithDescriptionIs30() {
            Attraction attraction = randomAttraction(true);
            Guide guide = new Guide(attraction, List.empty());

            assertThat(guideScore(guide)).isEqualTo(30);
        }

        @RepeatedTest(100)
        @DisplayName("アーティスト1人（説明なし）のスコアは 10〜25")
        void singleArtistScoreRange() {
            Attraction attraction = randomAttraction(false);
            Artist artist = randomArtist();
            Guide guide = new Guide(attraction, List.of(artist));

            int score = guideScore(guide);

            // 0 (説明なし) + 10 (アーティスト1人) + 0〜15 (フォロワー)
            assertThat(score).isBetween(10, 25);
        }

        @RepeatedTest(100)
        @DisplayName("映画1本（説明なし）のスコアは 10〜25")
        void singleMovieScoreRange() {
            Attraction attraction = randomAttraction(false);
            Movie movie = randomMovie();
            Guide guide = new Guide(attraction, List.of(movie));

            int score = guideScore(guide);

            // 0 (説明なし) + 10 (映画1本) + 0〜15 (興行収入)
            assertThat(score).isBetween(10, 25);
        }

        @Test
        @DisplayName("フォロワー数のオーバーフローを防ぐ")
        void preventsFollowerOverflow() {
            Attraction attraction = randomAttraction(false);
            List<Artist> artists = List.range(0, 100)
                    .map(i -> new Artist("Artist" + i, Integer.MAX_VALUE));
            Guide guide = new Guide(attraction, List.narrow(artists));

            // オーバーフローしないことを確認
            int score = guideScore(guide);

            // スコアは有効な範囲内
            assertThat(score).isBetween(0, 100);
        }

        @Test
        @DisplayName("興行収入のオーバーフローを防ぐ")
        void preventsBoxOfficeOverflow() {
            Attraction attraction = randomAttraction(false);
            List<Movie> movies = List.range(0, 100)
                    .map(i -> new Movie("Movie" + i, Integer.MAX_VALUE));
            Guide guide = new Guide(attraction, List.narrow(movies));

            // オーバーフローしないことを確認
            int score = guideScore(guide);

            // スコアは有効な範囲内
            assertThat(score).isBetween(0, 100);
        }
    }

    // ============================================
    // findBestGuide のプロパティテスト
    // ============================================

    @Nested
    @DisplayName("findBestGuide のプロパティ")
    class FindBestGuidePropertyTest {

        @RepeatedTest(100)
        @DisplayName("結果のスコアはリスト内の最大スコア以上")
        void resultHasMaxScore() {
            List<Guide> guides = List.range(0, random.nextInt(10) + 1)
                    .map(i -> randomGuide(random.nextBoolean(), 5));

            Option<Guide> best = ch11.TravelGuide.findBestGuide(guides);

            if (best.isDefined()) {
                int bestScore = guideScore(best.get());
                int maxScore = guides.map(g -> guideScore(g)).max().getOrElse(0);
                assertThat(bestScore).isGreaterThanOrEqualTo(maxScore);
            }
        }

        @RepeatedTest(100)
        @DisplayName("空リストは None を返す")
        void emptyListReturnsNone() {
            Option<Guide> result = ch11.TravelGuide.findBestGuide(List.empty());
            assertThat(result.isEmpty()).isTrue();
        }

        @RepeatedTest(100)
        @DisplayName("単一要素リストはその要素を返す")
        void singleElementListReturnsThatElement() {
            Guide guide = randomGuide(random.nextBoolean(), 5);
            Option<Guide> result = ch11.TravelGuide.findBestGuide(List.of(guide));

            assertThat(result.isDefined()).isTrue();
            assertThat(result.get()).isEqualTo(guide);
        }
    }

    // ============================================
    // 純粋関数のプロパティ
    // ============================================

    @Nested
    @DisplayName("純粋関数のプロパティ")
    class PureFunctionPropertyTest {

        @RepeatedTest(100)
        @DisplayName("guideScore は同じ入力に対して同じ出力を返す（参照透過性）")
        void guideScoreIsReferentiallyTransparent() {
            Guide guide = randomGuide(random.nextBoolean(), 5);

            int score1 = guideScore(guide);
            int score2 = guideScore(guide);
            int score3 = guideScore(guide);

            assertThat(score1).isEqualTo(score2).isEqualTo(score3);
        }

        @RepeatedTest(100)
        @DisplayName("findBestGuide は同じ入力に対して同じ出力を返す")
        void findBestGuideIsReferentiallyTransparent() {
            List<Guide> guides = List.range(0, random.nextInt(5) + 1)
                    .map(i -> randomGuide(random.nextBoolean(), 3));

            Option<Guide> result1 = ch11.TravelGuide.findBestGuide(guides);
            Option<Guide> result2 = ch11.TravelGuide.findBestGuide(guides);

            assertThat(result1).isEqualTo(result2);
        }
    }
}
