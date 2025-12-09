package ch07;

import ch06.TvShow;
import io.vavr.collection.List;
import io.vavr.control.Either;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第7章: Either を使った TV番組パーサーのテスト
 */
@DisplayName("第7章: Either を使った TV番組パーサー")
class TvShowParserEitherTest {

    @Nested
    @DisplayName("extractName")
    class ExtractNameTest {

        @Test
        @DisplayName("正常な形式から名前を抽出")
        void extractValidName() {
            Either<String, String> result = TvShowParserEither.extractName("Breaking Bad (2008-2013)");
            assertThat(result.isRight()).isTrue();
            assertThat(result.get()).isEqualTo("Breaking Bad");
        }

        @Test
        @DisplayName("括弧がない場合はエラーメッセージ")
        void noBracket() {
            Either<String, String> result = TvShowParserEither.extractName("No Bracket Show");
            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft()).contains("Can't extract name");
        }
    }

    @Nested
    @DisplayName("extractYearStart")
    class ExtractYearStartTest {

        @Test
        @DisplayName("正常な形式から開始年を抽出")
        void extractValidYearStart() {
            Either<String, Integer> result = TvShowParserEither.extractYearStart("Show (2008-2013)");
            assertThat(result.isRight()).isTrue();
            assertThat(result.get()).isEqualTo(2008);
        }

        @Test
        @DisplayName("数値でない場合はエラーメッセージ")
        void notNumber() {
            Either<String, Integer> result = TvShowParserEither.extractYearStart("Show (abc-2013)");
            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft()).contains("Can't parse");
        }
    }

    @Nested
    @DisplayName("parseShow")
    class ParseShowTest {

        @Test
        @DisplayName("範囲形式のパース")
        void parseRangeFormat() {
            Either<String, TvShow> result = TvShowParserEither.parseShow("Breaking Bad (2008-2013)");
            assertThat(result.isRight()).isTrue();
            assertThat(result.get().title()).isEqualTo("Breaking Bad");
            assertThat(result.get().start()).isEqualTo(2008);
            assertThat(result.get().end()).isEqualTo(2013);
        }

        @Test
        @DisplayName("単年形式のパース")
        void parseSingleYearFormat() {
            Either<String, TvShow> result = TvShowParserEither.parseShow("Chernobyl (2019)");
            assertThat(result.isRight()).isTrue();
            assertThat(result.get().title()).isEqualTo("Chernobyl");
            assertThat(result.get().start()).isEqualTo(2019);
            assertThat(result.get().end()).isEqualTo(2019);
        }

        @Test
        @DisplayName("名前抽出エラー")
        void nameError() {
            Either<String, TvShow> result = TvShowParserEither.parseShow("(2008-2013)");
            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft()).contains("Can't extract name");
        }

        @Test
        @DisplayName("年パースエラー")
        void yearError() {
            Either<String, TvShow> result = TvShowParserEither.parseShow("Show (abc-2013)");
            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft()).contains("Can't");
        }
    }

    @Nested
    @DisplayName("Best-effort 戦略")
    class BestEffortTest {

        @Test
        @DisplayName("パースできたものだけ返す")
        void parseOnlyValid() {
            List<String> rawShows = List.of(
                    "Breaking Bad (2008-2013)",
                    "Invalid Show",
                    "Mad Men (2007-2015)"
            );

            List<TvShow> result = TvShowParserEither.parseShowsBestEffort(rawShows);

            assertThat(result).hasSize(2);
        }
    }

    @Nested
    @DisplayName("All-or-nothing 戦略")
    class AllOrNothingTest {

        @Test
        @DisplayName("全て有効なら Right")
        void allValid() {
            List<String> rawShows = List.of(
                    "Breaking Bad (2008-2013)",
                    "Mad Men (2007-2015)"
            );

            Either<String, List<TvShow>> result = TvShowParserEither.parseShowsAllOrNothing(rawShows);

            assertThat(result.isRight()).isTrue();
            assertThat(result.get()).hasSize(2);
        }

        @Test
        @DisplayName("一つでも無効なら Left（最初のエラー）")
        void oneInvalid() {
            List<String> rawShows = List.of(
                    "Breaking Bad (2008-2013)",
                    "Invalid Show"
            );

            Either<String, List<TvShow>> result = TvShowParserEither.parseShowsAllOrNothing(rawShows);

            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft()).contains("Can't");
        }
    }

    @Nested
    @DisplayName("エラー収集戦略")
    class CollectErrorsTest {

        @Test
        @DisplayName("全て有効なら Right")
        void allValid() {
            List<String> rawShows = List.of(
                    "Breaking Bad (2008-2013)",
                    "Mad Men (2007-2015)"
            );

            Either<List<String>, List<TvShow>> result =
                    TvShowParserEither.parseShowsCollectErrors(rawShows);

            assertThat(result.isRight()).isTrue();
            assertThat(result.get()).hasSize(2);
        }

        @Test
        @DisplayName("一部無効なら全てのエラーを収集")
        void someInvalid() {
            List<String> rawShows = List.of(
                    "Breaking Bad (2008-2013)",
                    "Invalid 1",
                    "Invalid 2"
            );

            Either<List<String>, List<TvShow>> result =
                    TvShowParserEither.parseShowsCollectErrors(rawShows);

            assertThat(result.isLeft()).isTrue();
            assertThat(result.getLeft()).hasSize(2);
        }
    }
}
