package ch06;

import io.vavr.collection.List;
import io.vavr.control.Option;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * 第6章: TV番組パーサーのテスト
 */
@DisplayName("第6章: TV番組パーサー")
class TvShowParserTest {

    @Nested
    @DisplayName("危険なパーサー（例外あり）")
    class UnsafeParserTest {

        @Test
        @DisplayName("正常ケース")
        void normalCase() {
            TvShow show = TvShowParser.parseShowUnsafe("Breaking Bad (2008-2013)");
            assertThat(show.title()).isEqualTo("Breaking Bad");
            assertThat(show.start()).isEqualTo(2008);
            assertThat(show.end()).isEqualTo(2013);
        }

        @Test
        @DisplayName("異常ケース: ダッシュがない")
        void noDash() {
            assertThatThrownBy(() -> TvShowParser.parseShowUnsafe("Chernobyl (2019)"))
                    .isInstanceOf(StringIndexOutOfBoundsException.class);
        }

        @Test
        @DisplayName("異常ケース: 括弧がない")
        void noBrackets() {
            assertThatThrownBy(() -> TvShowParser.parseShowUnsafe("The Wire 2002-2008"))
                    .isInstanceOf(StringIndexOutOfBoundsException.class);
        }
    }

    @Nested
    @DisplayName("extractName")
    class ExtractNameTest {

        @Test
        @DisplayName("正常な形式から名前を抽出")
        void extractValidName() {
            assertThat(TvShowParser.extractName("Breaking Bad (2008-2013)"))
                    .isEqualTo(Option.some("Breaking Bad"));
        }

        @Test
        @DisplayName("括弧がない場合は None")
        void noBracket() {
            assertThat(TvShowParser.extractName("The Wire 2002-2008")).isEqualTo(Option.none());
        }

        @Test
        @DisplayName("括弧が先頭の場合は None")
        void bracketAtStart() {
            assertThat(TvShowParser.extractName("(2008-2013)")).isEqualTo(Option.none());
        }
    }

    @Nested
    @DisplayName("extractYearStart")
    class ExtractYearStartTest {

        @Test
        @DisplayName("正常な形式から開始年を抽出")
        void extractValidYearStart() {
            assertThat(TvShowParser.extractYearStart("Show (2008-2013)"))
                    .isEqualTo(Option.some(2008));
        }

        @Test
        @DisplayName("ダッシュがない場合は None")
        void noDash() {
            assertThat(TvShowParser.extractYearStart("Show (2019)")).isEqualTo(Option.none());
        }

        @Test
        @DisplayName("数値でない場合は None")
        void notNumber() {
            assertThat(TvShowParser.extractYearStart("Show (abc-2013)")).isEqualTo(Option.none());
        }
    }

    @Nested
    @DisplayName("extractYearEnd")
    class ExtractYearEndTest {

        @Test
        @DisplayName("正常な形式から終了年を抽出")
        void extractValidYearEnd() {
            assertThat(TvShowParser.extractYearEnd("Show (2008-2013)"))
                    .isEqualTo(Option.some(2013));
        }

        @Test
        @DisplayName("ダッシュがない場合は None")
        void noDash() {
            assertThat(TvShowParser.extractYearEnd("Show (2019)")).isEqualTo(Option.none());
        }
    }

    @Nested
    @DisplayName("extractSingleYear")
    class ExtractSingleYearTest {

        @Test
        @DisplayName("単年形式から年を抽出")
        void extractSingleYear() {
            assertThat(TvShowParser.extractSingleYear("Chernobyl (2019)"))
                    .isEqualTo(Option.some(2019));
        }

        @Test
        @DisplayName("ダッシュがある場合は None")
        void hasDash() {
            assertThat(TvShowParser.extractSingleYear("Show (2008-2013)")).isEqualTo(Option.none());
        }
    }

    @Nested
    @DisplayName("parseShow")
    class ParseShowTest {

        @Test
        @DisplayName("範囲形式のパース")
        void parseRangeFormat() {
            Option<TvShow> result = TvShowParser.parseShow("Breaking Bad (2008-2013)");
            assertThat(result.isDefined()).isTrue();
            assertThat(result.get().title()).isEqualTo("Breaking Bad");
            assertThat(result.get().start()).isEqualTo(2008);
            assertThat(result.get().end()).isEqualTo(2013);
        }

        @Test
        @DisplayName("単年形式のパース")
        void parseSingleYearFormat() {
            Option<TvShow> result = TvShowParser.parseShow("Chernobyl (2019)");
            assertThat(result.isDefined()).isTrue();
            assertThat(result.get().title()).isEqualTo("Chernobyl");
            assertThat(result.get().start()).isEqualTo(2019);
            assertThat(result.get().end()).isEqualTo(2019);
        }

        @Test
        @DisplayName("不正な形式は None")
        void invalidFormat() {
            assertThat(TvShowParser.parseShow("The Wire 2002-2008")).isEqualTo(Option.none());
            assertThat(TvShowParser.parseShow("Invalid")).isEqualTo(Option.none());
            assertThat(TvShowParser.parseShow("(2008-2013)")).isEqualTo(Option.none());
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
                    "Mad Men (2007-2015)",
                    "Another Invalid"
            );

            List<TvShow> result = TvShowParser.parseShowsBestEffort(rawShows);

            assertThat(result).hasSize(2);
            assertThat(result.get(0).title()).isEqualTo("Breaking Bad");
            assertThat(result.get(1).title()).isEqualTo("Mad Men");
        }

        @Test
        @DisplayName("全て無効なら空リスト")
        void allInvalid() {
            List<String> rawShows = List.of("Invalid 1", "Invalid 2");
            assertThat(TvShowParser.parseShowsBestEffort(rawShows)).isEmpty();
        }
    }

    @Nested
    @DisplayName("All-or-nothing 戦略")
    class AllOrNothingTest {

        @Test
        @DisplayName("全て有効なら Some")
        void allValid() {
            List<String> rawShows = List.of(
                    "Breaking Bad (2008-2013)",
                    "Mad Men (2007-2015)"
            );

            Option<List<TvShow>> result = TvShowParser.parseShowsAllOrNothing(rawShows);

            assertThat(result.isDefined()).isTrue();
            assertThat(result.get()).hasSize(2);
        }

        @Test
        @DisplayName("一つでも無効なら None")
        void oneInvalid() {
            List<String> rawShows = List.of(
                    "Breaking Bad (2008-2013)",
                    "Invalid Show"
            );

            assertThat(TvShowParser.parseShowsAllOrNothing(rawShows)).isEqualTo(Option.none());
        }

        @Test
        @DisplayName("空リストは Some(空リスト)")
        void emptyList() {
            Option<List<TvShow>> result = TvShowParser.parseShowsAllOrNothing(List.empty());
            assertThat(result.isDefined()).isTrue();
            assertThat(result.get()).isEmpty();
        }
    }
}
