package ch04;

import io.vavr.collection.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;

/**
 * 第4章: プログラミング言語のテスト
 */
@DisplayName("第4章: プログラミング言語")
class ProgrammingLanguageTest {

    private final List<ProgrammingLanguage> languages = List.of(
            new ProgrammingLanguage("Java", 1995),
            new ProgrammingLanguage("Scala", 2004),
            new ProgrammingLanguage("Kotlin", 2011),
            new ProgrammingLanguage("Rust", 2010),
            new ProgrammingLanguage("Go", 2009)
    );

    @Nested
    @DisplayName("ソート")
    class SortingTest {

        @Test
        @DisplayName("名前でソート")
        void sortByName() {
            List<ProgrammingLanguage> sorted = ProgrammingLanguage.sortByName(languages);
            assertThat(sorted.map(ProgrammingLanguage::name))
                    .containsExactly("Go", "Java", "Kotlin", "Rust", "Scala");
        }

        @Test
        @DisplayName("年でソート")
        void sortByYear() {
            List<ProgrammingLanguage> sorted = ProgrammingLanguage.sortByYear(languages);
            assertThat(sorted.map(ProgrammingLanguage::name))
                    .containsExactly("Java", "Scala", "Go", "Rust", "Kotlin");
        }
    }

    @Nested
    @DisplayName("フィルタ")
    class FilterTest {

        @Test
        @DisplayName("指定年以降の言語をフィルタ")
        void filterByYearAfter() {
            List<ProgrammingLanguage> recent = ProgrammingLanguage.filterByYearAfter(languages, 2005);
            assertThat(recent.map(ProgrammingLanguage::name))
                    .containsExactlyInAnyOrder("Go", "Rust", "Kotlin");
        }

        @Test
        @DisplayName("該当なしの場合は空リスト")
        void filterNoMatch() {
            List<ProgrammingLanguage> future = ProgrammingLanguage.filterByYearAfter(languages, 2020);
            assertThat(future).isEmpty();
        }
    }

    @Nested
    @DisplayName("抽出")
    class ExtractionTest {

        @Test
        @DisplayName("言語名のリストを取得")
        void getNames() {
            List<String> names = ProgrammingLanguage.getNames(languages);
            assertThat(names).containsExactly("Java", "Scala", "Kotlin", "Rust", "Go");
        }
    }

    @Nested
    @DisplayName("集計")
    class AggregationTest {

        @Test
        @DisplayName("平均年を計算")
        void averageYear() {
            double avg = ProgrammingLanguage.averageYear(languages);
            // (1995 + 2004 + 2011 + 2010 + 2009) / 5 = 10029 / 5 = 2005.8
            assertThat(avg).isCloseTo(2005.8, within(0.1));
        }

        @Test
        @DisplayName("空リストの平均は0")
        void averageYearEmpty() {
            assertThat(ProgrammingLanguage.averageYear(List.empty())).isEqualTo(0.0);
        }
    }
}
