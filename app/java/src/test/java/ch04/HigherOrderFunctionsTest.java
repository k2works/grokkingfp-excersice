package ch04;

import io.vavr.collection.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第4章: 高階関数のテスト
 */
@DisplayName("第4章: 高階関数")
class HigherOrderFunctionsTest {

    @Nested
    @DisplayName("map")
    class MapTest {

        @Test
        @DisplayName("文字列の長さを取得")
        void lengthsOfStrings() {
            List<String> words = List.of("scala", "rust", "ada");
            assertThat(HigherOrderFunctions.lengths(words)).containsExactly(5, 4, 3);
        }

        @Test
        @DisplayName("空リストは空リストを返す")
        void lengthsOfEmptyList() {
            assertThat(HigherOrderFunctions.lengths(List.empty())).isEmpty();
        }

        @Test
        @DisplayName("整数を2倍にする")
        void doubleValues() {
            List<Integer> numbers = List.of(5, 1, 2, 4, 0);
            assertThat(HigherOrderFunctions.doubles(numbers)).containsExactly(10, 2, 4, 8, 0);
        }
    }

    @Nested
    @DisplayName("filter")
    class FilterTest {

        @Test
        @DisplayName("奇数のみを抽出")
        void filterOdds() {
            List<Integer> numbers = List.of(5, 1, 2, 4, 0);
            assertThat(HigherOrderFunctions.filterOdds(numbers)).containsExactly(5, 1);
        }

        @Test
        @DisplayName("4より大きい数を抽出")
        void filterLargerThan4() {
            List<Integer> numbers = List.of(5, 1, 2, 4, 0);
            assertThat(HigherOrderFunctions.filterLargerThan4(numbers)).containsExactly(5);
        }
    }

    @Nested
    @DisplayName("関数を返す関数")
    class FunctionReturningFunctionTest {

        @Test
        @DisplayName("largerThan: 指定値より大きい数を抽出")
        void largerThan() {
            List<Integer> numbers = List.of(5, 1, 2, 4, 0);

            assertThat(numbers.filter(HigherOrderFunctions.largerThan(4))).containsExactly(5);
            assertThat(numbers.filter(HigherOrderFunctions.largerThan(1))).containsExactly(5, 2, 4);
            assertThat(numbers.filter(HigherOrderFunctions.largerThan(0))).containsExactly(5, 1, 2, 4);
        }

        @Test
        @DisplayName("divisibleBy: 指定値で割り切れる数を抽出")
        void divisibleBy() {
            List<Integer> numbers = List.of(6, 3, 4, 9, 10);

            assertThat(numbers.filter(HigherOrderFunctions.divisibleBy(2))).containsExactly(6, 4, 10);
            assertThat(numbers.filter(HigherOrderFunctions.divisibleBy(3))).containsExactly(6, 3, 9);
        }

        @Test
        @DisplayName("contains: 指定文字列を含む要素を抽出")
        void contains() {
            List<String> words = List.of("scala", "java", "rust", "haskell");

            assertThat(words.filter(HigherOrderFunctions.contains("a")))
                    .containsExactly("scala", "java", "haskell");
            assertThat(words.filter(HigherOrderFunctions.contains("st")))
                    .containsExactly("rust");
        }
    }

    @Nested
    @DisplayName("foldLeft")
    class FoldLeftTest {

        @Test
        @DisplayName("合計を計算")
        void sum() {
            List<Integer> numbers = List.of(5, 1, 2, 4, 100);
            assertThat(HigherOrderFunctions.sum(numbers)).isEqualTo(112);
        }

        @Test
        @DisplayName("空リストの合計は0")
        void sumEmpty() {
            assertThat(HigherOrderFunctions.sum(List.empty())).isEqualTo(0);
        }

        @Test
        @DisplayName("最大値を取得")
        void max() {
            List<Integer> numbers = List.of(5, 1, 2, 4, 15);
            assertThat(HigherOrderFunctions.max(numbers)).isEqualTo(15);
        }

        @Test
        @DisplayName("文字列を連結")
        void concatenate() {
            List<String> strings = List.of("a", "b", "c");
            assertThat(HigherOrderFunctions.concatenate(strings)).isEqualTo("abc");
        }

        @Test
        @DisplayName("区切り文字で連結")
        void join() {
            List<String> strings = List.of("a", "b", "c");
            assertThat(HigherOrderFunctions.join(strings, ", ")).isEqualTo("a, b, c");
        }

        @Test
        @DisplayName("空リストの join は空文字")
        void joinEmpty() {
            assertThat(HigherOrderFunctions.join(List.empty(), ", ")).isEqualTo("");
        }
    }

    @Nested
    @DisplayName("sortBy")
    class SortByTest {

        @Test
        @DisplayName("文字列を長さでソート")
        void sortByLength() {
            List<String> words = List.of("scala", "rust", "ada");
            assertThat(HigherOrderFunctions.sortByFunction(words, String::length))
                    .containsExactly("ada", "rust", "scala");
        }

        @Test
        @DisplayName("数値をそのままソート")
        void sortByValue() {
            List<Integer> numbers = List.of(5, 1, 2, 4, 0);
            assertThat(HigherOrderFunctions.sortByFunction(numbers, i -> i))
                    .containsExactly(0, 1, 2, 4, 5);
        }
    }
}
