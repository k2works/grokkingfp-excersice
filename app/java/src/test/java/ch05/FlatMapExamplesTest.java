package ch05;

import io.vavr.collection.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第5章: flatMap のテスト
 */
@DisplayName("第5章: flatMap")
class FlatMapExamplesTest {

    @Nested
    @DisplayName("flatten")
    class FlattenTest {

        @Test
        @DisplayName("ネストしたリストを平坦化")
        void flattenNestedList() {
            List<List<Integer>> nested = List.of(
                    List.of(1, 2),
                    List.of(3, 4, 5),
                    List.of(6)
            );
            assertThat(FlatMapExamples.flatten(nested)).containsExactly(1, 2, 3, 4, 5, 6);
        }

        @Test
        @DisplayName("空リストを含む場合")
        void flattenWithEmptyLists() {
            List<List<Integer>> nested = List.of(
                    List.of(1),
                    List.empty(),
                    List.of(2, 3)
            );
            assertThat(FlatMapExamples.flatten(nested)).containsExactly(1, 2, 3);
        }

        @Test
        @DisplayName("全て空リストの場合")
        void flattenAllEmpty() {
            List<List<Integer>> nested = List.of(List.empty(), List.empty());
            assertThat(FlatMapExamples.flatten(nested)).isEmpty();
        }
    }

    @Nested
    @DisplayName("flatMap による要素数の変化")
    class SizeChangeTest {

        @Test
        @DisplayName("要素数が増える（duplicate）")
        void duplicateIncreasesSize() {
            List<Integer> numbers = List.of(1, 2, 3);
            assertThat(FlatMapExamples.duplicate(numbers))
                    .containsExactly(1, 11, 2, 12, 3, 13);
        }

        @Test
        @DisplayName("要素数が減る（filterEvens）")
        void filterEvensDecreasesSize() {
            List<Integer> numbers = List.of(1, 2, 3, 4, 5);
            assertThat(FlatMapExamples.filterEvens(numbers)).containsExactly(2, 4);
        }

        @Test
        @DisplayName("全て奇数の場合は空リスト")
        void filterEvensAllOdd() {
            List<Integer> numbers = List.of(1, 3, 5);
            assertThat(FlatMapExamples.filterEvens(numbers)).isEmpty();
        }
    }

    @Nested
    @DisplayName("文字への変換")
    class ToCharsTest {

        @Test
        @DisplayName("各単語を文字のリストに変換")
        void convertWordsToChars() {
            List<String> words = List.of("hi", "bye");
            assertThat(FlatMapExamples.toChars(words))
                    .containsExactly('h', 'i', 'b', 'y', 'e');
        }

        @Test
        @DisplayName("空文字列を含む場合")
        void toCharsWithEmptyString() {
            List<String> words = List.of("a", "", "b");
            assertThat(FlatMapExamples.toChars(words)).containsExactly('a', 'b');
        }
    }

    @Nested
    @DisplayName("文字列の分割")
    class SplitAndFlattenTest {

        @Test
        @DisplayName("文字列をスペースで分割して平坦化")
        void splitSentences() {
            List<String> sentences = List.of("hello world", "foo bar baz");
            assertThat(FlatMapExamples.splitAndFlatten(sentences))
                    .containsExactly("hello", "world", "foo", "bar", "baz");
        }
    }

    @Nested
    @DisplayName("範囲の生成")
    class RangesTest {

        @Test
        @DisplayName("数値の範囲を生成して平坦化")
        void generateRanges() {
            List<Integer> ends = List.of(2, 3);
            assertThat(FlatMapExamples.ranges(ends))
                    .containsExactly(1, 2, 1, 2, 3);
        }
    }

    @Nested
    @DisplayName("直積")
    class CartesianProductTest {

        @Test
        @DisplayName("直積を計算")
        void calculateCartesianProduct() {
            List<String> colors = List.of("Red", "Blue");
            List<Integer> sizes = List.of(1, 2);
            assertThat(FlatMapExamples.cartesianProduct(colors, sizes))
                    .containsExactly("(Red, 1)", "(Red, 2)", "(Blue, 1)", "(Blue, 2)");
        }

        @Test
        @DisplayName("片方が空の場合は空リスト")
        void cartesianProductWithEmpty() {
            List<String> colors = List.of("Red");
            List<Integer> sizes = List.empty();
            assertThat(FlatMapExamples.cartesianProduct(colors, sizes)).isEmpty();
        }
    }
}
