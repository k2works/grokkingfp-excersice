package ch03;

import io.vavr.collection.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第3章: イミュータブルなデータ操作のテスト
 */
@DisplayName("第3章: イミュータブルなデータ操作")
class ImmutableListTest {

    @Nested
    @DisplayName("基本操作")
    class BasicOperationsTest {

        @Test
        @DisplayName("firstTwo: 最初の2要素を取得")
        void firstTwoElements() {
            List<String> list = List.of("a", "b", "c", "d");
            assertThat(ImmutableList.firstTwo(list)).containsExactly("a", "b");
        }

        @Test
        @DisplayName("firstTwo: 空リストは空リストを返す")
        void firstTwoEmpty() {
            assertThat(ImmutableList.firstTwo(List.empty())).isEmpty();
        }

        @Test
        @DisplayName("firstTwo: 1要素のリストは1要素を返す")
        void firstTwoSingleElement() {
            assertThat(ImmutableList.firstTwo(List.of("a"))).containsExactly("a");
        }

        @Test
        @DisplayName("lastTwo: 最後の2要素を取得")
        void lastTwoElements() {
            List<String> list = List.of("a", "b", "c", "d");
            assertThat(ImmutableList.lastTwo(list)).containsExactly("c", "d");
        }

        @Test
        @DisplayName("lastTwo: 空リストは空リストを返す")
        void lastTwoEmpty() {
            assertThat(ImmutableList.lastTwo(List.empty())).isEmpty();
        }

        @Test
        @DisplayName("slice: 指定範囲を取得")
        void sliceElements() {
            List<String> list = List.of("a", "b", "c", "d");
            assertThat(ImmutableList.slice(list, 1, 3)).containsExactly("b", "c");
        }

        @Test
        @DisplayName("slice: 開始位置が0の場合")
        void sliceFromStart() {
            List<String> list = List.of("a", "b", "c", "d");
            assertThat(ImmutableList.slice(list, 0, 2)).containsExactly("a", "b");
        }

        @Test
        @DisplayName("slice: 末尾までの場合")
        void sliceToEnd() {
            List<String> list = List.of("a", "b", "c", "d");
            assertThat(ImmutableList.slice(list, 2, 4)).containsExactly("c", "d");
        }
    }

    @Nested
    @DisplayName("リストの変換")
    class TransformationTest {

        @Test
        @DisplayName("movedFirstTwoToTheEnd: 最初の2要素を末尾に移動")
        void moveFirstTwoToEnd() {
            List<String> list = List.of("a", "b", "c");
            assertThat(ImmutableList.movedFirstTwoToTheEnd(list))
                    .containsExactly("c", "a", "b");
        }

        @Test
        @DisplayName("insertedBeforeLast: 最後の要素の前に挿入")
        void insertBeforeLast() {
            List<String> list = List.of("a", "b");
            assertThat(ImmutableList.insertedBeforeLast(list, "c"))
                    .containsExactly("a", "c", "b");
        }

        @Test
        @DisplayName("insertAt: 指定位置に挿入")
        void insertAtPosition() {
            List<String> list = List.of("a", "b", "c");
            assertThat(ImmutableList.insertAt(list, 1, "X"))
                    .containsExactly("a", "X", "b", "c");
        }

        @Test
        @DisplayName("insertAt: 先頭に挿入")
        void insertAtStart() {
            List<String> list = List.of("a", "b", "c");
            assertThat(ImmutableList.insertAt(list, 0, "X"))
                    .containsExactly("X", "a", "b", "c");
        }

        @Test
        @DisplayName("insertAt: 末尾に挿入")
        void insertAtEnd() {
            List<String> list = List.of("a", "b", "c");
            assertThat(ImmutableList.insertAt(list, 3, "X"))
                    .containsExactly("a", "b", "c", "X");
        }

        @Test
        @DisplayName("insertAtMiddle: 中央に挿入（偶数長）")
        void insertAtMiddleEven() {
            List<String> list = List.of("a", "b", "c", "d");
            assertThat(ImmutableList.insertAtMiddle(list, "X"))
                    .containsExactly("a", "b", "X", "c", "d");
        }

        @Test
        @DisplayName("insertAtMiddle: 中央に挿入（奇数長）")
        void insertAtMiddleOdd() {
            List<String> list = List.of("a", "b", "c");
            assertThat(ImmutableList.insertAtMiddle(list, "X"))
                    .containsExactly("a", "X", "b", "c");
        }
    }

    @Nested
    @DisplayName("イミュータブル性の検証")
    class ImmutabilityTest {

        @Test
        @DisplayName("元のリストは変更されない")
        void originalListUnchanged() {
            List<String> original = List.of("a", "b", "c");
            List<String> modified = ImmutableList.movedFirstTwoToTheEnd(original);

            assertThat(original).containsExactly("a", "b", "c");
            assertThat(modified).containsExactly("c", "a", "b");
        }

        @Test
        @DisplayName("append は新しいリストを作成")
        void appendCreatesNewList() {
            List<String> list1 = List.of("a", "b");
            List<String> list2 = list1.append("c");

            assertThat(list1).containsExactly("a", "b");
            assertThat(list2).containsExactly("a", "b", "c");
        }
    }
}
