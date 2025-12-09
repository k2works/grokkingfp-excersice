package ch02;

import io.vavr.collection.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第2章: チップ計算のテスト
 */
@DisplayName("第2章: チップ計算")
class TipCalculatorTest {

    @Nested
    @DisplayName("getTipPercentage")
    class GetTipPercentageTest {

        @Test
        @DisplayName("空のグループはチップなし")
        void emptyGroupNoTip() {
            List<String> emptyGroup = List.empty();
            assertThat(TipCalculator.getTipPercentage(emptyGroup)).isEqualTo(0);
        }

        @Test
        @DisplayName("1人のグループは10%チップ")
        void onePersonTenPercent() {
            List<String> group = List.of("Alice");
            assertThat(TipCalculator.getTipPercentage(group)).isEqualTo(10);
        }

        @Test
        @DisplayName("5人のグループは10%チップ")
        void fivePersonsTenPercent() {
            List<String> group = List.of("Alice", "Bob", "Charlie", "David", "Eve");
            assertThat(TipCalculator.getTipPercentage(group)).isEqualTo(10);
        }

        @Test
        @DisplayName("6人のグループは20%チップ")
        void sixPersonsTwentyPercent() {
            List<String> group = List.of("Alice", "Bob", "Charlie", "David", "Eve", "Frank");
            assertThat(TipCalculator.getTipPercentage(group)).isEqualTo(20);
        }

        @Test
        @DisplayName("7人以上のグループも20%チップ")
        void morePersonsTwentyPercent() {
            List<String> group = List.of("A", "B", "C", "D", "E", "F", "G", "H");
            assertThat(TipCalculator.getTipPercentage(group)).isEqualTo(20);
        }
    }

    @Nested
    @DisplayName("境界値テスト")
    class BoundaryTest {

        @Test
        @DisplayName("0人→1人の境界")
        void zeroToOneBoundary() {
            assertThat(TipCalculator.getTipPercentage(List.empty())).isEqualTo(0);
            assertThat(TipCalculator.getTipPercentage(List.of("A"))).isEqualTo(10);
        }

        @Test
        @DisplayName("5人→6人の境界")
        void fiveToSixBoundary() {
            List<String> five = List.of("A", "B", "C", "D", "E");
            List<String> six = five.append("F");

            assertThat(TipCalculator.getTipPercentage(five)).isEqualTo(10);
            assertThat(TipCalculator.getTipPercentage(six)).isEqualTo(20);
        }
    }

    @Nested
    @DisplayName("イミュータブル操作")
    class ImmutableOperationsTest {

        @Test
        @DisplayName("メンバー追加は新しいリストを作成")
        void appendCreatesNewList() {
            List<String> group1 = List.of("Alice", "Bob");
            List<String> group2 = group1.append("Charlie");

            // group1 は変更されていない
            assertThat(group1).containsExactly("Alice", "Bob");
            assertThat(group2).containsExactly("Alice", "Bob", "Charlie");

            // チップ率も正しい
            assertThat(TipCalculator.getTipPercentage(group1)).isEqualTo(10);
            assertThat(TipCalculator.getTipPercentage(group2)).isEqualTo(10);
        }

        @Test
        @DisplayName("appendAll で複数のメンバーを追加")
        void appendAllAddsMultipleMembers() {
            List<String> smallGroup = List.of("Alice", "Bob");
            List<String> largeGroup = smallGroup.appendAll(List.of("C", "D", "E", "F"));

            assertThat(smallGroup.size()).isEqualTo(2);
            assertThat(largeGroup.size()).isEqualTo(6);

            assertThat(TipCalculator.getTipPercentage(smallGroup)).isEqualTo(10);
            assertThat(TipCalculator.getTipPercentage(largeGroup)).isEqualTo(20);
        }
    }

    @Nested
    @DisplayName("関数型実装との比較")
    class FunctionalImplementationTest {

        @Test
        @DisplayName("両方の実装が同じ結果を返す")
        void bothImplementationsReturnSameResult() {
            List<String> empty = List.empty();
            List<String> small = List.of("A", "B", "C");
            List<String> large = List.of("A", "B", "C", "D", "E", "F", "G");

            assertThat(TipCalculator.getTipPercentage(empty))
                    .isEqualTo(TipCalculator.getTipPercentageFunctional(empty));

            assertThat(TipCalculator.getTipPercentage(small))
                    .isEqualTo(TipCalculator.getTipPercentageFunctional(small));

            assertThat(TipCalculator.getTipPercentage(large))
                    .isEqualTo(TipCalculator.getTipPercentageFunctional(large));
        }
    }
}
