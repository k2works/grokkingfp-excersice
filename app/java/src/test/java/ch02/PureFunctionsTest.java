package ch02;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第2章: 純粋関数のテスト
 */
@DisplayName("第2章: 純粋関数")
class PureFunctionsTest {

    @Nested
    @DisplayName("increment")
    class IncrementTest {

        @Test
        @DisplayName("正の数をインクリメント")
        void incrementsPositiveNumber() {
            assertThat(PureFunctions.increment(5)).isEqualTo(6);
            assertThat(PureFunctions.increment(0)).isEqualTo(1);
        }

        @Test
        @DisplayName("負の数をインクリメント")
        void incrementsNegativeNumber() {
            assertThat(PureFunctions.increment(-1)).isEqualTo(0);
            assertThat(PureFunctions.increment(-6)).isEqualTo(-5);
        }

        @Test
        @DisplayName("境界値のインクリメント")
        void incrementsBoundaryValues() {
            assertThat(PureFunctions.increment(Integer.MAX_VALUE - 1))
                    .isEqualTo(Integer.MAX_VALUE);
        }

        @Test
        @DisplayName("同じ入力に対して常に同じ出力")
        void consistentOutput() {
            assertThat(PureFunctions.increment(10))
                    .isEqualTo(PureFunctions.increment(10))
                    .isEqualTo(PureFunctions.increment(10));
        }
    }

    @Nested
    @DisplayName("add")
    class AddTest {

        @Test
        @DisplayName("2つの正の数を加算")
        void addsTwoPositiveNumbers() {
            assertThat(PureFunctions.add(2, 3)).isEqualTo(5);
            assertThat(PureFunctions.add(10, 20)).isEqualTo(30);
        }

        @Test
        @DisplayName("0を含む加算")
        void addsWithZero() {
            assertThat(PureFunctions.add(0, 0)).isEqualTo(0);
            assertThat(PureFunctions.add(5, 0)).isEqualTo(5);
            assertThat(PureFunctions.add(0, 5)).isEqualTo(5);
        }

        @Test
        @DisplayName("負の数を含む加算")
        void addsWithNegativeNumbers() {
            assertThat(PureFunctions.add(-1, 1)).isEqualTo(0);
            assertThat(PureFunctions.add(-3, -5)).isEqualTo(-8);
        }

        @Test
        @DisplayName("加算の交換法則")
        void additionIsCommutative() {
            assertThat(PureFunctions.add(3, 7)).isEqualTo(PureFunctions.add(7, 3));
        }
    }

    @Nested
    @DisplayName("getFirstCharacter")
    class GetFirstCharacterTest {

        @Test
        @DisplayName("文字列の最初の文字を返す")
        void returnsFirstCharacter() {
            assertThat(PureFunctions.getFirstCharacter("Hello")).isEqualTo('H');
            assertThat(PureFunctions.getFirstCharacter("abc")).isEqualTo('a');
            assertThat(PureFunctions.getFirstCharacter("123")).isEqualTo('1');
        }

        @Test
        @DisplayName("1文字の文字列")
        void singleCharacterString() {
            assertThat(PureFunctions.getFirstCharacter("X")).isEqualTo('X');
        }
    }

    @Nested
    @DisplayName("applyDiscount")
    class ApplyDiscountTest {

        @Test
        @DisplayName("5%の割引を適用")
        void appliesFivePercentDiscount() {
            assertThat(PureFunctions.applyDiscount(100)).isEqualTo(95.0);
            assertThat(PureFunctions.applyDiscount(200)).isEqualTo(190.0);
        }

        @Test
        @DisplayName("0に対する割引")
        void discountOnZero() {
            assertThat(PureFunctions.applyDiscount(0)).isEqualTo(0.0);
        }

        @Test
        @DisplayName("小数を含む計算")
        void discountWithDecimals() {
            assertThat(PureFunctions.applyDiscount(20)).isEqualTo(19.0);
            assertThat(PureFunctions.applyDiscount(10)).isEqualTo(9.5);
        }
    }

    @Nested
    @DisplayName("calculateStringScore")
    class CalculateStringScoreTest {

        @Test
        @DisplayName("文字列の長さの3倍を返す")
        void returnsThreeTimesLength() {
            assertThat(PureFunctions.calculateStringScore("Scala")).isEqualTo(15); // 5 * 3
            assertThat(PureFunctions.calculateStringScore("Java")).isEqualTo(12);  // 4 * 3
            assertThat(PureFunctions.calculateStringScore("")).isEqualTo(0);       // 0 * 3
        }
    }

    @Nested
    @DisplayName("不純な関数との比較")
    class ImpureFunctionComparisonTest {

        @Test
        @DisplayName("randomPart は毎回異なる可能性がある")
        void randomPartMayReturnDifferentValues() {
            // 注意: このテストは確率的に失敗する可能性がある
            // 10回呼び出して、全て同じでないことを期待
            double first = PureFunctions.randomPart(100);
            boolean allSame = true;
            for (int i = 0; i < 10; i++) {
                if (PureFunctions.randomPart(100) != first) {
                    allSame = false;
                    break;
                }
            }
            // 高確率で異なる値が出るはず
            assertThat(allSame).isFalse();
        }

        @Test
        @DisplayName("currentTime は毎回異なる")
        void currentTimeReturnsDifferentValues() throws InterruptedException {
            long time1 = PureFunctions.currentTime();
            Thread.sleep(1); // 1ミリ秒待機
            long time2 = PureFunctions.currentTime();
            assertThat(time2).isGreaterThan(time1);
        }
    }
}
