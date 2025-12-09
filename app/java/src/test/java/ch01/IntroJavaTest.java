package ch01;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第1章: 関数型プログラミング入門のテスト
 */
@DisplayName("第1章: 関数型プログラミング入門")
class IntroJavaTest {

    @Nested
    @DisplayName("ワードスコア計算")
    class WordScoreTest {

        @Test
        @DisplayName("命令型と関数型で同じ結果を返す")
        void imperativeAndFunctionalReturnSameResult() {
            String word = "functional";
            assertThat(IntroJava.calculateScoreImperative(word))
                    .isEqualTo(IntroJava.wordScore(word));
        }

        @Test
        @DisplayName("空文字列のスコアは0")
        void emptyStringScoreIsZero() {
            assertThat(IntroJava.wordScore("")).isEqualTo(0);
        }

        @Test
        @DisplayName("文字列の長さがスコアになる")
        void scoreEqualsLength() {
            assertThat(IntroJava.wordScore("Java")).isEqualTo(4);
            assertThat(IntroJava.wordScore("Scala")).isEqualTo(5);
            assertThat(IntroJava.wordScore("functional")).isEqualTo(10);
        }
    }

    @Nested
    @DisplayName("'a' を除外したスコア計算")
    class WordScoreWithoutATest {

        @Test
        @DisplayName("'a' を除外してスコアを計算")
        void excludesLetterA() {
            assertThat(IntroJava.wordScoreWithoutA("Scala")).isEqualTo(3); // "Scl"
            assertThat(IntroJava.wordScoreWithoutA("Java")).isEqualTo(2);  // "Jv"
            assertThat(IntroJava.wordScoreWithoutA("aaa")).isEqualTo(0);   // ""
        }

        @Test
        @DisplayName("'a' がない文字列はそのままの長さ")
        void withoutAReturnsFullLength() {
            assertThat(IntroJava.wordScoreWithoutA("function")).isEqualTo(8);
        }

        @Test
        @DisplayName("命令型と関数型で同じ結果を返す")
        void imperativeAndFunctionalReturnSameResult() {
            String word = "banana";
            assertThat(IntroJava.calculateScoreWithoutAImperative(word))
                    .isEqualTo(IntroJava.wordScoreWithoutA(word));
        }
    }

    @Nested
    @DisplayName("基本的な純粋関数")
    class BasicPureFunctionsTest {

        @Test
        @DisplayName("increment は入力に1を加える")
        void incrementAddOne() {
            assertThat(IntroJava.increment(0)).isEqualTo(1);
            assertThat(IntroJava.increment(6)).isEqualTo(7);
            assertThat(IntroJava.increment(-1)).isEqualTo(0);
            assertThat(IntroJava.increment(Integer.MAX_VALUE - 1)).isEqualTo(Integer.MAX_VALUE);
        }

        @Test
        @DisplayName("add は2つの数を加算")
        void addSumsTwoNumbers() {
            assertThat(IntroJava.add(2, 3)).isEqualTo(5);
            assertThat(IntroJava.add(0, 0)).isEqualTo(0);
            assertThat(IntroJava.add(-1, 1)).isEqualTo(0);
        }

        @Test
        @DisplayName("getFirstCharacter は最初の文字を返す")
        void getFirstCharacterReturnsFirst() {
            assertThat(IntroJava.getFirstCharacter("Java")).isEqualTo('J');
            assertThat(IntroJava.getFirstCharacter("abc")).isEqualTo('a');
        }

        @Test
        @DisplayName("concatenate は2つの文字列を連結")
        void concatenateJoinsStrings() {
            assertThat(IntroJava.concatenate("Hello", "World")).isEqualTo("HelloWorld");
            assertThat(IntroJava.concatenate("", "test")).isEqualTo("test");
        }

        @Test
        @DisplayName("doubleValue は入力を2倍にする")
        void doubleValueDoubles() {
            assertThat(IntroJava.doubleValue(5)).isEqualTo(10);
            assertThat(IntroJava.doubleValue(0)).isEqualTo(0);
            assertThat(IntroJava.doubleValue(-3)).isEqualTo(-6);
        }

        @Test
        @DisplayName("greet は挨拶文を生成")
        void greetGeneratesGreeting() {
            assertThat(IntroJava.greet("FP")).isEqualTo("Hello, FP!");
            assertThat(IntroJava.greet("World")).isEqualTo("Hello, World!");
        }

        @Test
        @DisplayName("isEven は偶数を判定")
        void isEvenChecksEvenness() {
            assertThat(IntroJava.isEven(0)).isTrue();
            assertThat(IntroJava.isEven(2)).isTrue();
            assertThat(IntroJava.isEven(4)).isTrue();
            assertThat(IntroJava.isEven(1)).isFalse();
            assertThat(IntroJava.isEven(3)).isFalse();
            assertThat(IntroJava.isEven(-2)).isTrue();
            assertThat(IntroJava.isEven(-3)).isFalse();
        }
    }

    @Nested
    @DisplayName("参照透過性")
    class ReferentialTransparencyTest {

        @Test
        @DisplayName("同じ入力に対して常に同じ出力を返す")
        void sameInputSameOutput() {
            // 複数回呼び出しても同じ結果
            assertThat(IntroJava.wordScore("test"))
                    .isEqualTo(IntroJava.wordScore("test"))
                    .isEqualTo(IntroJava.wordScore("test"));

            assertThat(IntroJava.increment(5))
                    .isEqualTo(IntroJava.increment(5))
                    .isEqualTo(IntroJava.increment(5));
        }

        @Test
        @DisplayName("式をその結果で置き換えても動作が変わらない")
        void expressionCanBeReplacedWithValue() {
            // wordScore("Java") + wordScore("Scala") と 4 + 5 は同等
            int result1 = IntroJava.wordScore("Java") + IntroJava.wordScore("Scala");
            int result2 = 4 + 5;
            assertThat(result1).isEqualTo(result2);
        }
    }
}
