package ch06;

import io.vavr.control.Option;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第6章: Option の基本操作のテスト
 */
@DisplayName("第6章: Option の基本操作")
class OptionBasicsTest {

    @Nested
    @DisplayName("Option の作成")
    class CreationTest {

        @Test
        @DisplayName("someValue: Some を作成")
        void createSome() {
            Option<Integer> opt = OptionBasics.someValue();
            assertThat(opt.isDefined()).isTrue();
            assertThat(opt.get()).isEqualTo(42);
        }

        @Test
        @DisplayName("noValue: None を作成")
        void createNone() {
            Option<Integer> opt = OptionBasics.noValue();
            assertThat(opt.isEmpty()).isTrue();
        }

        @Test
        @DisplayName("fromNullable: null なら None")
        void fromNullableNull() {
            assertThat(OptionBasics.fromNullable(null).isEmpty()).isTrue();
        }

        @Test
        @DisplayName("fromNullable: 非 null なら Some")
        void fromNullableNotNull() {
            assertThat(OptionBasics.fromNullable("hello").get()).isEqualTo("hello");
        }
    }

    @Nested
    @DisplayName("safeDivide")
    class SafeDivideTest {

        @Test
        @DisplayName("正常な除算")
        void normalDivision() {
            assertThat(OptionBasics.safeDivide(10, 2)).isEqualTo(Option.some(5));
            assertThat(OptionBasics.safeDivide(7, 2)).isEqualTo(Option.some(3));
        }

        @Test
        @DisplayName("0 での除算は None")
        void divisionByZero() {
            assertThat(OptionBasics.safeDivide(10, 0)).isEqualTo(Option.none());
        }
    }

    @Nested
    @DisplayName("addStrings")
    class AddStringsTest {

        @Test
        @DisplayName("両方が有効な数値")
        void bothValid() {
            assertThat(OptionBasics.addStrings("10", "20")).isEqualTo(Option.some(30));
        }

        @Test
        @DisplayName("最初が無効")
        void firstInvalid() {
            assertThat(OptionBasics.addStrings("abc", "20")).isEqualTo(Option.none());
        }

        @Test
        @DisplayName("2番目が無効")
        void secondInvalid() {
            assertThat(OptionBasics.addStrings("10", "xyz")).isEqualTo(Option.none());
        }

        @Test
        @DisplayName("両方が無効")
        void bothInvalid() {
            assertThat(OptionBasics.addStrings("abc", "xyz")).isEqualTo(Option.none());
        }
    }

    @Nested
    @DisplayName("map")
    class MapTest {

        @Test
        @DisplayName("Some を変換")
        void mapSome() {
            assertThat(OptionBasics.mapExample(Option.some(5))).isEqualTo(Option.some(10));
        }

        @Test
        @DisplayName("None は None のまま")
        void mapNone() {
            assertThat(OptionBasics.mapExample(Option.none())).isEqualTo(Option.none());
        }
    }

    @Nested
    @DisplayName("flatMap")
    class FlatMapTest {

        @Test
        @DisplayName("Some で成功する関数を適用")
        void flatMapSuccess() {
            assertThat(OptionBasics.flatMapExample(Option.some(10))).isEqualTo(Option.some(10));
        }

        @Test
        @DisplayName("Some で失敗する関数を適用")
        void flatMapFailure() {
            assertThat(OptionBasics.flatMapExample(Option.some(0))).isEqualTo(Option.none());
        }

        @Test
        @DisplayName("None は None のまま")
        void flatMapNone() {
            assertThat(OptionBasics.flatMapExample(Option.none())).isEqualTo(Option.none());
        }
    }

    @Nested
    @DisplayName("filter")
    class FilterTest {

        @Test
        @DisplayName("条件を満たす Some")
        void filterPass() {
            assertThat(OptionBasics.filterExample(Option.some(100), 50)).isEqualTo(Option.some(100));
        }

        @Test
        @DisplayName("条件を満たさない Some は None")
        void filterFail() {
            assertThat(OptionBasics.filterExample(Option.some(30), 50)).isEqualTo(Option.none());
        }

        @Test
        @DisplayName("None は None のまま")
        void filterNone() {
            assertThat(OptionBasics.filterExample(Option.none(), 50)).isEqualTo(Option.none());
        }
    }

    @Nested
    @DisplayName("orElse")
    class OrElseTest {

        @Test
        @DisplayName("Some は代替を使わない")
        void orElseSome() {
            assertThat(OptionBasics.orElseExample(Option.some(1), Option.some(2)))
                    .isEqualTo(Option.some(1));
        }

        @Test
        @DisplayName("None は代替を使う")
        void orElseNone() {
            assertThat(OptionBasics.orElseExample(Option.none(), Option.some(2)))
                    .isEqualTo(Option.some(2));
        }

        @Test
        @DisplayName("両方 None は None")
        void orElseBothNone() {
            assertThat(OptionBasics.orElseExample(Option.none(), Option.none()))
                    .isEqualTo(Option.none());
        }
    }

    @Nested
    @DisplayName("getOrElse")
    class GetOrElseTest {

        @Test
        @DisplayName("Some は値を返す")
        void getOrElseSome() {
            assertThat(OptionBasics.getOrElseExample(Option.some(42), 0)).isEqualTo(42);
        }

        @Test
        @DisplayName("None はデフォルト値を返す")
        void getOrElseNone() {
            assertThat(OptionBasics.getOrElseExample(Option.none(), 0)).isEqualTo(0);
        }
    }

    @Nested
    @DisplayName("forall / exists")
    class ForallExistsTest {

        @Test
        @DisplayName("forall: Some が条件を満たす")
        void forallSomePass() {
            assertThat(OptionBasics.forallExample(Option.some(100), 200)).isTrue();
        }

        @Test
        @DisplayName("forall: Some が条件を満たさない")
        void forallSomeFail() {
            assertThat(OptionBasics.forallExample(Option.some(100), 50)).isFalse();
        }

        @Test
        @DisplayName("forall: None は常に true")
        void forallNone() {
            assertThat(OptionBasics.forallExample(Option.none(), 50)).isTrue();
        }

        @Test
        @DisplayName("exists: Some が条件を満たす")
        void existsSomePass() {
            assertThat(OptionBasics.existsExample(Option.some(100), 50)).isTrue();
        }

        @Test
        @DisplayName("exists: Some が条件を満たさない")
        void existsSomeFail() {
            assertThat(OptionBasics.existsExample(Option.some(30), 50)).isFalse();
        }

        @Test
        @DisplayName("exists: None は常に false")
        void existsNone() {
            assertThat(OptionBasics.existsExample(Option.none(), 50)).isFalse();
        }
    }
}
