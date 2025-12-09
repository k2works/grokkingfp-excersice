package ch02;

import io.vavr.collection.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第2章: ショッピングカートのテスト
 */
@DisplayName("第2章: ショッピングカート")
class ShoppingCartTest {

    @Nested
    @DisplayName("getDiscountPercentage")
    class GetDiscountPercentageTest {

        @Test
        @DisplayName("空のカートは割引なし")
        void emptyCartNoDiscount() {
            List<String> emptyCart = List.empty();
            assertThat(ShoppingCart.getDiscountPercentage(emptyCart)).isEqualTo(0);
        }

        @Test
        @DisplayName("Book がないカートは割引なし")
        void noBookNoDiscount() {
            List<String> cart = List.of("Apple", "Orange", "Banana");
            assertThat(ShoppingCart.getDiscountPercentage(cart)).isEqualTo(0);
        }

        @Test
        @DisplayName("Book があるカートは5%割引")
        void withBookFivePercentDiscount() {
            List<String> cart = List.of("Apple", "Book");
            assertThat(ShoppingCart.getDiscountPercentage(cart)).isEqualTo(5);
        }

        @Test
        @DisplayName("Book のみのカートも5%割引")
        void onlyBookFivePercentDiscount() {
            List<String> cart = List.of("Book");
            assertThat(ShoppingCart.getDiscountPercentage(cart)).isEqualTo(5);
        }

        @Test
        @DisplayName("複数の Book があるカートも5%割引")
        void multipleBooksStillFivePercentDiscount() {
            List<String> cart = List.of("Book", "Book", "Apple");
            assertThat(ShoppingCart.getDiscountPercentage(cart)).isEqualTo(5);
        }
    }

    @Nested
    @DisplayName("イミュータブル操作")
    class ImmutableOperationsTest {

        @Test
        @DisplayName("append は新しいリストを作成")
        void appendCreatesNewList() {
            List<String> cart1 = List.of("Apple");
            List<String> cart2 = cart1.append("Book");

            // cart1 は変更されていない
            assertThat(cart1).containsExactly("Apple");
            assertThat(cart2).containsExactly("Apple", "Book");

            // 割引率も正しい
            assertThat(ShoppingCart.getDiscountPercentage(cart1)).isEqualTo(0);
            assertThat(ShoppingCart.getDiscountPercentage(cart2)).isEqualTo(5);
        }

        @Test
        @DisplayName("remove は新しいリストを作成")
        void removeCreatesNewList() {
            List<String> cart1 = List.of("Apple", "Book", "Orange");
            List<String> cart2 = cart1.remove("Book");

            // cart1 は変更されていない
            assertThat(cart1).containsExactly("Apple", "Book", "Orange");
            assertThat(cart2).containsExactly("Apple", "Orange");

            // 割引率も正しい
            assertThat(ShoppingCart.getDiscountPercentage(cart1)).isEqualTo(5);
            assertThat(ShoppingCart.getDiscountPercentage(cart2)).isEqualTo(0);
        }

        @Test
        @DisplayName("連鎖的な操作でも元のリストは変更されない")
        void chainedOperationsDoNotModifyOriginal() {
            List<String> original = List.of("Apple");

            List<String> modified = original
                    .append("Book")
                    .append("Orange")
                    .remove("Apple");

            assertThat(original).containsExactly("Apple");
            assertThat(modified).containsExactly("Book", "Orange");
        }
    }

    @Nested
    @DisplayName("純粋関数としての特性")
    class PureFunctionCharacteristicsTest {

        @Test
        @DisplayName("同じ入力に対して常に同じ出力")
        void sameInputSameOutput() {
            List<String> cart = List.of("Apple", "Book");

            assertThat(ShoppingCart.getDiscountPercentage(cart))
                    .isEqualTo(ShoppingCart.getDiscountPercentage(cart))
                    .isEqualTo(ShoppingCart.getDiscountPercentage(cart));
        }

        @Test
        @DisplayName("関数呼び出しは入力を変更しない")
        void functionDoesNotModifyInput() {
            List<String> cart = List.of("Apple", "Book");
            List<String> cartCopy = List.of("Apple", "Book");

            ShoppingCart.getDiscountPercentage(cart);
            ShoppingCart.getDiscountPercentage(cart);
            ShoppingCart.getDiscountPercentage(cart);

            assertThat(cart).isEqualTo(cartCopy);
        }
    }
}
