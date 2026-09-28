package ch02

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class ShoppingCartTest : FunSpec({

    context("ShoppingCartBad（問題のあるコード）") {
        test("Book を追加すると 5% 割引になる") {
            val cart = ShoppingCartBad()
            cart.addItem("Apple")
            cart.getDiscountPercentage() shouldBe 0
            cart.addItem("Book")
            cart.getDiscountPercentage() shouldBe 5
        }

        test("外部から Book を削除しても割引が残ってしまう") {
            val cart = ShoppingCartBad()
            cart.addItem("Book")
            cart.items.remove("Book")

            cart.items shouldBe emptyList()
            cart.getDiscountPercentage() shouldBe 5
        }

        test("読み取り専用ビューでも内部状態の変更が見えてしまう") {
            val cart = ShoppingCartBad()
            val view: List<String> = cart.itemsView()
            view shouldBe emptyList()

            cart.addItem("Apple")
            view shouldBe listOf("Apple")
        }
    }

    context("getDiscountPercentage（純粋関数版）") {
        test("空のカートは割引なし") {
            getDiscountPercentage(emptyList()) shouldBe 0
        }

        test("Book がないカートは割引なし") {
            getDiscountPercentage(listOf("Apple", "Lemon")) shouldBe 0
        }

        test("Book があるカートは 5% 割引") {
            getDiscountPercentage(listOf("Apple", "Book")) shouldBe 5
        }

        test("Book のみのカートも 5% 割引") {
            getDiscountPercentage(listOf("Book")) shouldBe 5
        }

        test("複数の Book があるカートも 5% 割引") {
            getDiscountPercentage(listOf("Book", "Book")) shouldBe 5
        }
    }

    context("イミュータブル操作") {
        test("plus は新しいリストを作成") {
            val cart1 = listOf("Apple")
            val cart2 = cart1 + "Book"

            cart1 shouldBe listOf("Apple")
            cart2 shouldBe listOf("Apple", "Book")
            getDiscountPercentage(cart1) shouldBe 0
            getDiscountPercentage(cart2) shouldBe 5
        }

        test("minus は新しいリストを作成") {
            val cart1 = listOf("Apple", "Book")
            val cart2 = cart1 - "Book"

            cart1 shouldBe listOf("Apple", "Book")
            cart2 shouldBe listOf("Apple")
            getDiscountPercentage(cart1) shouldBe 5
            getDiscountPercentage(cart2) shouldBe 0
        }

        test("連鎖的な操作でも元のリストは変更されない") {
            val cart1 = listOf("Apple")
            val cart3 = cart1 + "Book" + "Lemon"

            cart1 shouldBe listOf("Apple")
            cart3 shouldBe listOf("Apple", "Book", "Lemon")
            getDiscountPercentage(cart3) shouldBe 5
        }
    }
})
