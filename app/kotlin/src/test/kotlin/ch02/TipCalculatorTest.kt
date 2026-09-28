package ch02

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class TipCalculatorTest : FunSpec({

    context("getTipPercentage") {
        test("空のグループはチップなし") {
            getTipPercentage(emptyList()) shouldBe 0
        }

        test("1 人のグループは 10%") {
            getTipPercentage(listOf("Alice")) shouldBe 10
        }

        test("5 人のグループは 10%") {
            getTipPercentage(listOf("Alice", "Bob", "Charlie", "David", "Eve")) shouldBe 10
        }

        test("6 人以上のグループは 20%") {
            getTipPercentage(listOf("Alice", "Bob", "Charlie", "David", "Eve", "Frank")) shouldBe 20
        }
    }

    context("イミュータブル操作") {
        test("メンバーを追加しても元のグループは変わらない") {
            val group1 = listOf("Alice", "Bob")
            val group2 = group1 + "Charlie"
            val group3 = group2 + listOf("David", "Eve", "Frank")

            group1 shouldBe listOf("Alice", "Bob")
            getTipPercentage(group1) shouldBe 10
            getTipPercentage(group2) shouldBe 10
            getTipPercentage(group3) shouldBe 20
        }
    }
})
