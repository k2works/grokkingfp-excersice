package ch04

import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class HigherOrderFunctionsTest : FunSpec({
    context("sortedBy") {
        test("関数参照でソート基準を渡せる") {
            listOf("rust", "java").sortedBy(::score) shouldBe listOf("java", "rust")
        }

        test("sortByFunction は任意のキー関数でソートする") {
            sortByFunction(listOf("scala", "rust", "ada")) { it.length } shouldBe listOf("ada", "rust", "scala")
        }
    }

    context("map") {
        test("lengths は各文字列の長さを返す") {
            lengths(listOf("scala", "rust", "ada")) shouldBe listOf(5, 4, 3)
        }

        test("doubles は各要素を 2 倍にする") {
            doubles(listOf(5, 1, 2, 4, 0)) shouldBe listOf(10, 2, 4, 8, 0)
        }

        test("末尾ラムダと it で直接書ける") {
            listOf(5, 1, 2, 4, 0).map { it * 2 } shouldBe listOf(10, 2, 4, 8, 0)
        }
    }

    context("filter") {
        test("filterOdds は奇数のみを返す") {
            filterOdds(listOf(5, 1, 2, 4, 0)) shouldBe listOf(5, 1)
        }

        test("filterLargerThan4 は 4 より大きい数のみを返す") {
            filterLargerThan4(listOf(5, 1, 2, 4, 0)) shouldBe listOf(5)
        }
    }

    context("fold と reduce") {
        test("sum は fold で合計を計算する") {
            sum(listOf(5, 1, 2, 4, 100)) shouldBe 112
        }

        test("largest は fold で最大値を求める") {
            largest(listOf(5, 1, 2, 4, 15)) shouldBe 15
        }

        test("fold は空リストに対して初期値を返す") {
            sum(emptyList()) shouldBe 0
        }

        test("reduce は空リストに対して例外を投げる") {
            shouldThrow<UnsupportedOperationException> {
                emptyList<Int>().reduce { acc, i -> acc + i }
            }
        }

        test("reduceOrNull は空リストに対して null を返す") {
            largestOrNull(emptyList()) shouldBe null
            largestOrNull(listOf(5, 1, 2, 4, 15)) shouldBe 15
        }

        test("concatenate は文字列を連結する") {
            concatenate(listOf("a", "b", "c")) shouldBe "abc"
        }

        test("join は区切り文字で連結する") {
            join(listOf("a", "b", "c"), ", ") shouldBe "a, b, c"
            join(emptyList(), ", ") shouldBe ""
        }
    }

    context("関数を返す関数") {
        val numbers = listOf(5, 1, 2, 4, 0)

        test("largerThan は指定値より大きいかを判定する関数を返す") {
            numbers.filter(largerThan(4)) shouldBe listOf(5)
            numbers.filter(largerThan(1)) shouldBe listOf(5, 2, 4)
        }

        test("divisibleBy は割り切れるかを判定する関数を返す") {
            numbers.filter(divisibleBy(2)) shouldBe listOf(2, 4, 0)
        }

        test("containsText は部分文字列を含むかを判定する関数を返す") {
            listOf("scala", "rust", "ada").filter(containsText("a")) shouldBe listOf("scala", "ada")
        }
    }

    context("カリー化") {
        val numbers = listOf(5, 1, 2, 4, 0)

        test("カリー化された関数は部分適用できる") {
            numbers.filter(largerThanCurried(4)) shouldBe listOf(5)
        }

        test("curry は 2 引数関数をカリー化する") {
            val curried = curry(::largerThanNormal)
            numbers.filter(curried(1)) shouldBe listOf(5, 2, 4)
        }
    }
})
