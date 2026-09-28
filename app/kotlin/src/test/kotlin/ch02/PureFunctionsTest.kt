package ch02

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.doubles.plusOrMinus
import io.kotest.matchers.longs.shouldBeGreaterThan
import io.kotest.matchers.shouldBe

class PureFunctionsTest : FunSpec({

    context("increment") {
        test("正の数をインクリメント") {
            increment(5) shouldBe 6
        }

        test("負の数をインクリメント") {
            increment(-5) shouldBe -4
        }

        test("境界値のインクリメント") {
            increment(Int.MAX_VALUE - 1) shouldBe Int.MAX_VALUE
        }

        test("同じ入力に対して常に同じ出力") {
            List(3) { increment(10) } shouldBe listOf(11, 11, 11)
        }
    }

    context("add") {
        test("2 つの正の数を加算") {
            add(2, 3) shouldBe 5
            add(10, 20) shouldBe 30
        }

        test("0 を含む加算") {
            add(0, 0) shouldBe 0
            add(5, 0) shouldBe 5
            add(0, 5) shouldBe 5
        }

        test("負の数を含む加算") {
            add(-1, 1) shouldBe 0
            add(-3, -5) shouldBe -8
        }

        test("加算の交換法則") {
            add(3, 7) shouldBe add(7, 3)
        }
    }

    context("getFirstCharacter") {
        test("文字列の最初の文字を返す") {
            getFirstCharacter("Hello") shouldBe 'H'
        }

        test("1 文字の文字列") {
            getFirstCharacter("a") shouldBe 'a'
        }
    }

    context("applyDiscount") {
        test("5% の割引を適用") {
            applyDiscount(100.0) shouldBe 95.0
        }

        test("0 に対する割引") {
            applyDiscount(0.0) shouldBe 0.0
        }

        test("小数を含む計算") {
            applyDiscount(10.5) shouldBe (9.975 plusOrMinus 0.0001)
        }
    }

    context("calculateStringScore") {
        test("文字列の長さの 3 倍を返す") {
            calculateStringScore("Kotlin") shouldBe 18
            calculateStringScore("") shouldBe 0
        }
    }

    context("不純な関数との比較") {
        test("randomPart は同じ入力でも毎回異なる値を返しうる") {
            val results = List(10) { randomPart(10.0) }.toSet()
            (results.size > 1) shouldBe true
        }

        test("currentTime は呼び出すタイミングで結果が変わる") {
            val t1 = currentTime()
            Thread.sleep(5)
            val t2 = currentTime()
            t2 shouldBeGreaterThan t1
        }
    }
})
