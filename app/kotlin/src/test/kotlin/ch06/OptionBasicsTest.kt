package ch06

import arrow.core.None
import arrow.core.Some
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class OptionBasicsTest : FunSpec({

    context("nullable 型による安全な計算") {
        test("safeDivide は 0 以外で割ると商を返す") {
            safeDivide(10, 2) shouldBe 5
            safeDivide(7, 2) shouldBe 3
        }

        test("safeDivide は 0 で割ると null を返す") {
            safeDivide(10, 0) shouldBe null
        }

        test("addStrings は 2 つの数値文字列の合計を返す") {
            addStrings("10", "20") shouldBe 30
            addStrings(" 10 ", "20") shouldBe 30
        }

        test("addStrings はどちらかが数値でなければ null を返す") {
            addStrings("10", "abc") shouldBe null
            addStrings("xyz", "20") shouldBe null
        }

        test("addStringsWithLet は ?.let の連鎖でも同じ結果になる") {
            addStringsWithLet("10", "20") shouldBe 30
            addStringsWithLet("10", "abc") shouldBe null
            addStringsWithLet("xyz", "20") shouldBe null
        }
    }

    context("nullable 型の主要な操作") {
        val year: Int? = 996
        val noYear: Int? = null

        test("?.let は値があれば変換する（map 相当）") {
            mapExample(year) shouldBe 1992
            mapExample(noYear) shouldBe null
        }

        test("?.let で nullable を返す関数をつなぐ（flatMap 相当）") {
            flatMapExample(4) shouldBe 25
            flatMapExample(0) shouldBe null
            flatMapExample(noYear) shouldBe null
        }

        test("takeIf は条件を満たさなければ null にする（filter 相当）") {
            filterExample(year, 500) shouldBe 996
            filterExample(year, 1000) shouldBe null
            filterExample(noYear, 500) shouldBe null
        }

        test("?: は null のとき代替の nullable 値を使う（orElse 相当）") {
            orElseExample(7, 8) shouldBe 7
            orElseExample(null, 8) shouldBe 8
            orElseExample(7, null) shouldBe 7
            orElseExample(null, null) shouldBe null
        }

        test("?: はデフォルト値を与えて非 null にする（getOrElse 相当）") {
            getOrElseExample(year, 2020) shouldBe 996
            getOrElseExample(noYear, 2020) shouldBe 2020
        }

        test("listOfNotNull は 0 または 1 要素のリストにする（toList 相当）") {
            toListExample(year) shouldBe listOf(996)
            toListExample(noYear) shouldBe emptyList()
        }
    }

    context("forall / exists 相当の判定") {
        test("forallExample は null なら true、値があれば条件を判定する") {
            forallExample(996, 2020) shouldBe true
            forallExample(null, 2020) shouldBe true
            forallExample(2021, 2020) shouldBe false
        }

        test("existsExample は null なら false、値があれば条件を判定する") {
            existsExample(996, 500) shouldBe true
            existsExample(null, 500) shouldBe false
            existsExample(100, 500) shouldBe false
        }
    }

    context("Arrow Option との比較") {
        val withNullHead: List<Int?> = listOf(null, 2, 3)
        val empty: List<Int?> = emptyList()

        test("firstOrNull では「空のリスト」と「先頭が null」を区別できない") {
            withNullHead.firstOrNull() shouldBe null
            empty.firstOrNull() shouldBe null
        }

        test("firstOrNone なら Option<Int?> として両者を区別できる") {
            firstElement(withNullHead) shouldBe Some(null)
            firstElement(empty) shouldBe None
        }

        test("describeFirst は 3 つの状態を区別して説明する") {
            describeFirst(listOf(1, 2)) shouldBe "first element is 1"
            describeFirst(withNullHead) shouldBe "first element is null"
            describeFirst(empty) shouldBe "list is empty"
        }

        test("option { } DSL で Option を組み立てる") {
            addStringsOption("10", "20") shouldBe Some(30)
            addStringsOption("10", "abc") shouldBe None
        }

        test("toOption と getOrNull で nullable 型と相互に変換できる") {
            toOptionExample(42) shouldBe Some(42)
            toOptionExample(null) shouldBe None
            Some(42).getOrNull() shouldBe 42
            None.getOrNull() shouldBe null
        }
    }
})
