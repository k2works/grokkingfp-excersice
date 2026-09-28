package ch05

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class FlatMapExamplesTest : FunSpec({
    context("flatten") {
        test("ネストしたリストを平坦化する") {
            flattenList(listOf(listOf(1, 2), listOf(3, 4, 5), listOf(6))) shouldBe listOf(1, 2, 3, 4, 5, 6)
        }

        test("flatMap { it } は flatten と同じ結果になる") {
            val nested = listOf(listOf(1, 2), listOf(3))
            nested.flatMap { it } shouldBe nested.flatten()
        }
    }

    context("flatMap によるリストサイズの変化") {
        test("要素数が増える") {
            duplicate(listOf(1, 2, 3)) shouldBe listOf(1, 11, 2, 12, 3, 13)
        }

        test("要素数が変わらない") {
            listOf(1, 2, 3).flatMap { listOf(it * 2) } shouldBe listOf(2, 4, 6)
        }

        test("要素数が減る（filter の代わり）") {
            filterEvens(listOf(1, 2, 3, 4, 5)) shouldBe listOf(2, 4)
        }
    }

    context("実用的な例") {
        test("toChars は単語を文字に分解する") {
            toChars(listOf("hi", "bye")) shouldBe listOf('h', 'i', 'b', 'y', 'e')
        }

        test("splitAndFlatten は文を単語に分解する") {
            splitAndFlatten(listOf("hello world", "foo bar baz")) shouldBe
                listOf("hello", "world", "foo", "bar", "baz")
        }

        test("ranges は 1 から n までの範囲を連結する") {
            ranges(listOf(2, 3)) shouldBe listOf(1, 2, 1, 2, 3)
        }

        test("cartesianProduct は直積を返す") {
            cartesianProduct(listOf("Red", "Blue"), listOf(1, 2)) shouldBe
                listOf("(Red, 1)", "(Red, 2)", "(Blue, 1)", "(Blue, 2)")
        }
    }

    context("ネストした flatMap と sequence ビルダー") {
        val expected = listOf(111, 211, 121, 221, 112, 212, 122, 222)

        test("ネストした flatMap で 3 つのリストを組み合わせる") {
            sumsWithFlatMap(listOf(1, 2), listOf(10, 20), listOf(100, 200)) shouldBe expected
        }

        test("sequence ビルダーでも同じ結果になる") {
            sumsWithSequence(listOf(1, 2), listOf(10, 20), listOf(100, 200)) shouldBe expected
        }
    }

    context("flatMap の戻り値の型") {
        test("Iterable の flatMap は常に List を返すため重複が残る") {
            productsFromSet(setOf(1, 2), listOf(2, 1)) shouldBe listOf(2, 1, 4, 2)
        }

        test("flatMapTo で Set を指定すると重複が除かれる") {
            distinctProducts(setOf(1, 2), listOf(2, 1)) shouldBe setOf(2, 1, 4)
        }
    }
})
