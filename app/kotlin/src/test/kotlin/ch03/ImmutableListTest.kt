package ch03

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class ImmutableListTest : FunSpec({
    context("plus による要素の追加") {
        test("plus は新しいリストを返し、元のリストは変わらない") {
            val appleBook = listOf("Apple", "Book")
            val appleBookMango = appleBook + "Mango"

            appleBook shouldBe listOf("Apple", "Book")
            appleBookMango shouldBe listOf("Apple", "Book", "Mango")
        }

        test("plus にリストを渡すと全要素を連結する") {
            listOf("a", "b").plus(listOf("c", "d")) shouldBe listOf("a", "b", "c", "d")
        }
    }

    context("読み取り専用 List はイミュータブルではない") {
        test("MutableList を List として参照しても、元のリストの変更は見えてしまう") {
            val mutable = mutableListOf("a", "b")
            val readOnly: List<String> = mutable

            mutable.add("c")

            readOnly shouldBe listOf("a", "b", "c")
        }

        test("toList で防御的コピーを取れば、元のリストの変更の影響を受けない") {
            val mutable = mutableListOf("a", "b")
            val snapshot = snapshotOf(mutable)

            mutable.add("c")

            snapshot shouldBe listOf("a", "b")
        }
    }

    context("take / drop / slice") {
        val letters = listOf("a", "b", "c", "d")

        test("firstTwo は先頭の 2 要素を返す") {
            firstTwo(letters) shouldBe listOf("a", "b")
        }

        test("lastTwo は末尾の 2 要素を返す") {
            lastTwo(letters) shouldBe listOf("c", "d")
        }

        test("slice は start 以上 end 未満の要素を返す") {
            slice(letters, 1, 3) shouldBe listOf("b", "c")
        }

        test("要素数より大きな値を渡しても take は例外を投げない") {
            firstTwo(listOf("a")) shouldBe listOf("a")
        }
    }

    context("リストの変換") {
        test("movedFirstTwoToTheEnd は先頭 2 要素を末尾に移動する") {
            movedFirstTwoToTheEnd(listOf("a", "b", "c")) shouldBe listOf("c", "a", "b")
        }

        test("insertedBeforeLast は最後の要素の前に挿入する") {
            insertedBeforeLast(listOf("a", "b"), "c") shouldBe listOf("a", "c", "b")
        }

        test("insertAt は指定位置に挿入する") {
            insertAt(listOf("a", "b", "c"), 1, "X") shouldBe listOf("a", "X", "b", "c")
        }

        test("insertAtMiddle はリストの中央に挿入する") {
            insertAtMiddle(listOf("a", "b", "c", "d"), "X") shouldBe listOf("a", "b", "X", "c", "d")
            insertAtMiddle(listOf("a", "b"), "X") shouldBe listOf("a", "X", "b")
        }
    }

    context("buildList") {
        test("buildList は内部で可変操作をしても読み取り専用の List を返す") {
            squares(4) shouldBe listOf(1, 4, 9, 16)
        }
    }

    context("String と List の類似性") {
        test("List と String はどちらも + で連結できる") {
            listOf("a", "b") + listOf("c", "d") shouldBe listOf("a", "b", "c", "d")
            "ab" + "cd" shouldBe "abcd"
        }

        test("List の slice と String の substring は同じ形をしている") {
            listOf("a", "b", "c", "d").slice(1 until 3) shouldBe listOf("b", "c")
            "abcd".substring(1, 3) shouldBe "bc"
        }
    }

    context("名前の省略") {
        test("abbreviate は名を頭文字に省略する") {
            abbreviate("Alonzo Church") shouldBe "A. Church"
        }

        test("すでに省略されている名前はそのまま") {
            abbreviate("A. Church") shouldBe "A. Church"
        }
    }
})
