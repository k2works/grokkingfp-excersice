package ch01

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class IntroKotlinTest : FunSpec({

    context("ワードスコア計算") {
        test("命令型と関数型で同じ結果を返す") {
            listOf("", "a", "imperative", "functional", "Kotlin").forEach { word ->
                calculateScoreImperative(word) shouldBe wordScore(word)
            }
        }

        test("空文字列のスコアは 0") {
            wordScore("") shouldBe 0
        }

        test("文字列の長さがスコアになる") {
            wordScore("imperative") shouldBe 10
            wordScore("declarative") shouldBe 11
        }
    }

    context("'a' を除外したスコア計算") {
        test("'a' を除外してスコアを計算") {
            wordScoreWithoutA("Scala") shouldBe 3
            wordScoreWithoutA("banana") shouldBe 3
        }

        test("'a' がない文字列はそのままの長さ") {
            wordScoreWithoutA("function") shouldBe 8
            wordScoreWithoutA("") shouldBe 0
        }

        test("命令型と関数型で同じ結果を返す") {
            listOf("", "Scala", "banana", "Kotlin", "haskell").forEach { word ->
                calculateScoreWithoutAImperative(word) shouldBe wordScoreWithoutA(word)
            }
        }
    }

    context("基本的な純粋関数") {
        test("increment は入力に 1 を加える") {
            increment(0) shouldBe 1
            increment(6) shouldBe 7
            increment(-1) shouldBe 0
            increment(Int.MAX_VALUE - 1) shouldBe Int.MAX_VALUE
        }

        test("add は 2 つの数を加算") {
            add(2, 3) shouldBe 5
            add(-3, 3) shouldBe 0
        }

        test("getFirstCharacter は最初の文字を返す") {
            getFirstCharacter("Kotlin") shouldBe 'K'
        }

        test("concatenate は 2 つの文字列を連結") {
            concatenate("Hello", "World") shouldBe "HelloWorld"
            concatenate("", "Kotlin") shouldBe "Kotlin"
        }

        test("doubleValue は入力を 2 倍にする") {
            doubleValue(5) shouldBe 10
            doubleValue(-4) shouldBe -8
        }

        test("greet は挨拶文を生成") {
            greet("FP") shouldBe "Hello, FP!"
        }

        test("isEven は偶数を判定") {
            isEven(0) shouldBe true
            isEven(2) shouldBe true
            isEven(1) shouldBe false
            isEven(-2) shouldBe true
            isEven(-3) shouldBe false
        }
    }

    context("参照透過性") {
        test("同じ入力に対して常に同じ出力を返す") {
            val score1 = wordScore("Kotlin")
            val score2 = wordScore("Kotlin")
            score1 shouldBe score2
        }

        test("式をその結果で置き換えても動作が変わらない") {
            val total1 = wordScore("Kotlin") + wordScore("Scala")
            val total2 = 6 + 5
            total1 shouldBe total2
        }
    }
})
