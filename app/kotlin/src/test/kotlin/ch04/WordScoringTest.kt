package ch04

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class WordScoringTest : FunSpec({
    val words = listOf("ada", "haskell", "scala", "java", "rust")

    context("スコア関数") {
        test("score は a を除いた文字数を返す") {
            score("java") shouldBe 2
            score("haskell") shouldBe 6
        }

        test("bonus は c を含むと 5 を返す") {
            bonus("scala") shouldBe 5
            bonus("java") shouldBe 0
        }

        test("penalty は s を含むと 7 を返す") {
            penalty("rust") shouldBe 7
            penalty("java") shouldBe 0
        }
    }

    context("rankedWords") {
        test("基本スコアでランキングする") {
            rankedWords(::score, words) shouldBe listOf("haskell", "rust", "scala", "java", "ada")
        }

        test("ボーナス付きスコアでランキングする") {
            rankedWords({ w -> score(w) + bonus(w) }, words) shouldBe
                listOf("scala", "haskell", "rust", "java", "ada")
        }

        test("ボーナスとペナルティ付きスコアでランキングする") {
            rankedWords({ w -> score(w) + bonus(w) - penalty(w) }, words) shouldBe
                listOf("java", "scala", "ada", "haskell", "rust")
        }

        test("拡張関数 rankedBy は末尾ラムダで書ける") {
            words.rankedBy { score(it) + bonus(it) } shouldBe listOf("scala", "haskell", "rust", "java", "ada")
        }
    }

    context("スコアを利用するその他の関数") {
        test("rankedWordsWithScore はスコア付きの文字列を返す") {
            rankedWordsWithScore(::score, listOf("java", "rust")) shouldBe listOf("rust: 4", "java: 2")
        }

        test("highScoringWords は閾値以上の単語を返す") {
            highScoringWords(::score, words, 4) shouldBe listOf("haskell", "rust")
        }

        test("カリー化された highScoringWordsCurried は閾値と単語リストを後から受け取る") {
            val wordsWithScoreHigherThan = highScoringWordsCurried { w -> score(w) + bonus(w) - penalty(w) }

            wordsWithScoreHigherThan(1)(words) shouldBe listOf("java")
            wordsWithScoreHigherThan(0)(words) shouldBe listOf("ada", "scala", "java")
        }

        test("cumulativeScore は fold で合計スコアを計算する") {
            cumulativeScore(::score, words) shouldBe 16
        }
    }
})
