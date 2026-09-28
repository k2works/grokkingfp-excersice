package ch01

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class IntroKotlinTest : FunSpec({
    test("wordScore は単語の長さを返す") {
        wordScore("imperative") shouldBe 10
    }
})
