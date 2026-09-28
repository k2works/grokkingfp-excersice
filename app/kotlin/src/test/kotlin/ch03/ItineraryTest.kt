package ch03

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class ItineraryTest : FunSpec({
    val planA = listOf("Paris", "Berlin", "Kraków")

    test("replan は指定した都市の前に新しい都市を挿入する") {
        replan(planA, "Vienna", "Kraków") shouldBe listOf("Paris", "Berlin", "Vienna", "Kraków")
    }

    test("replan しても元の計画は変わらない") {
        replan(planA, "Vienna", "Kraków")
        planA shouldBe listOf("Paris", "Berlin", "Kraków")
    }

    test("removeCity は指定した都市を取り除く") {
        removeCity(planA, "Berlin") shouldBe listOf("Paris", "Kraków")
    }

    test("swapCities は 2 つの都市を入れ替える") {
        swapCities(planA, "Paris", "Kraków") shouldBe listOf("Kraków", "Berlin", "Paris")
    }

    test("拡張関数版の insertedBefore でもパイプライン風に再計画できる") {
        planA.insertedBefore("Kraków", "Vienna").insertedBefore("Paris", "London") shouldBe
            listOf("London", "Paris", "Berlin", "Vienna", "Kraków")
    }
})
