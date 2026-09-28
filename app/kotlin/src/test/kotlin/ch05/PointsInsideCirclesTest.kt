package ch05

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class PointsInsideCirclesTest : FunSpec({
    val points = listOf(Point(5, 2), Point(1, 1))
    val radiuses = listOf(2, 1)

    test("isInside は点が円の内側にあるかを判定する") {
        isInside(Point(1, 1), 2) shouldBe true
        isInside(Point(5, 2), 2) shouldBe false
    }

    test("allCombinations は全組み合わせの判定結果を返す") {
        allCombinations(points, radiuses) shouldBe listOf(
            "Point(x=5, y=2) is within a radius of 2: false",
            "Point(x=1, y=1) is within a radius of 2: true",
            "Point(x=5, y=2) is within a radius of 1: false",
            "Point(x=1, y=1) is within a radius of 1: false",
        )
    }

    context("ガード相当のフィルタリング") {
        val expected = listOf("Point(x=1, y=1) is within a radius of 2")

        test("filter を使う") {
            insidePointsWithFilter(points, radiuses) shouldBe expected
        }

        test("flatMap と insideFilter を使う") {
            insidePointsWithFlatMap(points, radiuses) shouldBe expected
        }

        test("sequence ビルダーの if を使う") {
            insidePointsWithSequence(points, radiuses) shouldBe expected
        }
    }

    test("countPointsPerRadius は半径ごとに内側の点の数を数える") {
        countPointsPerRadius(points, radiuses) shouldBe listOf(
            "Radius 2: 1 points inside",
            "Radius 1: 0 points inside",
        )
    }

    context("any と all") {
        val morePoints = listOf(Point(5, 2), Point(1, 1), Point(0, 0))

        test("pointsInAnyCircle はいずれかの円に含まれる点を返す") {
            pointsInAnyCircle(morePoints, listOf(1, 2)) shouldBe listOf(Point(1, 1), Point(0, 0))
        }

        test("pointsInAllCircles はすべての円に含まれる点を返す") {
            pointsInAllCircles(morePoints, listOf(1, 2)) shouldBe listOf(Point(0, 0))
        }
    }
})
