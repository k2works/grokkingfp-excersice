package ch05

/**
 * 第5章: 円内の点の判定
 *
 * flatMap による組み合わせ生成と、ガード相当のフィルタリング
 */

data class Point(val x: Int, val y: Int)

/** 点が半径 radius の円の内側にあるか判定 */
fun isInside(point: Point, radius: Int): Boolean =
    radius * radius >= point.x * point.x + point.y * point.y

/** 全組み合わせの判定結果を生成 */
fun allCombinations(points: List<Point>, radiuses: List<Int>): List<String> =
    radiuses.flatMap { r ->
        points.map { point -> "$point is within a radius of $r: ${isInside(point, r)}" }
    }

/** filter によるフィルタリング */
fun insidePointsWithFilter(points: List<Point>, radiuses: List<Int>): List<String> =
    radiuses.flatMap { r ->
        points.filter { point -> isInside(point, r) }
            .map { point -> "$point is within a radius of $r" }
    }

/** 条件を満たせば 1 要素、満たさなければ空のリストを返す */
fun insideFilter(point: Point, radius: Int): List<Point> =
    if (isInside(point, radius)) listOf(point) else emptyList()

/** flatMap によるフィルタリング */
fun insidePointsWithFlatMap(points: List<Point>, radiuses: List<Int>): List<String> =
    radiuses.flatMap { r ->
        points.flatMap { point ->
            insideFilter(point, r).map { inside -> "$inside is within a radius of $r" }
        }
    }

/** sequence ビルダーの if によるフィルタリング（ガード式相当） */
fun insidePointsWithSequence(points: List<Point>, radiuses: List<Int>): List<String> =
    sequence {
        for (r in radiuses)
            for (point in points)
                if (isInside(point, r)) yield("$point is within a radius of $r")
    }.toList()

/** 半径ごとに内側の点の数を数える */
fun countPointsPerRadius(points: List<Point>, radiuses: List<Int>): List<String> =
    radiuses.map { r ->
        val count = points.count { isInside(it, r) }
        "Radius $r: $count points inside"
    }

/** いずれかの円に含まれる点を取得 */
fun pointsInAnyCircle(points: List<Point>, radiuses: List<Int>): List<Point> =
    points.filter { point -> radiuses.any { r -> isInside(point, r) } }

/** すべての円に含まれる点を取得 */
fun pointsInAllCircles(points: List<Point>, radiuses: List<Int>): List<Point> =
    points.filter { point -> radiuses.all { r -> isInside(point, r) } }
