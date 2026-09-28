package ch03

/**
 * 第3章: 旅程の再計画
 *
 * イミュータブルなリスト操作による旅行計画の変更例
 */

/** 旅程の再計画 - 指定した都市の前に新しい都市を挿入 */
fun replan(plan: List<String>, newCity: String, beforeCity: String): List<String> {
    val beforeCityIndex = plan.indexOf(beforeCity)
    val citiesBefore = plan.take(beforeCityIndex)
    val citiesAfter = plan.drop(beforeCityIndex)
    return citiesBefore + newCity + citiesAfter
}

/** 旅程から都市を削除 */
fun removeCity(plan: List<String>, city: String): List<String> =
    plan.filter { it != city }

/** 旅程の 2 つの都市を入れ替え */
fun swapCities(plan: List<String>, city1: String, city2: String): List<String> =
    plan.map { city ->
        when (city) {
            city1 -> city2
            city2 -> city1
            else -> city
        }
    }

/** 拡張関数版の replan - beforeCity の前に newCity を挿入 */
fun List<String>.insertedBefore(beforeCity: String, newCity: String): List<String> =
    replan(this, newCity, beforeCity)
