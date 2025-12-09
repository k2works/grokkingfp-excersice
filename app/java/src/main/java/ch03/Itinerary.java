package ch03;

import io.vavr.collection.List;

/**
 * 第3章: 旅程の再計画
 *
 * イミュータブルなリスト操作による旅行計画の変更例
 */
public class Itinerary {

    /**
     * 旅程の再計画 - 指定した都市の前に新しい都市を挿入
     */
    public static List<String> replan(List<String> plan, String newCity, String beforeCity) {
        int beforeCityIndex = plan.indexOf(beforeCity);
        List<String> citiesBefore = plan.take(beforeCityIndex);
        List<String> citiesAfter = plan.drop(beforeCityIndex);
        return citiesBefore.append(newCity).appendAll(citiesAfter);
    }

    /**
     * 旅程から都市を削除
     */
    public static List<String> removeCity(List<String> plan, String city) {
        return plan.filter(c -> !c.equals(city));
    }

    /**
     * 旅程の都市を入れ替え
     */
    public static List<String> swapCities(List<String> plan, String city1, String city2) {
        return plan.map(city -> {
            if (city.equals(city1)) return city2;
            if (city.equals(city2)) return city1;
            return city;
        });
    }

    public static void main(String[] args) {
        System.out.println("=== 旅程の再計画 ===\n");

        List<String> planA = List.of("Paris", "Berlin", "Kraków");
        System.out.println("元の旅程: " + planA);

        // Vienna を Kraków の前に挿入
        List<String> planB = replan(planA, "Vienna", "Kraków");
        System.out.println("Vienna を追加: " + planB);
        System.out.println("元の旅程は変わらない: " + planA);

        // 都市の削除
        List<String> planC = removeCity(planB, "Berlin");
        System.out.println("Berlin を削除: " + planC);

        // 都市の入れ替え
        List<String> planD = swapCities(planA, "Paris", "Kraków");
        System.out.println("Paris と Kraków を入れ替え: " + planD);
    }
}
