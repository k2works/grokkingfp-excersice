package ch03;

import io.vavr.collection.List;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第3章: 旅程の再計画のテスト
 */
@DisplayName("第3章: 旅程の再計画")
class ItineraryTest {

    @Nested
    @DisplayName("replan")
    class ReplanTest {

        @Test
        @DisplayName("指定した都市の前に新しい都市を挿入")
        void insertBeforeCity() {
            List<String> plan = List.of("Paris", "Berlin", "Kraków");
            List<String> result = Itinerary.replan(plan, "Vienna", "Kraków");

            assertThat(result).containsExactly("Paris", "Berlin", "Vienna", "Kraków");
        }

        @Test
        @DisplayName("先頭の都市の前に挿入")
        void insertBeforeFirst() {
            List<String> plan = List.of("Paris", "Berlin", "Kraków");
            List<String> result = Itinerary.replan(plan, "London", "Paris");

            assertThat(result).containsExactly("London", "Paris", "Berlin", "Kraków");
        }

        @Test
        @DisplayName("元の旅程は変更されない")
        void originalPlanUnchanged() {
            List<String> planA = List.of("Paris", "Berlin", "Kraków");
            List<String> planB = Itinerary.replan(planA, "Vienna", "Kraków");

            assertThat(planA).containsExactly("Paris", "Berlin", "Kraków");
            assertThat(planB).containsExactly("Paris", "Berlin", "Vienna", "Kraków");
        }
    }

    @Nested
    @DisplayName("removeCity")
    class RemoveCityTest {

        @Test
        @DisplayName("指定した都市を削除")
        void removeCity() {
            List<String> plan = List.of("Paris", "Berlin", "Kraków");
            List<String> result = Itinerary.removeCity(plan, "Berlin");

            assertThat(result).containsExactly("Paris", "Kraków");
        }

        @Test
        @DisplayName("存在しない都市を削除しても変化なし")
        void removeNonExistentCity() {
            List<String> plan = List.of("Paris", "Berlin", "Kraków");
            List<String> result = Itinerary.removeCity(plan, "Vienna");

            assertThat(result).containsExactly("Paris", "Berlin", "Kraków");
        }
    }

    @Nested
    @DisplayName("swapCities")
    class SwapCitiesTest {

        @Test
        @DisplayName("2つの都市を入れ替え")
        void swapTwoCities() {
            List<String> plan = List.of("Paris", "Berlin", "Kraków");
            List<String> result = Itinerary.swapCities(plan, "Paris", "Kraków");

            assertThat(result).containsExactly("Kraków", "Berlin", "Paris");
        }

        @Test
        @DisplayName("同じ都市を指定しても変化なし")
        void swapSameCity() {
            List<String> plan = List.of("Paris", "Berlin", "Kraków");
            List<String> result = Itinerary.swapCities(plan, "Berlin", "Berlin");

            assertThat(result).containsExactly("Paris", "Berlin", "Kraków");
        }
    }
}
