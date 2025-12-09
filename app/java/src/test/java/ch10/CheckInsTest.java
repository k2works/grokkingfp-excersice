package ch10;

import io.vavr.collection.HashMap;
import io.vavr.collection.List;
import io.vavr.collection.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static ch10.CheckIns.*;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * 第10章: チェックインのテスト
 */
@DisplayName("第10章: チェックイン集計")
class CheckInsTest {

    @Nested
    @DisplayName("topCities")
    class TopCitiesTest {

        @Test
        @DisplayName("チェックイン数でソート")
        void sortsByCheckIns() {
            Map<City, Integer> checkIns = HashMap.of(
                    new City("Tokyo"), 100,
                    new City("New York"), 50,
                    new City("London"), 75
            );

            List<CityStats> top = topCities(checkIns, 3);

            assertThat(top.map(CityStats::city).map(City::name))
                    .containsExactly("Tokyo", "London", "New York");
        }

        @Test
        @DisplayName("指定した数だけ返す")
        void returnsTopN() {
            Map<City, Integer> checkIns = HashMap.of(
                    new City("A"), 10,
                    new City("B"), 20,
                    new City("C"), 30,
                    new City("D"), 40,
                    new City("E"), 50
            );

            List<CityStats> top3 = topCities(checkIns, 3);

            assertThat(top3).hasSize(3);
            assertThat(top3.get(0).city().name()).isEqualTo("E");
            assertThat(top3.get(1).city().name()).isEqualTo("D");
            assertThat(top3.get(2).city().name()).isEqualTo("C");
        }

        @Test
        @DisplayName("空のマップは空のリスト")
        void emptyMapReturnsEmpty() {
            List<CityStats> top = topCities(HashMap.empty(), 3);
            assertThat(top).isEmpty();
        }
    }

    @Nested
    @DisplayName("updateCheckIns")
    class UpdateCheckInsTest {

        @Test
        @DisplayName("新しい都市を追加")
        void addsNewCity() {
            Map<City, Integer> current = HashMap.empty();
            City city = new City("Tokyo");

            Map<City, Integer> updated = updateCheckIns(current, city);

            assertThat(updated.get(city).get()).isEqualTo(1);
        }

        @Test
        @DisplayName("既存の都市をインクリメント")
        void incrementsExisting() {
            City city = new City("Tokyo");
            Map<City, Integer> current = HashMap.of(city, 5);

            Map<City, Integer> updated = updateCheckIns(current, city);

            assertThat(updated.get(city).get()).isEqualTo(6);
        }

        @Test
        @DisplayName("複数回更新")
        void multipleUpdates() {
            City tokyo = new City("Tokyo");
            City london = new City("London");

            Map<City, Integer> result = HashMap.<City, Integer>empty()
                    .transform(m -> updateCheckIns(m, tokyo))
                    .transform(m -> updateCheckIns(m, tokyo))
                    .transform(m -> updateCheckIns(m, london))
                    .transform(m -> updateCheckIns(m, tokyo));

            assertThat(result.get(tokyo).get()).isEqualTo(3);
            assertThat(result.get(london).get()).isEqualTo(1);
        }
    }

    @Nested
    @DisplayName("processCheckInsSequential")
    class SequentialTest {

        @Test
        @DisplayName("逐次処理でトップ都市を計算")
        void processesSequentially() {
            List<City> checkIns = List.of(
                    new City("Tokyo"),
                    new City("Tokyo"),
                    new City("Tokyo"),
                    new City("New York"),
                    new City("New York"),
                    new City("London")
            );

            List<CityStats> top = processCheckInsSequential(checkIns, 2).unsafeRun();

            assertThat(top).hasSize(2);
            assertThat(top.get(0).city().name()).isEqualTo("Tokyo");
            assertThat(top.get(0).checkIns()).isEqualTo(3);
            assertThat(top.get(1).city().name()).isEqualTo("New York");
            assertThat(top.get(1).checkIns()).isEqualTo(2);
        }
    }

    @Nested
    @DisplayName("processCheckInsConcurrent")
    class ConcurrentTest {

        @Test
        @DisplayName("並行処理でトップ都市を計算")
        void processesConcurrently() {
            List<City> checkIns = biasedSampleCheckIns();

            List<CityStats> top = processCheckInsConcurrent(checkIns, 3).unsafeRun();

            assertThat(top).hasSize(3);
            assertThat(top.get(0).city().name()).isEqualTo("Tokyo");
            assertThat(top.get(0).checkIns()).isEqualTo(5);
        }

        @Test
        @DisplayName("大量のチェックインを処理")
        void handlesLargeVolume() {
            List<City> checkIns = sampleCheckIns(100);

            List<CityStats> top = processCheckInsConcurrent(checkIns, 3).unsafeRun();

            assertThat(top).hasSize(3);
            // 各都市が100回ずつチェックインされる
            assertThat(top.get(0).checkIns()).isEqualTo(100);
        }
    }

    @Nested
    @DisplayName("storeCheckIn")
    class StoreCheckInTest {

        @Test
        @DisplayName("Ref にチェックインを保存")
        void storesInRef() {
            Ref<Map<City, Integer>> ref = Ref.unsafe(HashMap.empty());
            City tokyo = new City("Tokyo");

            storeCheckIn(ref, tokyo).unsafeRun();
            storeCheckIn(ref, tokyo).unsafeRun();
            storeCheckIn(ref, new City("London")).unsafeRun();

            Map<City, Integer> result = ref.unsafeGet();
            assertThat(result.get(tokyo).get()).isEqualTo(2);
            assertThat(result.get(new City("London")).get()).isEqualTo(1);
        }
    }

    @Nested
    @DisplayName("updateRanking")
    class UpdateRankingTest {

        @Test
        @DisplayName("ランキングを更新")
        void updatesRanking() {
            Ref<Map<City, Integer>> checkIns = Ref.unsafe(HashMap.of(
                    new City("A"), 10,
                    new City("B"), 20,
                    new City("C"), 5
            ));
            Ref<List<CityStats>> ranking = Ref.unsafe(List.empty());

            updateRanking(checkIns, ranking, 2).unsafeRun();

            List<CityStats> result = ranking.unsafeGet();
            assertThat(result).hasSize(2);
            assertThat(result.get(0).city().name()).isEqualTo("B");
            assertThat(result.get(1).city().name()).isEqualTo("A");
        }
    }

    @Nested
    @DisplayName("sampleCheckIns")
    class SampleDataTest {

        @Test
        @DisplayName("サンプルデータを生成")
        void generatesSampleData() {
            List<City> checkIns = sampleCheckIns(3);

            // 5都市 * 3回 = 15
            assertThat(checkIns).hasSize(15);
        }

        @Test
        @DisplayName("偏りのあるサンプルデータ")
        void generatesBiasedData() {
            List<City> checkIns = biasedSampleCheckIns();

            long tokyoCount = checkIns.filter(c -> c.name().equals("Tokyo")).size();
            assertThat(tokyoCount).isEqualTo(5);
        }
    }
}
