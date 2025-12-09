package ch10;

import ch08.IO;
import io.vavr.collection.HashMap;
import io.vavr.collection.List;
import io.vavr.collection.Map;

/**
 * 第10章: チェックインのリアルタイム集計
 *
 * 都市へのチェックインをリアルタイムで集計し、
 * ランキングを更新する並行処理の例です。
 */
public final class CheckIns {

    private CheckIns() {}

    // ============================================
    // データ型
    // ============================================

    /**
     * 都市
     */
    public record City(String name) {}

    /**
     * 都市統計
     */
    public record CityStats(City city, int checkIns) {}

    /**
     * 処理中のチェックイン（呼び出し元に制御を返す）
     */
    public record ProcessingCheckIns(
            IO<List<CityStats>> currentRanking,
            IO<Void> stop
    ) {}

    // ============================================
    // 純粋関数
    // ============================================

    /**
     * チェックインマップからトップ N 都市を計算
     */
    public static List<CityStats> topCities(Map<City, Integer> cityCheckIns, int n) {
        return cityCheckIns.toList()
                .map(t -> new CityStats(t._1(), t._2()))
                .sortBy(stats -> -stats.checkIns())  // 降順
                .take(n);
    }

    /**
     * チェックインマップを更新
     */
    public static Map<City, Integer> updateCheckIns(Map<City, Integer> current, City city) {
        return current.put(city, current.get(city).map(c -> c + 1).getOrElse(1));
    }

    // ============================================
    // 逐次処理版
    // ============================================

    /**
     * チェックインを逐次処理（シンプルだが非効率）
     */
    public static IO<List<CityStats>> processCheckInsSequential(List<City> checkIns, int topN) {
        return IO.delay(() -> {
            Map<City, Integer> cityCheckIns = checkIns.foldLeft(
                    HashMap.<City, Integer>empty(),
                    CheckIns::updateCheckIns
            );
            return topCities(cityCheckIns, topN);
        });
    }

    // ============================================
    // 並行処理版（Ref を使用）
    // ============================================

    /**
     * チェックインを保存（Ref を使用）
     */
    public static IO<Void> storeCheckIn(Ref<Map<City, Integer>> storedCheckIns, City city) {
        return storedCheckIns.update(current -> updateCheckIns(current, city));
    }

    /**
     * ランキングを更新（Ref から読み取り、計算して別の Ref に保存）
     */
    public static IO<Void> updateRanking(
            Ref<Map<City, Integer>> storedCheckIns,
            Ref<List<CityStats>> storedRanking,
            int topN) {
        return storedCheckIns.get()
                .map(checkIns -> topCities(checkIns, topN))
                .flatMap(storedRanking::set);
    }

    /**
     * チェックインを並行処理（Ref を使用）
     */
    public static IO<List<CityStats>> processCheckInsConcurrent(List<City> checkIns, int topN) {
        return Ref.<Map<City, Integer>>of(HashMap.empty())
                .flatMap(storedCheckIns ->
                        // すべてのチェックインを並列に処理
                        ParallelIO.parTraverse(checkIns, city -> storeCheckIn(storedCheckIns, city))
                                .andThen(storedCheckIns.get())
                                .map(cityCheckIns -> topCities(cityCheckIns, topN))
                );
    }

    // ============================================
    // バックグラウンド処理版（Fiber を使用）
    // ============================================

    /**
     * チェックインをバックグラウンドで処理
     * 呼び出し元にすぐ制御を返し、後からランキングを取得できる
     */
    public static IO<ProcessingCheckIns> processCheckInsBackground(
            List<City> checkIns,
            int topN,
            long updateIntervalMillis) {

        return Ref.<Map<City, Integer>>of(HashMap.empty())
                .flatMap(storedCheckIns ->
                        Ref.<List<CityStats>>of(List.empty())
                                .flatMap(storedRanking -> {
                                    // チェックイン処理プログラム
                                    IO<Void> checkInsProgram = IO.traverse(
                                            checkIns,
                                            city -> storeCheckIn(storedCheckIns, city)
                                                    .andThen(Fiber.sleep(1)) // 少し遅延
                                    ).map(ignored -> null);

                                    // ランキング更新プログラム（定期的に実行）
                                    IO<Void> rankingProgram = IO.delay(() -> {
                                        updateRanking(storedCheckIns, storedRanking, topN).unsafeRun();
                                        return null;
                                    });

                                    // Fiber として起動
                                    return Fiber.start(checkInsProgram)
                                            .flatMap(checkInsFiber ->
                                                    Fiber.startForever(
                                                            Fiber.sleep(updateIntervalMillis)
                                                                    .andThen(rankingProgram)
                                                    ).map(rankingFiber ->
                                                            new ProcessingCheckIns(
                                                                    storedRanking.get(),
                                                                    checkInsFiber.cancel()
                                                                            .andThen(rankingFiber.cancel())
                                                            )
                                                    )
                                            );
                                })
                );
    }

    // ============================================
    // サンプルデータ
    // ============================================

    /**
     * サンプルのチェックインデータを生成
     */
    public static List<City> sampleCheckIns(int repeatCount) {
        List<City> cities = List.of(
                new City("Sydney"),
                new City("Dublin"),
                new City("Cape Town"),
                new City("Lima"),
                new City("Singapore")
        );

        return List.range(0, repeatCount)
                .flatMap(i -> cities);
    }

    /**
     * 偏りのあるサンプルデータを生成
     */
    public static List<City> biasedSampleCheckIns() {
        return List.of(
                new City("Tokyo"),
                new City("Tokyo"),
                new City("Tokyo"),
                new City("Tokyo"),
                new City("Tokyo"),
                new City("New York"),
                new City("New York"),
                new City("New York"),
                new City("London"),
                new City("London"),
                new City("Paris")
        );
    }
}
