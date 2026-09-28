package ch10

import arrow.atomic.Atomic
import arrow.atomic.update
import arrow.fx.coroutines.parMap
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.cancelAndJoin
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.launch
import kotlin.coroutines.CoroutineContext
import kotlin.time.Duration

/**
 * 第10章: チェックインのリアルタイム集計
 *
 * 都市へのチェックインを集計し、ランキングを更新する並行処理の例です。
 */

// ============================================
// データ型
// ============================================

data class City(val name: String)

data class CityStats(val city: City, val checkIns: Int)

private val sampleCities = listOf(
    City("Sydney"),
    City("Dublin"),
    City("Cape Town"),
    City("Lima"),
    City("Singapore"),
)

/** サンプルのチェックインデータ（5 都市を repeatCount 回繰り返す） */
fun sampleCheckIns(repeatCount: Int): List<City> =
    List(repeatCount) { sampleCities }.flatten()

// ============================================
// 純粋関数
// ============================================

/** チェックイン数の降順（同数なら都市名の昇順）で上位 n 件を返す */
fun topCities(cityCheckIns: Map<City, Int>, n: Int = 3): List<CityStats> =
    cityCheckIns
        .map { (city, checkIns) -> CityStats(city, checkIns) }
        .sortedWith(compareByDescending<CityStats> { it.checkIns }.thenBy { it.city.name })
        .take(n)

/** チェックインを 1 件反映した新しい Map を返す（元の Map は変更しない） */
fun updateCheckIns(current: Map<City, Int>, city: City): Map<City, Int> =
    current + (city to (current[city] ?: 0) + 1)

// ============================================
// 逐次処理版
// ============================================

/** チェックインを逐次処理する（シンプルだが並行性はない） */
fun processCheckInsSequential(checkIns: List<City>, topN: Int): List<CityStats> =
    topCities(checkIns.fold(emptyMap(), ::updateCheckIns), topN)

// ============================================
// 並行処理版（Atomic を使用）
// ============================================

/** Atomic に保存されたチェックイン数をアトミックに更新する */
fun storeCheckIn(storedCheckIns: Atomic<Map<City, Int>>, city: City) {
    storedCheckIns.update { current -> updateCheckIns(current, city) }
}

/** すべてのチェックインを並列に保存してからランキングを計算する */
suspend fun processCheckInsConcurrent(
    checkIns: List<City>,
    topN: Int,
    context: CoroutineContext = Dispatchers.Default,
): List<CityStats> {
    val storedCheckIns = Atomic(emptyMap<City, Int>())
    checkIns.parMap(context) { city -> storeCheckIn(storedCheckIns, city) }
    return topCities(storedCheckIns.get(), topN)
}

// ============================================
// ランキングの継続的な更新
// ============================================

/** 現在のチェックインからランキングを 1 回計算して保存する */
fun updateRankingOnce(
    storedCheckIns: Atomic<Map<City, Int>>,
    storedRanking: MutableStateFlow<List<CityStats>>,
    topN: Int,
) {
    storedRanking.value = topCities(storedCheckIns.get(), topN)
}

/** 一定間隔でランキングを更新し続ける（キャンセルされるまで終わらない） */
suspend fun updateRanking(
    storedCheckIns: Atomic<Map<City, Int>>,
    storedRanking: MutableStateFlow<List<CityStats>>,
    topN: Int,
    interval: Duration,
): Nothing {
    while (true) {
        updateRankingOnce(storedCheckIns, storedRanking, topN)
        delay(interval)
    }
}

/**
 * チェックインの保存とランキングの更新を並行して実行する。
 * チェックインの Flow が完了したらランキング更新を止め、最終ランキングを返す。
 */
suspend fun processCheckIns(
    checkIns: Flow<City>,
    topN: Int,
    interval: Duration,
): List<CityStats> = coroutineScope {
    val storedCheckIns = Atomic(emptyMap<City, Int>())
    val storedRanking = MutableStateFlow(emptyList<CityStats>())

    val rankingJob = launch { updateRanking(storedCheckIns, storedRanking, topN, interval) }
    checkIns.collect { city -> storeCheckIn(storedCheckIns, city) }

    rankingJob.cancelAndJoin()
    updateRankingOnce(storedCheckIns, storedRanking, topN)
    storedRanking.value
}

// ============================================
// 呼び出し元に制御を返す（Fiber 相当）
// ============================================

/** バックグラウンドで処理中のチェックイン */
class ProcessingCheckIns(
    private val ranking: StateFlow<List<CityStats>>,
    private val job: Job,
) {
    /** 現在のランキングを取得する */
    fun currentRanking(): List<CityStats> = ranking.value

    /** 処理を停止し、終了まで待つ */
    suspend fun stop() = job.cancelAndJoin()

    val isActive: Boolean get() = job.isActive
}

/**
 * チェックイン処理とランキング更新をバックグラウンドで起動し、すぐに制御を返す。
 * 起動したコルーチンは受け取った CoroutineScope の子になる。
 */
fun CoroutineScope.startProcessingCheckIns(
    checkIns: Flow<City>,
    topN: Int,
    interval: Duration,
): ProcessingCheckIns {
    val storedCheckIns = Atomic(emptyMap<City, Int>())
    val storedRanking = MutableStateFlow(emptyList<CityStats>())

    val job = launch {
        launch { updateRanking(storedCheckIns, storedRanking, topN, interval) }
        launch { checkIns.collect { city -> storeCheckIn(storedCheckIns, city) } }
    }
    return ProcessingCheckIns(storedRanking, job)
}
