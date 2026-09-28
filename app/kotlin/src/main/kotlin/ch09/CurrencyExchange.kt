package ch09

import ch08.MAX_RETRIES
import ch08.retry
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.filter
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.mapNotNull
import kotlinx.coroutines.flow.zip
import java.math.BigDecimal
import java.math.RoundingMode
import kotlin.random.Random
import kotlin.time.Duration
import kotlin.time.Duration.Companion.seconds

/**
 * 第9章: 通貨交換レートの例
 *
 * 為替レートのストリームを監視し、上昇トレンドを検出したら交換する。
 */

@JvmInline
value class Currency(val name: String)

const val TREND_WINDOW_SIZE = 3
private const val API_FAILURE_RATE = 0.25

// ============================================
// 外部 API のシミュレーション（不純・ランダムに失敗する）
// ============================================

fun exchangeRatesTableApiCall(currency: String): Map<String, BigDecimal> {
    if (Random.nextDouble() < API_FAILURE_RATE) throw RuntimeException("Connection error")
    if (currency != "USD") throw RuntimeException("Rate not available")
    return mapOf(
        "EUR" to fluctuate(0.81, 0.05),
        "JPY" to fluctuate(103.25, 5.0),
    )
}

private fun fluctuate(base: Double, spread: Double): BigDecimal =
    BigDecimal.valueOf(base + (Random.nextDouble() - 0.5) * 2 * spread).setScale(2, RoundingMode.FLOOR)

// ============================================
// 純粋関数
// ============================================

/** 連続して上昇していれば true */
fun trending(rates: List<BigDecimal>): Boolean =
    rates.size > 1 && rates.zipWithNext().all { (previous, rate) -> rate > previous }

/** 連続して下降していれば true */
fun trendingDown(rates: List<BigDecimal>): Boolean =
    rates.size > 1 && rates.zipWithNext().all { (previous, rate) -> rate < previous }

/** 3 つ以上の値がすべて同じなら true */
fun isStable(values: List<BigDecimal>): Boolean = values.size >= 3 && values.distinct().size == 1

/** レート表から 1 つの通貨のレートを取り出す（カリー化） */
fun extractSingleCurrencyRate(currencyToExtract: Currency): (Map<Currency, BigDecimal>) -> BigDecimal? =
    { table -> table[currencyToExtract] }

/** レートの List からトレンドを検出する（Sequence の windowed を使う） */
fun findTrendInRates(rates: List<BigDecimal>, windowSize: Int): BigDecimal? =
    rates.asSequence()
        .windowed(windowSize)
        .firstOrNull(::trending)
        ?.last()

// ============================================
// suspend 関数による API 呼び出し
// ============================================

suspend fun exchangeTable(from: Currency): Map<Currency, BigDecimal> =
    exchangeRatesTableApiCall(from.name).mapKeys { (name, _) -> Currency(name) }

/** Version 1: 直近 3 回分のレートを取得する */
suspend fun lastRates(from: Currency, to: Currency): List<BigDecimal> =
    List(TREND_WINDOW_SIZE) { retry(MAX_RETRIES) { exchangeTable(from) } }
        .mapNotNull(extractSingleCurrencyRate(to))

/** Version 1: 1 回だけ判断する（トレンドがなければ null） */
suspend fun exchangeIfTrendingOnce(amount: BigDecimal, from: Currency, to: Currency): BigDecimal? {
    val rates = lastRates(from, to)
    return if (trending(rates)) amount * rates.last() else null
}

// ============================================
// Flow によるストリーム処理
// ============================================

/** 為替レートの無限ストリーム */
fun rates(from: Currency, to: Currency): Flow<BigDecimal> =
    flow { while (true) emit(retry(MAX_RETRIES) { exchangeTable(from) }) }
        .mapNotNull(extractSingleCurrencyRate(to))

/** ストリームから最初の上昇トレンドの最新レートを取り出す */
suspend fun firstTrendingRate(rates: Flow<BigDecimal>, windowSize: Int = TREND_WINDOW_SIZE): BigDecimal =
    rates.windowed(windowSize)
        .filter(::trending)
        .map { it.last() }
        .first()

suspend fun exchangeIfTrending(amount: BigDecimal, from: Currency, to: Currency): BigDecimal =
    firstTrendingRate(rates(from, to)) * amount

/** ticks と zip して、一定間隔でレートを取得する */
suspend fun exchangeIfTrendingWithTicks(
    amount: BigDecimal,
    from: Currency,
    to: Currency,
    period: Duration = 1.seconds,
): BigDecimal {
    val ratesWithDelay = rates(from, to).zip(ticks(period)) { rate, _ -> rate }
    return firstTrendingRate(ratesWithDelay) * amount
}
