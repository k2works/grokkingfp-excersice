@file:OptIn(ExperimentalCoroutinesApi::class)

package ch09

import ch08.retry
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.collections.shouldHaveSize
import io.kotest.matchers.comparables.shouldBeGreaterThan
import io.kotest.matchers.longs.shouldBeGreaterThanOrEqual
import io.kotest.matchers.shouldBe
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.flow.flowOf
import kotlinx.coroutines.flow.take
import kotlinx.coroutines.flow.toList
import kotlinx.coroutines.test.currentTime
import kotlinx.coroutines.test.runTest
import java.math.BigDecimal
import kotlin.time.Duration.Companion.seconds

private fun rates(vararg values: String): List<BigDecimal> = values.map(::BigDecimal)

class CurrencyExchangeTest : FunSpec({

    val usd = Currency("USD")
    val eur = Currency("EUR")
    val jpy = Currency("JPY")

    context("イミュータブルな Map") {
        test("plus で新しい Map を作り、元の Map は変わらない") {
            val usdRates = mapOf(eur to BigDecimal("0.82"))
            val updated = usdRates + (jpy to BigDecimal("103.91"))
            updated shouldBe mapOf(eur to BigDecimal("0.82"), jpy to BigDecimal("103.91"))
            usdRates shouldBe mapOf(eur to BigDecimal("0.82"))
        }

        test("minus でキーを取り除いた新しい Map を作る") {
            val usdRates = mapOf(eur to BigDecimal("0.82"))
            usdRates - eur shouldBe emptyMap()
            usdRates - jpy shouldBe usdRates
        }
    }

    context("trending") {
        test("連続して上昇していれば true") {
            trending(rates("0.81", "0.82", "0.83")) shouldBe true
            trending(rates("1", "2", "3", "8")) shouldBe true
        }

        test("途中で下降していれば false") {
            trending(rates("0.81", "0.84", "0.83")) shouldBe false
            trending(rates("1", "2", "9", "8")) shouldBe false
        }

        test("要素が 1 つ以下なら false") {
            trending(emptyList()) shouldBe false
            trending(rates("0.81")) shouldBe false
        }

        test("同じ値が続く場合は上昇ではない") {
            trending(rates("0.81", "0.81", "0.82")) shouldBe false
        }
    }

    context("trendingDown と isStable") {
        test("trendingDown は連続して下降していれば true") {
            trendingDown(rates("0.83", "0.82", "0.81")) shouldBe true
            trendingDown(rates("0.83", "0.84", "0.81")) shouldBe false
            trendingDown(rates("0.83")) shouldBe false
        }

        test("isStable は 3 つ以上の値が全て同じなら true") {
            isStable(rates("5", "5", "5")) shouldBe true
            isStable(rates("5", "5", "6")) shouldBe false
            isStable(rates("5", "6", "5")) shouldBe false
            isStable(rates("5")) shouldBe false
        }
    }

    context("extractSingleCurrencyRate") {
        val usdExchangeTables = listOf(
            mapOf(eur to BigDecimal("0.88")),
            mapOf(eur to BigDecimal("0.89"), jpy to BigDecimal("114.62")),
            mapOf(jpy to BigDecimal("114")),
        )

        test("指定した通貨のレートを取り出し、なければ null") {
            usdExchangeTables.map(extractSingleCurrencyRate(eur)) shouldBe
                listOf(BigDecimal("0.88"), BigDecimal("0.89"), null)
            usdExchangeTables.map(extractSingleCurrencyRate(jpy)) shouldBe
                listOf(null, BigDecimal("114.62"), BigDecimal("114"))
        }

        test("mapNotNull で null を取り除ける") {
            usdExchangeTables.mapNotNull(extractSingleCurrencyRate(eur)) shouldBe
                listOf(BigDecimal("0.88"), BigDecimal("0.89"))
        }
    }

    context("suspend 関数による API 呼び出し") {
        test("exchangeTable は USD のレート表を返す") {
            val table = retry(10) { exchangeTable(usd) }
            table.keys shouldBe setOf(eur, jpy)
        }

        test("lastRates は直近 3 回分のレートを返す") {
            lastRates(usd, eur) shouldHaveSize 3
        }

        test("exchangeIfTrendingOnce はトレンドがなければ null を返しうる") {
            val result = exchangeIfTrendingOnce(BigDecimal(1000), usd, eur)
            if (result != null) result shouldBeGreaterThan BigDecimal(750)
        }
    }

    context("ストリームによるトレンド検出") {
        test("findTrendInRates はウィンドウ内で上昇トレンドになった最新レートを返す") {
            findTrendInRates(rates("0.81", "0.80", "0.82", "0.83", "0.84"), 3) shouldBe BigDecimal("0.83")
            findTrendInRates(rates("0.81", "0.80", "0.79"), 3) shouldBe null
        }

        test("firstTrendingRate は Flow から最初のトレンドを見つける") {
            val stream = flowOf(*rates("0.81", "0.80", "0.82", "0.83", "0.84").toTypedArray())
            firstTrendingRate(stream) shouldBe BigDecimal("0.83")
        }

        test("rates は無限のレートストリームから必要な分だけ取り出せる") {
            rates(usd, eur).take(3).toList() shouldHaveSize 3
        }

        test("exchangeIfTrending はトレンドを検出したレートで交換する") {
            exchangeIfTrending(BigDecimal(1000), usd, eur) shouldBeGreaterThan BigDecimal(750)
        }

        test("exchangeIfTrendingWithTicks は一定間隔でレートを取得する（仮想時間）") {
            runTest {
                val result = exchangeIfTrendingWithTicks(BigDecimal(1000), usd, eur, 1.seconds)
                result shouldBeGreaterThan BigDecimal(750)
                currentTime shouldBeGreaterThanOrEqual 3_000
                currentTime % 1_000 shouldBe 0
            }
        }
    }
})
