@file:OptIn(ExperimentalCoroutinesApi::class)

package ch10

import arrow.atomic.Atomic
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.collections.shouldBeEmpty
import io.kotest.matchers.shouldBe
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.flow.onEach
import kotlinx.coroutines.test.advanceTimeBy
import kotlinx.coroutines.test.runCurrent
import kotlinx.coroutines.test.runTest
import kotlin.time.Duration.Companion.milliseconds

class CheckInsTest : FunSpec({

    val sydney = City("Sydney")
    val dublin = City("Dublin")
    val lima = City("Lima")

    context("純粋関数") {
        test("sampleCheckIns は 5 都市を指定回数だけ繰り返す") {
            val checkIns = sampleCheckIns(3)
            checkIns.size shouldBe 15
            checkIns.distinct().size shouldBe 5
        }

        test("updateCheckIns は初回のチェックインを 1 として記録する") {
            updateCheckIns(emptyMap(), sydney) shouldBe mapOf(sydney to 1)
        }

        test("updateCheckIns は既存のチェックイン数を 1 増やす") {
            updateCheckIns(mapOf(sydney to 2), sydney) shouldBe mapOf(sydney to 3)
        }

        test("updateCheckIns は元の Map を変更しない") {
            val original = mapOf(sydney to 1)
            updateCheckIns(original, sydney)
            original shouldBe mapOf(sydney to 1)
        }

        test("topCities はチェックイン数の降順で上位 N 件を返す") {
            val checkIns = mapOf(sydney to 2, dublin to 5, lima to 3)
            topCities(checkIns, 2) shouldBe listOf(CityStats(dublin, 5), CityStats(lima, 3))
        }

        test("topCities は同数の場合に都市名の昇順で並べる") {
            val checkIns = mapOf(sydney to 1, dublin to 1, lima to 1)
            topCities(checkIns).map { it.city } shouldBe listOf(dublin, lima, sydney)
        }

        test("topCities は空の Map に対して空リストを返す") {
            topCities(emptyMap()).shouldBeEmpty()
        }
    }

    context("逐次処理") {
        test("processCheckInsSequential は fold でランキングを計算する") {
            val checkIns = listOf(sydney, dublin, sydney, lima, sydney, dublin)
            processCheckInsSequential(checkIns, 2) shouldBe
                listOf(CityStats(sydney, 3), CityStats(dublin, 2))
        }
    }

    context("アトミックな共有状態") {
        test("storeCheckIn は Atomic に保存されたチェックイン数を更新する") {
            val stored = Atomic(emptyMap<City, Int>())
            storeCheckIn(stored, sydney)
            storeCheckIn(stored, sydney)
            storeCheckIn(stored, lima)
            stored.get() shouldBe mapOf(sydney to 2, lima to 1)
        }

        test("processCheckInsConcurrent は複数スレッドから更新しても数え漏れがない") {
            runTest {
                val result = processCheckInsConcurrent(sampleCheckIns(2_000), 5)
                result.map { it.checkIns } shouldBe List(5) { 2_000 }
            }
        }
    }

    context("ランキングの継続的な更新") {
        test("processCheckIns はチェックインの完了後に最終ランキングを返す") {
            runTest {
                val checkIns = listOf(sydney, dublin, sydney, lima, sydney)
                    .asFlow()
                    .onEach { delay(10.milliseconds) }
                processCheckIns(checkIns, 2, 25.milliseconds) shouldBe
                    listOf(CityStats(sydney, 3), CityStats(dublin, 1))
            }
        }
    }

    context("呼び出し元に制御を返す") {
        test("startProcessingCheckIns はすぐに制御を返し、ランキングは時間とともに更新される") {
            runTest {
                val checkIns = sampleCheckIns(10).asFlow().onEach { delay(10.milliseconds) }
                val processing = startProcessingCheckIns(checkIns, 3, 100.milliseconds)

                processing.currentRanking().shouldBeEmpty()

                advanceTimeBy(150.milliseconds)
                runCurrent()
                val early = processing.currentRanking()
                early.size shouldBe 3

                advanceTimeBy(1_000.milliseconds)
                runCurrent()
                processing.currentRanking().map { it.checkIns } shouldBe listOf(10, 10, 10)

                processing.stop()
                processing.isActive shouldBe false
            }
        }

        test("stop の後はランキングが更新されない") {
            runTest {
                val checkIns = sampleCheckIns(100).asFlow().onEach { delay(10.milliseconds) }
                val processing = startProcessingCheckIns(checkIns, 1, 50.milliseconds)

                advanceTimeBy(210.milliseconds)
                runCurrent()
                val beforeStop = processing.currentRanking()
                processing.stop()

                advanceTimeBy(1_000.milliseconds)
                runCurrent()
                processing.currentRanking() shouldBe beforeStop
            }
        }
    }
})
