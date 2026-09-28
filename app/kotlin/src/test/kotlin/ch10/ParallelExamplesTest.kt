@file:OptIn(ExperimentalCoroutinesApi::class)

package ch10

import arrow.core.Either
import arrow.fx.coroutines.raceN
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.collections.shouldContainExactlyInAnyOrder
import io.kotest.matchers.shouldBe
import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.awaitCancellation
import kotlinx.coroutines.cancelAndJoin
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.delay
import kotlinx.coroutines.test.advanceTimeBy
import kotlinx.coroutines.test.currentTime
import kotlinx.coroutines.test.runCurrent
import kotlinx.coroutines.test.runTest
import java.util.concurrent.atomic.AtomicInteger
import kotlin.coroutines.EmptyCoroutineContext
import kotlin.time.Duration.Companion.milliseconds

class ParallelExamplesTest : FunSpec({

    // 決定論的な「サイコロ」: 呼ばれるたびに 1, 2, 3, ... を返す
    fun fakeDie(): suspend () -> Int {
        val counter = AtomicInteger(0)
        return { counter.incrementAndGet() }
    }

    fun delayed(millis: Long, value: Int): suspend () -> Int = {
        delay(millis)
        value
    }

    context("構造化並行性") {
        test("castTheDieTwice は 2 つのサイコロを並行して振り合計を返す") {
            runTest {
                castTheDieTwice(fakeDie()) shouldBe 3
            }
        }

        test("sumSequential は処理時間の合計だけかかる") {
            runTest {
                val ios = listOf(delayed(100, 1), delayed(100, 2), delayed(100, 3))
                sumSequential(ios) shouldBe 6
                currentTime shouldBe 300
            }
        }

        test("sumConcurrent は最も遅い処理の時間だけで終わる") {
            runTest {
                val ios = listOf(delayed(100, 1), delayed(200, 2), delayed(100, 3))
                sumConcurrent(ios) shouldBe 6
                currentTime shouldBe 200
            }
        }

        test("coroutineScope 内の 1 つが失敗すると兄弟のコルーチンもキャンセルされる") {
            runTest {
                val siblingCancelled = CompletableDeferred<Boolean>()
                val sibling: suspend () -> Int = {
                    try {
                        awaitCancellation()
                    } finally {
                        siblingCancelled.complete(true)
                    }
                }
                val failing: suspend () -> Int = {
                    delay(50)
                    error("boom")
                }
                shouldThrow<IllegalStateException> { sumConcurrent(listOf(sibling, failing)) }
                siblingCancelled.await() shouldBe true
            }
        }
    }

    context("Atomic と並列実行の組み合わせ") {
        test("castTheDieAndStore は結果を Atomic に保存して返す") {
            runTest {
                castTheDieAndStore(3, fakeDie()) shouldContainExactlyInAnyOrder listOf(1, 2, 3)
            }
        }
    }

    context("parMap と parZip") {
        test("parSequence は suspend 関数のリストを並列実行して結果を順序通りに返す") {
            runTest {
                val ios = listOf(delayed(300, 1), delayed(100, 2), delayed(200, 3))
                ios.parSequence() shouldBe listOf(1, 2, 3)
                currentTime shouldBe 300
            }
        }

        test("parTraverse は各要素に関数を適用して並列実行する") {
            runTest {
                listOf(1, 2, 3, 4, 5).parTraverse { n ->
                    delay(100)
                    n * 2
                } shouldBe listOf(2, 4, 6, 8, 10)
                currentTime shouldBe 100
            }
        }

        test("parMap の concurrency で同時実行数を制限できる") {
            runTest {
                val result = fetchAllLimited(listOf(1, 2, 3, 4, 5, 6), concurrency = 2) { n ->
                    delay(100)
                    n
                }
                result shouldBe listOf(1, 2, 3, 4, 5, 6)
                currentTime shouldBe 300
            }
        }

        test("fetchProfile は parZip で 2 つの取得処理を並列に行い結果を組み合わせる") {
            runTest {
                val profile = fetchProfile(
                    fetchName = { delay(100); "Alice" },
                    fetchScore = { delay(150); 42 },
                )
                profile shouldBe Profile("Alice", 42)
                currentTime shouldBe 150
            }
        }

        test("parFold は並列実行した結果を畳み込む") {
            runTest {
                val ios = listOf(delayed(10, 1), delayed(20, 2), delayed(30, 3), delayed(40, 4))
                parFold(ios, 0) { acc, n -> acc + n } shouldBe 10
            }
        }
    }

    context("race と timeout") {
        test("raceN は先に完了した側を Either で返す") {
            runTest {
                // context を省略すると Dispatchers.Default で実行されるため、仮想時間を使うには明示する
                val result = raceN(EmptyCoroutineContext, { delay(200); "slow" }, { delay(50); 42 })
                result shouldBe Either.Right(42)
                currentTime shouldBe 50
            }
        }

        test("race は同じ型の 2 つの処理のうち速い方の結果を返す") {
            runTest {
                race(delayed(200, 1), delayed(50, 2)) shouldBe 2
            }
        }

        test("race で負けた処理はキャンセルされる") {
            runTest {
                val loserCancelled = CompletableDeferred<Boolean>()
                val loser: suspend () -> Int = {
                    try {
                        delay(1_000)
                        1
                    } finally {
                        loserCancelled.complete(true)
                    }
                }
                race(loser, delayed(10, 2)) shouldBe 2
                loserCancelled.await() shouldBe true
            }
        }

        test("timeoutOrNull は時間内に完了すれば結果を返す") {
            runTest {
                timeoutOrNull(200.milliseconds, delayed(50, 42)) shouldBe 42
            }
        }

        test("timeoutOrNull は時間切れなら null を返す") {
            runTest {
                timeoutOrNull(100.milliseconds, delayed(500, 42)) shouldBe null
                currentTime shouldBe 100
            }
        }
    }

    context("バックグラウンド実行とキャンセル") {
        test("repeatForever で起動した Job はキャンセルするまで繰り返し実行される") {
            runTest {
                val counter = AtomicInteger(0)
                val job = repeatForever(100.milliseconds) { counter.incrementAndGet() }

                advanceTimeBy(350.milliseconds)
                counter.get() shouldBe 3

                job.cancel()
                advanceTimeBy(1_000.milliseconds)
                counter.get() shouldBe 3
                job.isCancelled shouldBe true
            }
        }

        test("cancelAndJoin はキャンセルした Job の終了まで待つ") {
            runTest {
                val counter = AtomicInteger(0)
                val job = repeatForever(10.milliseconds) { counter.incrementAndGet() }
                advanceTimeBy(55.milliseconds)
                runCurrent()
                job.cancelAndJoin()
                job.isCompleted shouldBe true
                counter.get() shouldBe 5
            }
        }
    }

    context("演習問題") {
        test("incrementThreeTimes は 3 を返す") {
            runTest {
                incrementThreeTimes() shouldBe 3
            }
        }

        test("sumParallel は 3 つの処理を並列実行して合計する") {
            runTest {
                sumParallel(delayed(100, 1), delayed(100, 2), delayed(100, 3)) shouldBe 6
                currentTime shouldBe 100
            }
        }

        test("countEvens は偶数を返した処理の数を数える") {
            runTest {
                val ios = (0 until 100).map { n -> suspend { n } }
                countEvens(ios) shouldBe 50
            }
        }

        test("collectFor は指定時間だけ値を集めてから停止する") {
            runTest {
                val die = fakeDie()
                val collected = collectFor(1_050.milliseconds, 100.milliseconds, die)
                collected shouldBe (1..10).toList()
                currentTime shouldBe 1_050
            }
        }

        test("collectFor の結果の件数は期間と間隔から決まる") {
            runTest {
                collectFor(550.milliseconds, 100.milliseconds, fakeDie()).size shouldBe 5
            }
        }

        test("applyUpdates は異なるキーへの更新をすべて反映する") {
            runTest {
                val updates = listOf(Update("a", 1), Update("b", 2), Update("c", 4))
                applyUpdates(updates) shouldBe mapOf("a" to 1, "b" to 2, "c" to 4)
            }
        }
    }
})
