@file:OptIn(ExperimentalCoroutinesApi::class)

package ch09

import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.collections.shouldHaveSize
import io.kotest.matchers.ints.shouldBeInRange
import io.kotest.matchers.shouldBe
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.flow.flowOf
import kotlinx.coroutines.flow.map
import kotlinx.coroutines.flow.retry
import kotlinx.coroutines.flow.scan
import kotlinx.coroutines.flow.take
import kotlinx.coroutines.flow.toList
import kotlinx.coroutines.flow.zip
import kotlinx.coroutines.test.currentTime
import kotlinx.coroutines.test.runTest
import kotlin.time.Duration.Companion.seconds

class StreamsTest : FunSpec({

    context("Sequence による遅延評価") {
        test("有限の Sequence を List に変換できる") {
            numbers.toList() shouldBe listOf(1, 2, 3)
            numbers.filter { it % 2 != 0 }.toList() shouldBe listOf(1, 3)
        }

        test("repeat で無限に繰り返し、take で必要な分だけ取り出す") {
            numbers.repeat().take(8).toList() shouldBe listOf(1, 2, 3, 1, 2, 3, 1, 2)
        }

        test("空の Sequence の repeat は空になる") {
            emptySequence<Int>().repeat().take(3).toList() shouldBe emptyList()
        }

        test("naturals は 1 から始まる無限の自然数") {
            naturals.take(5).toList() shouldBe listOf(1, 2, 3, 4, 5)
            naturals.filter { it % 2 == 0 }.take(3).toList() shouldBe listOf(2, 4, 6)
        }

        test("Sequence は必要な要素だけを評価する") {
            val evaluated = mutableListOf<Int>()
            val result = naturals
                .onEach { evaluated.add(it) }
                .map { it * 10 }
                .take(3)
                .toList()
            result shouldBe listOf(10, 20, 30)
            evaluated shouldBe listOf(1, 2, 3)
        }

        test("windowed でスライディングウィンドウを作れる") {
            sequenceOf(1, 2, 3, 4, 5).windowed(3).toList() shouldBe listOf(
                listOf(1, 2, 3),
                listOf(2, 3, 4),
                listOf(3, 4, 5),
            )
        }

        test("演習: 偶数・交互の真偽値・先頭 5 要素の合計") {
            val evens = (1..10).asSequence().filter { it % 2 == 0 }
            evens.toList() shouldBe listOf(2, 4, 6, 8, 10)
            val alternating = sequenceOf(true, false).repeat()
            alternating.take(5).toList() shouldBe listOf(true, false, true, false, true)
            val sum = (1..10).asSequence().take(5).sum()
            sum shouldBe 15
        }

        test("dieCasts は無限のサイコロの目を生成する") {
            dieCasts().take(20).toList().forEach { it shouldBeInRange 1..6 }
        }
    }

    context("Flow によるコールドストリーム") {
        test("infiniteDieCasts から必要な分だけ取り出せる") {
            infiniteDieCasts().take(3).toList() shouldHaveSize 3
        }

        test("firstThreeCasts は 3 回分の目を返す") {
            val casts = firstThreeCasts()
            casts shouldHaveSize 3
            casts.forEach { it shouldBeInRange 1..6 }
        }

        test("castUntilSix は 6 が出るまでの目を返し、最後は 6") {
            val casts = castUntilSix()
            casts.last() shouldBe 6
            casts.dropLast(1).forEach { it shouldBeInRange 1..5 }
        }

        test("sumOfFirstThree は 3 から 18 の値を返す") {
            sumOfFirstThree() shouldBeInRange 3..18
        }

        test("Flow はコールドなので collect するたびに最初から実行される") {
            var calls = 0
            val stream = flow {
                calls += 1
                emit(calls)
            }
            stream.toList() shouldBe listOf(1)
            stream.toList() shouldBe listOf(2)
            calls shouldBe 2
        }

        test("map / zip / scan で変換・結合・累積ができる") {
            flowOf(1, 2, 3).map { it * 2 }.toList() shouldBe listOf(2, 4, 6)
            flowOf(1, 2, 3).zip(flowOf("a", "b", "c")) { n, s -> "$n$s" }.toList() shouldBe
                listOf("1a", "2b", "3c")
            flowOf(1, 2, 3).scan(0) { acc, n -> acc + n }.toList() shouldBe listOf(0, 1, 3, 6)
        }

        test("runningTotals は累積和のストリームを返す") {
            runningTotals(flowOf(1, 2, 3, 4)).toList() shouldBe listOf(1, 3, 6, 10)
        }

        test("Flow の windowed はスライディングウィンドウを作る") {
            flowOf(1, 2, 3, 4, 5).windowed(3).toList() shouldBe listOf(
                listOf(1, 2, 3),
                listOf(2, 3, 4),
                listOf(3, 4, 5),
            )
        }

        test("要素数がウィンドウサイズ未満なら何も出力しない") {
            flowOf(1, 2).windowed(3).toList() shouldBe emptyList()
        }

        test("windowed のサイズは正でなければならない") {
            shouldThrow<IllegalArgumentException> { flowOf(1).windowed(0) }
        }

        test("Flow の retry は失敗した上流を最初からやり直す") {
            var attempts = 0
            val flaky = flow {
                attempts += 1
                if (attempts < 3) throw RuntimeException("Connection error")
                emit(attempts)
            }
            flaky.retry(5).toList() shouldBe listOf(3)
        }

        test("ticks と zip すると一定間隔で要素が流れる（仮想時間）") {
            runTest {
                val result = flowOf("a", "b", "c").zip(ticks(1.seconds)) { value, _ -> value }.toList()
                result shouldBe listOf("a", "b", "c")
                currentTime shouldBe 3_000
            }
        }
    }
})
