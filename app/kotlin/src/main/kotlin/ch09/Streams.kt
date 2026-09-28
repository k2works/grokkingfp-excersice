package ch09

import ch08.castTheDie
import ch08.castTheDieImpure
import kotlinx.coroutines.delay
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.drop
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.flow.fold
import kotlinx.coroutines.flow.scan
import kotlinx.coroutines.flow.take
import kotlinx.coroutines.flow.toList
import kotlinx.coroutines.flow.transformWhile
import kotlin.time.Duration

/**
 * 第9章: ストリーム処理の基本
 *
 * Sequence（同期・遅延）と Flow（非同期・コールド）の 2 種類のストリームを扱う。
 */

// ============================================
// Sequence: 同期・遅延評価のストリーム
// ============================================

val numbers: Sequence<Int> = sequenceOf(1, 2, 3)

/** 自然数の無限ストリーム */
val naturals: Sequence<Int> = generateSequence(1) { it + 1 }

/** Sequence を無限に繰り返す（空なら空のまま） */
fun <A> Sequence<A>.repeat(): Sequence<A> {
    val source = this
    return sequence {
        if (source.none()) return@sequence
        while (true) yieldAll(source)
    }
}

/** サイコロの目の無限ストリーム（評価のたびに副作用が起きる） */
fun dieCasts(): Sequence<Int> = generateSequence { castTheDieImpure() }

// ============================================
// Flow: 非同期・コールドなストリーム
// ============================================

/** suspend 関数 castTheDie を無限に呼び出すストリーム */
fun infiniteDieCasts(): Flow<Int> = flow {
    while (true) emit(castTheDie())
}

suspend fun firstThreeCasts(): List<Int> = infiniteDieCasts().take(3).toList()

/** 6 が出るまで振り続け、6 を含めた目を返す */
suspend fun castUntilSix(): List<Int> =
    infiniteDieCasts().transformWhile { value ->
        emit(value)
        value != 6
    }.toList()

suspend fun sumOfFirstThree(): Int = infiniteDieCasts().take(3).fold(0) { acc, value -> acc + value }

/** 累積和のストリーム（初期値 0 は捨てる） */
fun runningTotals(values: Flow<Int>): Flow<Int> = values.scan(0) { acc, value -> acc + value }.drop(1)

/** Flow のスライディングウィンドウ（標準ライブラリにはないので自作する） */
fun <A> Flow<A>.windowed(size: Int): Flow<List<A>> {
    require(size > 0) { "size must be positive: $size" }
    val upstream = this
    return flow {
        val window = ArrayDeque<A>(size)
        upstream.collect { value ->
            window.addLast(value)
            if (window.size > size) window.removeFirst()
            if (window.size == size) emit(window.toList())
        }
    }
}

/** 一定間隔で Unit を流す無限ストリーム */
fun ticks(period: Duration): Flow<Unit> = flow {
    while (true) {
        delay(period)
        emit(Unit)
    }
}
