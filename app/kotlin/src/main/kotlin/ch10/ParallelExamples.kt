package ch10

import arrow.atomic.Atomic
import arrow.atomic.AtomicInt
import arrow.atomic.update
import arrow.core.merge
import arrow.fx.coroutines.parMap
import arrow.fx.coroutines.parZip
import arrow.fx.coroutines.raceN
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Job
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.cancelAndJoin
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import kotlinx.coroutines.withTimeoutOrNull
import kotlin.coroutines.CoroutineContext
import kotlin.coroutines.EmptyCoroutineContext
import kotlin.time.Duration

/**
 * 第10章: 並行・並列処理の基本パターン
 *
 * 構造化並行性（coroutineScope / async / launch）と
 * Arrow Fx Coroutines（parMap / parZip / raceN）の使い方を示します。
 */

// ============================================
// 構造化並行性: coroutineScope と async / await
// ============================================

/** 2 つのサイコロを並行して振り、合計を返す */
suspend fun castTheDieTwice(castTheDie: suspend () -> Int): Int = coroutineScope {
    val first = async { castTheDie() }
    val second = async { castTheDie() }
    first.await() + second.await()
}

/** 1 つずつ順番に実行して合計する */
suspend fun sumSequential(ios: List<suspend () -> Int>): Int =
    ios.sumOf { io -> io() }

/** すべてを並行に実行して合計する */
suspend fun sumConcurrent(ios: List<suspend () -> Int>): Int = coroutineScope {
    ios.map { io -> async { io() } }.awaitAll().sum()
}

// ============================================
// Atomic と並列実行の組み合わせ
// ============================================

/** n 個のサイコロを並列に振り、結果を Atomic に保存して返す */
suspend fun castTheDieAndStore(n: Int, castTheDie: suspend () -> Int): List<Int> {
    val storedCasts = Atomic(emptyList<Int>())
    (1..n).parMap {
        val result = castTheDie()
        storedCasts.update { casts -> casts + result }
    }
    return storedCasts.get()
}

// ============================================
// parMap / parZip
// ============================================

/** Scala の parSequence に相当: suspend 関数のリストを並列実行する */
suspend fun <A> List<suspend () -> A>.parSequence(): List<A> =
    parMap { io -> io() }

/** Scala の parTraverse に相当: 各要素に関数を適用して並列実行する */
suspend fun <A, B> List<A>.parTraverse(f: suspend (A) -> B): List<B> =
    parMap { a -> f(a) }

/** 同時実行数を制限して並列に取得する */
suspend fun <A, B> fetchAllLimited(
    ids: List<A>,
    concurrency: Int,
    fetch: suspend (A) -> B,
): List<B> = ids.parMap(concurrency = concurrency) { id -> fetch(id) }

data class Profile(val name: String, val score: Int)

/**
 * 名前とスコアを並列に取得して Profile に組み立てる。
 * context を省略すると呼び出し元の Dispatcher を引き継ぐ。
 */
suspend fun fetchProfile(
    fetchName: suspend () -> String,
    fetchScore: suspend () -> Int,
    context: CoroutineContext = EmptyCoroutineContext,
): Profile = parZip(
    context,
    { fetchName() },
    { fetchScore() },
) { name, score -> Profile(name, score) }

/** 並列に実行した結果を畳み込む */
suspend fun <A, B> parFold(ios: List<suspend () -> A>, initial: B, f: (B, A) -> B): B =
    ios.parSequence().fold(initial, f)

// ============================================
// race と timeout
// ============================================

/** 同じ型の 2 つの処理を競争させ、先に完了した方の結果を返す */
suspend fun <A> race(
    first: suspend () -> A,
    second: suspend () -> A,
    context: CoroutineContext = EmptyCoroutineContext,
): A = raceN(context, { first() }, { second() }).merge()

/** 時間内に完了すれば結果を、時間切れなら null を返す */
suspend fun <A> timeoutOrNull(timeout: Duration, action: suspend () -> A): A? =
    withTimeoutOrNull(timeout) { action() }

// ============================================
// Job によるバックグラウンド実行（Fiber 相当）
// ============================================

/** interval ごとに action を繰り返す Job を起動し、すぐに返す */
fun CoroutineScope.repeatForever(interval: Duration, action: suspend () -> Unit): Job =
    launch {
        while (true) {
            delay(interval)
            action()
        }
    }

// ============================================
// 演習問題の解答
// ============================================

/** 問題 1: カウンターを 3 回インクリメントした結果を返す */
fun incrementThreeTimes(): Int {
    val counter = AtomicInt(0)
    repeat(3) { counter.update { it + 1 } }
    return counter.get()
}

/** 問題 2: 3 つの処理を並列実行して合計する */
suspend fun sumParallel(
    first: suspend () -> Int,
    second: suspend () -> Int,
    third: suspend () -> Int,
): Int = parFold(listOf(first, second, third), 0) { acc, n -> acc + n }

/** 問題 3: 並行に実行し、偶数を返した処理の数を数える */
suspend fun countEvens(ios: List<suspend () -> Int>): Int {
    val counter = AtomicInt(0)
    ios.parMap { io ->
        if (io() % 2 == 0) counter.incrementAndGet()
    }
    return counter.get()
}

/** 問題 4: interval ごとに値を集め、duration 経過後に停止して結果を返す */
suspend fun collectFor(
    duration: Duration,
    interval: Duration,
    produce: suspend () -> Int,
): List<Int> = coroutineScope {
    val collected = Atomic(emptyList<Int>())
    val producer = repeatForever(interval) {
        val n = produce()
        collected.update { values -> values + n }
    }
    delay(duration)
    producer.cancelAndJoin()
    collected.get()
}

data class Update(val key: String, val value: Int)

/** 問題 5: 複数の更新を並行して Map に適用する */
suspend fun applyUpdates(updates: List<Update>): Map<String, Int> {
    val stored = Atomic(emptyMap<String, Int>())
    updates.parMap { update ->
        stored.update { current -> current + (update.key to update.value) }
    }
    return stored.get()
}
