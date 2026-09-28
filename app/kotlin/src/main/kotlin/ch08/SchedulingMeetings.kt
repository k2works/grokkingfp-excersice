package ch08

import arrow.core.Either
import arrow.core.getOrElse
import arrow.fx.coroutines.parMap
import arrow.resilience.Schedule
import arrow.resilience.retry
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.withContext
import kotlin.random.Random

/**
 * 第8章: ミーティングスケジューリングの例
 *
 * 前半は自作の IO、後半は suspend 関数と Arrow の Schedule で同じ問題を解く。
 */

data class MeetingTime(val startHour: Int, val endHour: Int)

const val WORK_DAY_START = 8
const val WORK_DAY_END = 16
const val MAX_RETRIES = 10
private const val API_FAILURE_RATE = 0.25

// ============================================
// 外部 API のシミュレーション（不純・ランダムに失敗する）
// ============================================

fun calendarEntriesApiCall(name: String): List<MeetingTime> {
    if (Random.nextDouble() < API_FAILURE_RATE) throw RuntimeException("Connection error")
    return when (name) {
        "Alice" -> listOf(MeetingTime(8, 10), MeetingTime(11, 12))
        "Bob" -> listOf(MeetingTime(9, 10))
        else -> listOf(MeetingTime(Random.nextInt(8, 13), Random.nextInt(13, 17)))
    }
}

fun createMeetingApiCall(names: List<String>, meetingTime: MeetingTime) {
    println("SIDE-EFFECT: Created meeting $meetingTime for $names")
}

// ============================================
// 純粋関数
// ============================================

fun meetingsOverlap(meeting1: MeetingTime, meeting2: MeetingTime): Boolean =
    meeting1.endHour > meeting2.startHour && meeting2.endHour > meeting1.startHour

fun possibleMeetings(
    existingMeetings: List<MeetingTime>,
    startHour: Int,
    endHour: Int,
    lengthHours: Int,
): List<MeetingTime> =
    (startHour..endHour - lengthHours)
        .map { start -> MeetingTime(start, start + lengthHours) }
        .filter { slot -> existingMeetings.none { meetingsOverlap(it, slot) } }

// ============================================
// IO 版
// ============================================

fun calendarEntriesIO(name: String): IO<List<MeetingTime>> =
    IO.delay { calendarEntriesApiCall(name) }

fun createMeetingIO(names: List<String>, meeting: MeetingTime): IO<Unit> =
    IO.delay { createMeetingApiCall(names, meeting) }

fun scheduledMeetingsIO(person1: String, person2: String): IO<List<MeetingTime>> =
    calendarEntriesIO(person1).flatMap { entries1 ->
        calendarEntriesIO(person2).map { entries2 -> entries1 + entries2 }
    }

fun scheduleIO(person1: String, person2: String, lengthHours: Int): IO<MeetingTime?> =
    listOf(person1, person2)
        .traverse { calendarEntriesIO(it).retry(MAX_RETRIES) }
        .map { it.flatten() }
        .orElse(IO.pure(emptyList()))
        .map { possibleMeetings(it, WORK_DAY_START, WORK_DAY_END, lengthHours).firstOrNull() }
        .flatMap { meeting ->
            if (meeting == null) IO.pure(null)
            else createMeetingIO(listOf(person1, person2), meeting).map { meeting }
        }

// ============================================
// suspend 版
// ============================================

suspend fun calendarEntries(name: String): List<MeetingTime> =
    withContext(Dispatchers.IO) { calendarEntriesApiCall(name) }

suspend fun createMeeting(names: List<String>, meeting: MeetingTime): Unit =
    withContext(Dispatchers.IO) { createMeetingApiCall(names, meeting) }

/** suspend ラムダ版の orElse: 失敗したら fallback を実行する新しい「記述」を返す */
infix fun <A> (suspend () -> A).orElse(fallback: suspend () -> A): suspend () -> A = {
    Either.catch { this() }.getOrElse { fallback() }
}

/** Arrow の Schedule で最大 maxRetries 回まで再実行する */
suspend fun <A> retry(maxRetries: Int, action: suspend () -> A): A =
    Schedule.recurs<Throwable>(maxRetries.toLong()).retry(action)

/** リトライしても全て失敗したらデフォルト値を返す */
suspend fun <A> retryWithDefault(maxRetries: Int, default: A, action: suspend () -> A): A =
    Either.catch { retry(maxRetries, action) }.getOrElse { default }

/** 複数人の予定を逐次取得する（sequence に相当） */
suspend fun scheduledMeetings(attendees: List<String>): List<MeetingTime> =
    attendees.flatMap { retry(MAX_RETRIES) { calendarEntries(it) } }

/** async / awaitAll で並行に取得する */
suspend fun scheduledMeetingsConcurrently(attendees: List<String>): List<MeetingTime> =
    coroutineScope {
        attendees
            .map { async { retry(MAX_RETRIES) { calendarEntries(it) } } }
            .awaitAll()
            .flatten()
    }

/** Arrow Fx の parMap で並行に取得する（Part V で詳しく扱う） */
suspend fun scheduledMeetingsPar(attendees: List<String>): List<MeetingTime> =
    attendees.parMap { retry(MAX_RETRIES) { calendarEntries(it) } }.flatten()

suspend fun schedule(attendees: List<String>, lengthHours: Int): MeetingTime? {
    val existingMeetings = scheduledMeetings(attendees)
    val possibleMeeting =
        possibleMeetings(existingMeetings, WORK_DAY_START, WORK_DAY_END, lengthHours).firstOrNull()
    possibleMeeting?.let { retry(MAX_RETRIES) { createMeeting(attendees, it) } }
    return possibleMeeting
}

/** 入出力を関数として受け取ることで、コンソールにもテスト用スタブにも差し替えられる */
suspend fun schedulingProgram(
    getName: suspend () -> String,
    showMeeting: suspend (MeetingTime?) -> Unit,
) {
    val name1 = getName()
    val name2 = getName()
    val possibleMeeting = schedule(listOf(name1, name2), 2)
    showMeeting(possibleMeeting)
}
