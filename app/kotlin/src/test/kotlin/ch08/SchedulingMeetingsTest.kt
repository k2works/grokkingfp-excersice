package ch08

import arrow.core.Either
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.collections.shouldHaveSize
import io.kotest.matchers.shouldBe
import io.kotest.matchers.types.shouldBeInstanceOf

class SchedulingMeetingsTest : FunSpec({

    context("純粋関数") {
        test("重なる 2 つのミーティングを検出する") {
            meetingsOverlap(MeetingTime(9, 10), MeetingTime(9, 11)) shouldBe true
            meetingsOverlap(MeetingTime(9, 11), MeetingTime(10, 12)) shouldBe true
        }

        test("隣接するミーティングは重ならない") {
            meetingsOverlap(MeetingTime(9, 10), MeetingTime(10, 11)) shouldBe false
            meetingsOverlap(MeetingTime(11, 12), MeetingTime(9, 10)) shouldBe false
        }

        test("possibleMeetings は既存の予定と重ならない枠を返す") {
            val existing = listOf(MeetingTime(8, 10), MeetingTime(11, 12), MeetingTime(9, 10))
            possibleMeetings(existing, 8, 16, 1) shouldBe listOf(
                MeetingTime(10, 11),
                MeetingTime(12, 13),
                MeetingTime(13, 14),
                MeetingTime(14, 15),
                MeetingTime(15, 16),
            )
            possibleMeetings(existing, 8, 16, 2).first() shouldBe MeetingTime(12, 14)
        }

        test("予定がなければ全ての枠が候補になる") {
            possibleMeetings(emptyList(), 8, 12, 2) shouldBe listOf(
                MeetingTime(8, 10),
                MeetingTime(9, 11),
                MeetingTime(10, 12),
            )
        }

        test("枠が確保できなければ空リストを返す") {
            possibleMeetings(listOf(MeetingTime(8, 16)), 8, 16, 1) shouldBe emptyList()
        }
    }

    context("IO 版") {
        test("calendarEntriesIO は作成しただけでは API を呼ばない") {
            calendarEntriesIO("Alice").shouldBeInstanceOf<IO<List<MeetingTime>>>()
        }

        test("scheduleIO はリトライ付きで空き時間を返す") {
            scheduleIO("Alice", "Bob", 1).unsafeRun() shouldBe MeetingTime(10, 11)
            scheduleIO("Alice", "Bob", 2).unsafeRun() shouldBe MeetingTime(12, 14)
        }
    }

    context("suspend 版の orElse") {
        test("成功した場合は元の値を返す") {
            val program = suspend { 996 } orElse { 2020 }
            program() shouldBe 996
        }

        test("失敗した場合は代替の処理を実行する") {
            val noYear: suspend () -> Int = { throw RuntimeException("no year") }
            val program = noYear orElse { 2020 }
            program() shouldBe 2020
        }

        test("orElse は呼び出すまで実行されない") {
            var calls = 0
            val program = suspend { calls += 1; 1 } orElse { 2 }
            calls shouldBe 0
            program() shouldBe 1
            calls shouldBe 1
        }
    }

    context("Schedule によるリトライ") {
        test("失敗し続けると 1 + maxRetries 回実行して例外を投げる") {
            var calls = 0
            shouldThrow<RuntimeException> {
                retry(10) {
                    calls += 1
                    throw RuntimeException("failed")
                }
            }
            calls shouldBe 11
        }

        test("途中で成功すればその値を返す") {
            var calls = 0
            val result = retry(10) {
                calls += 1
                if (calls < 3) throw RuntimeException("failed") else calls
            }
            result shouldBe 3
        }

        test("retryWithDefault は全て失敗したらデフォルト値を返す") {
            var calls = 0
            val result = retryWithDefault(3, emptyList<MeetingTime>()) {
                calls += 1
                throw RuntimeException("Connection error")
            }
            result shouldBe emptyList()
            calls shouldBe 4
        }

        test("retryWithDefault は成功すればその値を返す") {
            retryWithDefault(3, 0) { 42 } shouldBe 42
        }

        test("Either.catch と組み合わせると失敗を値として扱える") {
            val result = Either.catch { retry(2) { throw RuntimeException("Connection error") } }
            result.shouldBeInstanceOf<Either.Left<Throwable>>()
            result.value.message shouldBe "Connection error"
        }
    }

    context("suspend 版のスケジューリング") {
        test("scheduledMeetings は複数人の予定をまとめる") {
            scheduledMeetings(listOf("Alice", "Bob")) shouldBe listOf(
                MeetingTime(8, 10),
                MeetingTime(11, 12),
                MeetingTime(9, 10),
            )
            scheduledMeetings(listOf("Alice", "Bob", "Charlie")) shouldHaveSize 4
            scheduledMeetings(emptyList()) shouldBe emptyList()
        }

        test("scheduledMeetingsConcurrently は async / awaitAll で同じ結果を返す") {
            scheduledMeetingsConcurrently(listOf("Alice", "Bob")) shouldBe listOf(
                MeetingTime(8, 10),
                MeetingTime(11, 12),
                MeetingTime(9, 10),
            )
        }

        test("scheduledMeetingsPar は parMap で同じ結果を返す") {
            scheduledMeetingsPar(listOf("Alice", "Bob")) shouldBe listOf(
                MeetingTime(8, 10),
                MeetingTime(11, 12),
                MeetingTime(9, 10),
            )
        }

        test("schedule は最初の空き時間を返す") {
            schedule(listOf("Alice", "Bob"), 1) shouldBe MeetingTime(10, 11)
            schedule(listOf("Alice", "Bob"), 2) shouldBe MeetingTime(12, 14)
            schedule(listOf("Alice", "Bob"), 3) shouldBe MeetingTime(12, 15)
            schedule(listOf("Alice", "Bob"), 4) shouldBe MeetingTime(12, 16)
        }

        test("schedule は空き時間がなければ null を返す") {
            schedule(listOf("Alice", "Bob"), 5) shouldBe null
        }

        test("schedulingProgram は入出力を関数として受け取る") {
            val names = ArrayDeque(listOf("Alice", "Bob"))
            val shown = mutableListOf<MeetingTime?>()
            schedulingProgram(
                getName = { names.removeFirst() },
                showMeeting = { shown.add(it) },
            )
            shown shouldBe listOf(MeetingTime(12, 14))
        }
    }
})
