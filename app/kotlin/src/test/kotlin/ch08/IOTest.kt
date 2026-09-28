package ch08

import arrow.core.Either
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe
import io.kotest.matchers.types.shouldBeInstanceOf

class IOTest : FunSpec({

    context("サンク () -> A") {
        test("サンクを作成しただけでは副作用は発生しない") {
            var calls = 0
            val thunk: () -> Int = { calls += 1; 42 }
            calls shouldBe 0
            thunk() shouldBe 42
            calls shouldBe 1
        }
    }

    context("IO の作成と実行") {
        test("IO.delay は unsafeRun するまで実行されない") {
            var calls = 0
            val io = IO.delay { calls += 1; "done" }
            calls shouldBe 0
            io.unsafeRun() shouldBe "done"
            calls shouldBe 1
        }

        test("同じ IO 値を 2 回実行すると副作用も 2 回発生する") {
            var calls = 0
            val io = IO.delay { calls += 1 }
            io.unsafeRun()
            io.unsafeRun()
            calls shouldBe 2
        }

        test("IO.pure は既存の値をラップする") {
            IO.pure(42).unsafeRun() shouldBe 42
        }

        test("IO.unit は Unit を返す") {
            IO.unit.unsafeRun() shouldBe Unit
        }
    }

    context("IO の合成") {
        test("map で結果を変換できる") {
            IO.pure(21).map { it * 2 }.unsafeRun() shouldBe 42
        }

        test("flatMap で IO を順番に合成できる") {
            val program = IO.pure(1).flatMap { a -> IO.pure(2).map { b -> a + b } }
            program.unsafeRun() shouldBe 3
        }

        test("flatMap で合成しても実行するまで副作用は発生しない") {
            val log = mutableListOf<String>()
            val program = IO.delay { log.add("first") }
                .flatMap { IO.delay { log.add("second") } }
            log shouldBe emptyList()
            program.unsafeRun()
            log shouldBe listOf("first", "second")
        }
    }

    context("orElse") {
        val year: IO<Int> = IO.delay { 996 }
        val noYear: IO<Int> = IO.delay { throw RuntimeException("no year") }

        test("成功した場合は元の値を返す") {
            year.orElse(IO.delay { 2020 }).unsafeRun() shouldBe 996
        }

        test("失敗した場合は代替の IO を実行する") {
            noYear.orElse(IO.delay { 2020 }).unsafeRun() shouldBe 2020
        }

        test("代替の IO も失敗した場合は例外が伝播する") {
            val program = noYear.orElse(IO.delay { throw RuntimeException("can't recover") })
            shouldThrow<RuntimeException> { program.unsafeRun() }.message shouldBe "can't recover"
        }
    }

    context("attempt") {
        test("成功を Right として返す") {
            IO.pure(1).attempt().unsafeRun() shouldBe Either.Right(1)
        }

        test("失敗を Left として返す") {
            val result = IO.delay<Int> { throw IllegalStateException("boom") }.attempt().unsafeRun()
            result.shouldBeInstanceOf<Either.Left<Throwable>>()
            result.value.message shouldBe "boom"
        }
    }

    context("retry") {
        test("失敗し続けると 1 + maxRetries 回実行して例外を投げる") {
            var calls = 0
            val action = IO.delay<Int> { calls += 1; throw RuntimeException("failed") }
            shouldThrow<RuntimeException> { action.retry(10).unsafeRun() }
            calls shouldBe 11
        }

        test("途中で成功すればその値を返す") {
            var calls = 0
            val action = IO.delay {
                calls += 1
                if (calls < 3) throw RuntimeException("failed") else calls
            }
            action.retry(10).unsafeRun() shouldBe 3
        }
    }

    context("sequence と traverse") {
        test("sequence は List<IO<A>> を IO<List<A>> に変換する") {
            listOf(IO.pure(1), IO.pure(2), IO.pure(3)).sequence().unsafeRun() shouldBe listOf(1, 2, 3)
        }

        test("空リストの sequence は空リストを返す") {
            emptyList<IO<Int>>().sequence().unsafeRun() shouldBe emptyList()
        }

        test("sequence は実行するまで副作用を発生させない") {
            var calls = 0
            val program = List(3) { IO.delay { calls += 1; calls } }.sequence()
            calls shouldBe 0
            program.unsafeRun() shouldBe listOf(1, 2, 3)
        }

        test("traverse は各要素に IO を返す関数を適用してまとめる") {
            listOf(1, 2, 3).traverse { IO.pure(it * 10) }.unsafeRun() shouldBe listOf(10, 20, 30)
        }
    }
})
