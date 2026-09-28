package ch08

import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.ints.shouldBeInRange
import io.kotest.matchers.shouldBe

class CastingDieTest : FunSpec({

    context("不純な関数") {
        test("castTheDieImpure は 1 から 6 の値を返す") {
            repeat(100) { castTheDieImpure() shouldBeInRange 1..6 }
        }
    }

    context("サンクと IO による記述") {
        test("castTheDieThunk は呼び出すまでサイコロを振らない") {
            val thunk = castTheDieThunk()
            repeat(100) { thunk() shouldBeInRange 1..6 }
        }

        test("castTheDieIO は 1 から 6 の値を返す IO を作る") {
            val io = castTheDieIO()
            repeat(100) { io.unsafeRun() shouldBeInRange 1..6 }
        }

        test("castTheDieTwiceIO は 2 から 12 の値を返す") {
            repeat(100) { castTheDieTwiceIO().unsafeRun() shouldBeInRange 2..12 }
        }

        test("printAndReturn は実行時にメッセージを返す") {
            printAndReturn("Hello").unsafeRun() shouldBe "Hello"
        }

        test("combineIO は 2 つの IO の結果を関数で結合する") {
            combineIO(IO.pure(1), IO.pure(2)) { a, b -> a + b }.unsafeRun() shouldBe 3
        }
    }

    context("suspend 関数による記述") {
        test("castTheDie は 1 から 6 の値を返す") {
            repeat(100) { castTheDie() shouldBeInRange 1..6 }
        }

        test("castTheDieTwice は 2 から 12 の値を返す") {
            repeat(100) { castTheDieTwice() shouldBeInRange 2..12 }
        }

        test("suspend ラムダは値として保持でき、呼び出すまで実行されない") {
            var calls = 0
            val program: suspend () -> Int = { calls += 1; castTheDie() }
            calls shouldBe 0
            program() shouldBeInRange 1..6
            program() shouldBeInRange 1..6
            calls shouldBe 2
        }
    }
})
