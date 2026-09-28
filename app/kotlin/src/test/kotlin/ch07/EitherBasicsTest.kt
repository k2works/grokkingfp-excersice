package ch07

import arrow.core.None
import arrow.core.Some
import arrow.core.left
import arrow.core.nonEmptyListOf
import arrow.core.right
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

class EitherBasicsTest : FunSpec({

    context("Either の作成") {
        test("right() で成功値を作る") {
            rightValue() shouldBe 42.right()
            rightValue().isRight() shouldBe true
        }

        test("left() でエラーを作る") {
            leftValue() shouldBe "Error occurred".left()
            leftValue().isLeft() shouldBe true
        }
    }

    context("Either の主要な操作") {
        val year = 996.right()
        val noYear = "no year".left()

        test("map は Right の値だけを変換する") {
            mapExample(year) shouldBe 1992.right()
            mapExample(noYear) shouldBe "no year".left()
        }

        test("flatMap は Either を返す関数をつなぐ") {
            flatMapExample(4.right()) shouldBe 25.right()
            flatMapExample(0.right()) shouldBe "Cannot divide by zero".left()
            flatMapExample(noYear) shouldBe "no year".left()
        }

        test("recover は Left のときに代替の Either を使う（orElse 相当）") {
            orElseExample(year, 2020.right()) shouldBe 996.right()
            orElseExample(noYear, 2020.right()) shouldBe 2020.right()
            orElseExample(noYear, "other".left()) shouldBe "other".left()
        }

        test("getOrElse は Left のときにデフォルト値を返す") {
            getOrElseExample(year, 0) shouldBe 996
            getOrElseExample(noYear, 0) shouldBe 0
        }

        test("getOrNull と getOrNone でエラー情報を捨てて変換する") {
            toNullableExample(year) shouldBe 996
            toNullableExample(noYear) shouldBe null
            toOptionExample(year) shouldBe Some(996)
            toOptionExample(noYear) shouldBe None
        }

        test("fold は両方のケースを 1 つの値にまとめる") {
            foldExample(year) shouldBe "Success: 996"
            foldExample(noYear) shouldBe "Error: no year"
        }
    }

    context("nullable 型 / Option から Either への変換") {
        test("fromNullable は null をエラーメッセージ付きの Left にする") {
            fromNullable(996, "no year given") shouldBe 996.right()
            fromNullable(null, "no year given") shouldBe "no year given".left()
        }

        test("fromOption は None をエラーメッセージ付きの Left にする") {
            fromOption(Some(996), "no year given") shouldBe 996.right()
            fromOption(None, "no year given") shouldBe "no year given".left()
        }
    }

    context("Raise<E> によるバリデーション") {
        test("validateAge は範囲内の年齢を Right で返す") {
            validateAge(25) shouldBe 25.right()
        }

        test("validateAge は範囲外の年齢を Left で返す") {
            validateAge(-5) shouldBe "Age cannot be negative".left()
            validateAge(200) shouldBe "Age cannot be greater than 150".left()
        }

        test("validateEmail は @ を含まないと Left を返す") {
            validateEmail("alice@example.com") shouldBe "alice@example.com".right()
            validateEmail("") shouldBe "Email cannot be empty".left()
            validateEmail("invalid") shouldBe "Email must contain @".left()
        }

        test("validateUsername は長さを検証する") {
            validateUsername("alice") shouldBe "alice".right()
            validateUsername(" ") shouldBe "Username cannot be empty".left()
            validateUsername("ab") shouldBe "Username must be at least 3 characters".left()
            validateUsername("a".repeat(21)) shouldBe "Username must be at most 20 characters".left()
        }
    }

    context("複合バリデーション") {
        test("validateUser は flatMap のネストで User を組み立てる") {
            validateUser("alice", "alice@example.com", 25) shouldBe
                User("alice", "alice@example.com", 25).right()
            validateUser("ab", "alice@example.com", 25) shouldBe
                "Username must be at least 3 characters".left()
        }

        test("validateUserDsl は either { } と bind() で User を組み立てる") {
            validateUserDsl("alice", "alice@example.com", 25) shouldBe
                User("alice", "alice@example.com", 25).right()
            validateUserDsl("alice", "invalid", 25) shouldBe "Email must contain @".left()
        }

        test("validateUserRaise は Raise の関数を直接呼び出して組み立てる") {
            validateUserRaise("alice", "alice@example.com", 25) shouldBe
                User("alice", "alice@example.com", 25).right()
            validateUserRaise("alice", "alice@example.com", -1) shouldBe "Age cannot be negative".left()
        }

        test("最初のエラーで停止するため、複数のエラーがあっても 1 つしか返らない") {
            validateUserDsl("ab", "invalid", -1) shouldBe "Username must be at least 3 characters".left()
        }
    }

    context("エラーの蓄積") {
        test("zipOrAccumulate はすべて成功すれば User を返す") {
            validateUserAccumulating("alice", "alice@example.com", 25) shouldBe
                User("alice", "alice@example.com", 25).right()
        }

        test("zipOrAccumulate はすべてのエラーを NonEmptyList に集める") {
            validateUserAccumulating("ab", "invalid", -1) shouldBe nonEmptyListOf(
                "Username must be at least 3 characters",
                "Email must contain @",
                "Age cannot be negative",
            ).left()
        }

        test("zipOrAccumulate はエラーが 1 つでも NonEmptyList で返す") {
            validateUserAccumulating("alice", "invalid", 25) shouldBe
                nonEmptyListOf("Email must contain @").left()
        }
    }
})
