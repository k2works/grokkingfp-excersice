package ch07

import arrow.core.Either
import arrow.core.NonEmptyList
import arrow.core.Option
import arrow.core.flatMap
import arrow.core.getOrElse
import arrow.core.left
import arrow.core.raise.Raise
import arrow.core.raise.either
import arrow.core.raise.ensure
import arrow.core.raise.zipOrAccumulate
import arrow.core.recover
import arrow.core.right

/**
 * 第7章: Arrow Either の基本操作と Raise DSL
 */

// ============================================
// Either の作成
// ============================================

fun rightValue(): Either<String, Int> = 42.right()

fun leftValue(): Either<String, Int> = "Error occurred".left()

// ============================================
// Either の主要な操作
// ============================================

/** map: Right の値だけを変換する */
fun mapExample(either: Either<String, Int>): Either<String, Int> = either.map { it * 2 }

/** flatMap: Right なら Either を返す関数を適用する */
fun flatMapExample(either: Either<String, Int>): Either<String, Int> =
    either.flatMap { x ->
        if (x == 0) "Cannot divide by zero".left() else (100 / x).right()
    }

/** recover: Left なら代替の Either を使う（orElse 相当） */
fun orElseExample(either: Either<String, Int>, alternative: Either<String, Int>): Either<String, Int> =
    either.recover { alternative.bind() }

/** getOrElse: Left ならデフォルト値を返す */
fun getOrElseExample(either: Either<String, Int>, default: Int): Int = either.getOrElse { default }

/** getOrNull: エラー情報を捨てて nullable 型にする */
fun toNullableExample(either: Either<String, Int>): Int? = either.getOrNull()

/** getOrNone: エラー情報を捨てて Option にする */
fun toOptionExample(either: Either<String, Int>): Option<Int> = either.getOrNone()

/** fold: 両方のケースを 1 つの値にまとめる */
fun foldExample(either: Either<String, Int>): String =
    either.fold(
        ifLeft = { error -> "Error: $error" },
        ifRight = { value -> "Success: $value" },
    )

// ============================================
// nullable 型 / Option から Either への変換
// ============================================

fun fromNullable(value: Int?, errorMsg: String): Either<String, Int> =
    value?.right() ?: errorMsg.left()

fun fromOption(option: Option<Int>, errorMsg: String): Either<String, Int> =
    option.toEither { errorMsg }

// ============================================
// Raise<E> によるバリデーション
// ============================================

/** Raise<String> の文脈で年齢を検証する。失敗したら raise で処理を中断する */
fun Raise<String>.checkAge(age: Int): Int {
    ensure(age >= 0) { "Age cannot be negative" }
    ensure(age <= 150) { "Age cannot be greater than 150" }
    return age
}

fun Raise<String>.checkEmail(email: String): String {
    ensure(email.isNotBlank()) { "Email cannot be empty" }
    ensure("@" in email) { "Email must contain @" }
    return email
}

fun Raise<String>.checkUsername(username: String): String {
    ensure(username.isNotBlank()) { "Username cannot be empty" }
    ensure(username.length >= 3) { "Username must be at least 3 characters" }
    ensure(username.length <= 20) { "Username must be at most 20 characters" }
    return username
}

/** either { } で Raise の関数を Either を返す関数に変換する */
fun validateAge(age: Int): Either<String, Int> = either { checkAge(age) }

fun validateEmail(email: String): Either<String, String> = either { checkEmail(email) }

fun validateUsername(username: String): Either<String, String> = either { checkUsername(username) }

// ============================================
// 複合バリデーション
// ============================================

data class User(val username: String, val email: String, val age: Int)

/** flatMap のネストで組み立てる */
fun validateUser(username: String, email: String, age: Int): Either<String, User> =
    validateUsername(username).flatMap { validUsername ->
        validateEmail(email).flatMap { validEmail ->
            validateAge(age).map { validAge ->
                User(validUsername, validEmail, validAge)
            }
        }
    }

/** either { } と bind() で組み立てる */
fun validateUserDsl(username: String, email: String, age: Int): Either<String, User> = either {
    val validUsername = validateUsername(username).bind()
    val validEmail = validateEmail(email).bind()
    val validAge = validateAge(age).bind()
    User(validUsername, validEmail, validAge)
}

/** Raise の関数を直接呼び出して組み立てる（bind() も不要） */
fun validateUserRaise(username: String, email: String, age: Int): Either<String, User> = either {
    User(checkUsername(username), checkEmail(email), checkAge(age))
}

// ============================================
// エラーの蓄積
// ============================================

/** zipOrAccumulate: すべての検証を実行し、エラーを NonEmptyList に集める */
fun validateUserAccumulating(username: String, email: String, age: Int): Either<NonEmptyList<String>, User> =
    either {
        zipOrAccumulate(
            { checkUsername(username) },
            { checkEmail(email) },
            { checkAge(age) },
        ) { validUsername, validEmail, validAge ->
            User(validUsername, validEmail, validAge)
        }
    }
