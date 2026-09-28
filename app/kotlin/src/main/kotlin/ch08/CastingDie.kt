package ch08

import kotlin.random.Random

/**
 * 第8章: サイコロを振る例
 *
 * 不純な関数 → サンク → IO → suspend 関数 の順に副作用の扱い方を比較する。
 */

// ============================================
// 不純な関数（副作用あり）
// ============================================

/** 呼び出すたびに異なる値が返る */
fun castTheDieImpure(): Int = Random.nextInt(1, 7)

// ============================================
// サンクと IO による記述
// ============================================

/** サンク: 「サイコロを振る」という計算の記述を返す */
fun castTheDieThunk(): () -> Int = { castTheDieImpure() }

/** IO: 作成しただけではサイコロは振られない */
fun castTheDieIO(): IO<Int> = IO.delay { castTheDieImpure() }

/** サイコロを 2 回振って合計を返す */
fun castTheDieTwiceIO(): IO<Int> =
    castTheDieIO().flatMap { first -> castTheDieIO().map { second -> first + second } }

/** メッセージを表示してから値を返す */
fun printAndReturn(message: String): IO<String> = IO.delay {
    println(message)
    message
}

/** 2 つの IO を順番に実行し、結果を関数で結合する */
fun <A, B, C> combineIO(io1: IO<A>, io2: IO<B>, f: (A, B) -> C): IO<C> =
    io1.flatMap { a -> io2.map { b -> f(a, b) } }

// ============================================
// suspend 関数による記述
// ============================================

/** suspend: 呼び出せる場所がコルーチンの中に限定される */
suspend fun castTheDie(): Int = castTheDieImpure()

/** flatMap の代わりに、普通の逐次コードとして合成できる */
suspend fun castTheDieTwice(): Int {
    val first = castTheDie()
    val second = castTheDie()
    return first + second
}
