package ch08

import arrow.core.Either
import arrow.core.getOrElse

/**
 * 第8章: サンク () -> A を包んだ最小の IO
 *
 * Arrow 2.x は独自の IO 型を持たないため、学習用に「副作用の記述」を値として扱う
 * 小さな IO を自作する。実務では suspend 関数を使う（SchedulingMeetings.kt 参照）。
 */
class IO<A> private constructor(private val thunk: () -> A) {

    /** IO を実行して結果を取得する（ここで初めて副作用が発生する） */
    fun unsafeRun(): A = thunk()

    /** 結果を変換する */
    fun <B> map(f: (A) -> B): IO<B> = IO { f(thunk()) }

    /** IO を返す関数を適用してフラット化する */
    fun <B> flatMap(f: (A) -> IO<B>): IO<B> = IO { f(thunk()).unsafeRun() }

    /** 失敗したら代替の IO を実行する */
    fun orElse(alternative: IO<A>): IO<A> =
        IO { Either.catch { thunk() }.getOrElse { alternative.unsafeRun() } }

    /** 失敗を Either の値として取り出す */
    fun attempt(): IO<Either<Throwable, A>> = IO { Either.catch { thunk() } }

    /** 失敗したら最大 maxRetries 回まで再実行する */
    fun retry(maxRetries: Int): IO<A> =
        List(maxRetries) { this }.fold(this) { program, retryAction -> program.orElse(retryAction) }

    companion object {
        /** 副作用のある式を遅延実行する IO を作る */
        fun <A> delay(thunk: () -> A): IO<A> = IO(thunk)

        /** 既存の値をラップする（副作用なし） */
        fun <A> pure(value: A): IO<A> = IO { value }

        /** 何もしない IO */
        val unit: IO<Unit> = pure(Unit)
    }
}

/** List<IO<A>> を IO<List<A>> に変換する */
fun <A> List<IO<A>>.sequence(): IO<List<A>> =
    fold(IO.pure(emptyList())) { acc, io -> acc.flatMap { list -> io.map { list + it } } }

/** 各要素に IO を返す関数を適用し、結果を 1 つの IO にまとめる */
fun <A, B> List<A>.traverse(f: (A) -> IO<B>): IO<List<B>> = map(f).sequence()
