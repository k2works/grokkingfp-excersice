package ch06

import arrow.core.None
import arrow.core.Option
import arrow.core.Some
import arrow.core.firstOrNone
import arrow.core.raise.option
import arrow.core.toOption

/**
 * 第6章: nullable 型の基本操作と Arrow Option との比較
 */

// ============================================
// nullable 型による安全な計算
// ============================================

/**
 * 0 で割ると null を返す安全な除算
 */
fun safeDivide(a: Int, b: Int): Int? = if (b == 0) null else a / b

/**
 * 2 つの数値文字列を加算する（エルビス演算子と早期リターン）
 */
fun addStrings(a: String, b: String): Int? {
    val x = a.trim().toIntOrNull() ?: return null
    val y = b.trim().toIntOrNull() ?: return null
    return x + y
}

/**
 * 2 つの数値文字列を加算する（?.let の連鎖）
 */
fun addStringsWithLet(a: String, b: String): Int? =
    a.trim().toIntOrNull()?.let { x ->
        b.trim().toIntOrNull()?.let { y -> x + y }
    }

// ============================================
// nullable 型の主要な操作（Option のメソッドとの対応）
// ============================================

/** map 相当: 値があれば変換する */
fun mapExample(x: Int?): Int? = x?.let { it * 2 }

/** flatMap 相当: 値があれば nullable を返す関数を適用する */
fun flatMapExample(x: Int?): Int? = x?.let { safeDivide(100, it) }

/** filter 相当: 条件を満たさなければ null にする */
fun filterExample(x: Int?, threshold: Int): Int? = x?.takeIf { it > threshold }

/** orElse 相当: null なら代替の nullable 値を使う */
fun orElseExample(x: Int?, alternative: Int?): Int? = x ?: alternative

/** getOrElse 相当: null ならデフォルト値を使う */
fun getOrElseExample(x: Int?, default: Int): Int = x ?: default

/** toList 相当: 0 または 1 要素のリストにする */
fun toListExample(x: Int?): List<Int> = listOfNotNull(x)

/** forall 相当: null なら true、値があれば条件を判定する */
fun forallExample(x: Int?, max: Int): Boolean = x == null || x < max

/** exists 相当: null なら false、値があれば条件を判定する */
fun existsExample(x: Int?, min: Int): Boolean = x != null && x > min

// ============================================
// Arrow Option との比較
// ============================================

/**
 * 先頭要素を Option で返す。
 * 要素自体が null になりうるリストでも「空」と「先頭が null」を区別できる。
 */
fun firstElement(list: List<Int?>): Option<Int?> = list.firstOrNone()

fun describeFirst(list: List<Int?>): String =
    when (val first = firstElement(list)) {
        is Some -> "first element is ${first.value}"
        None -> "list is empty"
    }

/**
 * option { } DSL: bind() で None なら即座に None を返す
 */
fun addStringsOption(a: String, b: String): Option<Int> = option {
    val x = a.trim().toIntOrNull().toOption().bind()
    val y = ensureNotNull(b.trim().toIntOrNull())
    x + y
}

/** nullable 型から Option への変換 */
fun toOptionExample(x: Int?): Option<Int> = x.toOption()
