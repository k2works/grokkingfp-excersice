package ch04

/**
 * 第4章: 高階関数
 *
 * 関数を引数として受け取る関数、関数を返す関数、カリー化を学びます。
 */

// ============================================
// map - 各要素を変換
// ============================================

/** 文字列の長さを取得 */
fun len(s: String): Int = s.length

/** 整数を 2 倍にする */
fun double(i: Int): Int = 2 * i

/** 各文字列の長さを取得 */
fun lengths(words: List<String>): List<Int> = words.map(::len)

/** 各要素を 2 倍にする */
fun doubles(numbers: List<Int>): List<Int> = numbers.map(::double)

// ============================================
// filter - 条件に合う要素を抽出
// ============================================

/** 奇数判定 */
fun isOdd(i: Int): Boolean = i % 2 == 1

/** 4 より大きいか判定 */
fun isLargerThan4(i: Int): Boolean = i > 4

/** 奇数のみを抽出 */
fun filterOdds(numbers: List<Int>): List<Int> = numbers.filter(::isOdd)

/** 4 より大きい数のみを抽出 */
fun filterLargerThan4(numbers: List<Int>): List<Int> = numbers.filter(::isLargerThan4)

// ============================================
// fold / reduce - 畳み込み
// ============================================

/** 合計を計算 */
fun sum(numbers: List<Int>): Int = numbers.fold(0) { acc, i -> acc + i }

/** 最大値を取得（空リストなら Int.MIN_VALUE） */
fun largest(numbers: List<Int>): Int = numbers.fold(Int.MIN_VALUE) { max, i -> if (i > max) i else max }

/** 最大値を取得（空リストなら null） */
fun largestOrNull(numbers: List<Int>): Int? = numbers.reduceOrNull { max, i -> if (i > max) i else max }

/** 文字列を連結 */
fun concatenate(strings: List<String>): String = strings.fold("") { acc, s -> acc + s }

/** 区切り文字で連結 */
fun join(strings: List<String>, delimiter: String): String =
    strings.reduceOrNull { acc, s -> acc + delimiter + s } ?: ""

// ============================================
// sortedBy - ソート基準を関数で指定
// ============================================

/** 指定したキー関数でソート */
fun <T, R : Comparable<R>> sortByFunction(list: List<T>, key: (T) -> R): List<T> = list.sortedBy(key)

// ============================================
// 関数を返す関数
// ============================================

/** 指定値より大きいかを判定する関数を返す */
fun largerThan(n: Int): (Int) -> Boolean = { i -> i > n }

/** 指定値で割り切れるかを判定する関数を返す */
fun divisibleBy(n: Int): (Int) -> Boolean = { it % n == 0 }

/** 指定した部分文字列を含むかを判定する関数を返す */
fun containsText(text: String): (String) -> Boolean = { it.contains(text) }

// ============================================
// カリー化
// ============================================

/** 通常の 2 引数関数 */
fun largerThanNormal(n: Int, i: Int): Boolean = i > n

/** カリー化された関数（関数型の値） */
val largerThanCurried: (Int) -> (Int) -> Boolean = { n -> { i -> i > n } }

/** 2 引数関数をカリー化する */
fun <A, B, C> curry(f: (A, B) -> C): (A) -> (B) -> C = { a -> { b -> f(a, b) } }
