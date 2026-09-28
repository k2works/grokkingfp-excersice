package ch05

/**
 * 第5章: flatMap とネスト構造
 *
 * flatten と flatMap によるネストしたリストの操作を学びます。
 */

// ============================================
// flatten - ネストしたリストを平坦化
// ============================================

/** ネストしたリストを平坦化 */
fun <T> flattenList(nested: List<List<T>>): List<T> = nested.flatten()

// ============================================
// flatMap によるリストサイズの変化
// ============================================

/** 各要素を i と i + 10 の 2 つにする（要素数が増える） */
fun duplicate(numbers: List<Int>): List<Int> = numbers.flatMap { listOf(it, it + 10) }

/** 偶数のみを残す（要素数が減る - filter の代わり） */
fun filterEvens(numbers: List<Int>): List<Int> =
    numbers.flatMap { if (it % 2 == 0) listOf(it) else emptyList() }

// ============================================
// 実用的な例
// ============================================

/** 各単語を文字に分解 */
fun toChars(words: List<String>): List<Char> = words.flatMap { it.toList() }

/** 文をスペースで分割して平坦化 */
fun splitAndFlatten(sentences: List<String>): List<String> = sentences.flatMap { it.split(" ") }

/** 1 から n までの範囲を連結 */
fun ranges(ends: List<Int>): List<Int> = ends.flatMap { n -> 1..n }

/** 直積（デカルト積） */
fun <A, B> cartesianProduct(first: List<A>, second: List<B>): List<String> =
    first.flatMap { a -> second.map { b -> "($a, $b)" } }

// ============================================
// ネストした flatMap と sequence ビルダー
// ============================================

/** ネストした flatMap で 3 つのリストの全組み合わせの和を求める */
fun sumsWithFlatMap(xs: List<Int>, ys: List<Int>, zs: List<Int>): List<Int> =
    xs.flatMap { x ->
        ys.flatMap { y ->
            zs.map { z -> x + y + z }
        }
    }

/** sequence ビルダーで同じ計算を書く（for 内包表記の代替） */
fun sumsWithSequence(xs: List<Int>, ys: List<Int>, zs: List<Int>): List<Int> =
    sequence {
        for (x in xs) for (y in ys) for (z in zs) yield(x + y + z)
    }.toList()

// ============================================
// flatMap の戻り値の型
// ============================================

/** Set から始めても Iterable.flatMap は List を返す */
fun productsFromSet(first: Set<Int>, second: List<Int>): List<Int> =
    first.flatMap { a -> second.map { b -> a * b } }

/** flatMapTo で結果のコレクションを Set に指定する */
fun distinctProducts(first: Set<Int>, second: List<Int>): Set<Int> =
    first.flatMapTo(mutableSetOf()) { a -> second.map { b -> a * b } }
