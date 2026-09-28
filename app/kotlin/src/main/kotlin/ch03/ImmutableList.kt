package ch03

/**
 * 第3章: イミュータブルなデータ操作
 *
 * Kotlin 標準ライブラリの読み取り専用 List を使って、
 * 元のデータを変更せずに新しいリストを作る操作を学びます。
 */

// ============================================
// 読み取り専用 List と防御的コピー
// ============================================

/** 呼び出し時点の内容を新しいリストとしてコピーする */
fun <T> snapshotOf(list: List<T>): List<T> = list.toList()

// ============================================
// 基本的な List 操作
// ============================================

/** 最初の 2 要素を取得 */
fun firstTwo(list: List<String>): List<String> = list.take(2)

/** 最後の 2 要素を取得 */
fun lastTwo(list: List<String>): List<String> = list.takeLast(2)

/** start 以上 end 未満の要素を取得 */
fun <T> slice(list: List<T>, start: Int, end: Int): List<T> = list.slice(start until end)

// ============================================
// リストの変換
// ============================================

/** 最初の 2 要素を末尾に移動 */
fun movedFirstTwoToTheEnd(list: List<String>): List<String> {
    val firstTwo = list.take(2)
    val withoutFirstTwo = list.drop(2)
    return withoutFirstTwo + firstTwo
}

/** 最後の要素の前に挿入 */
fun insertedBeforeLast(list: List<String>, element: String): List<String> {
    val withoutLast = list.dropLast(1)
    val last = list.takeLast(1)
    return withoutLast + element + last
}

/** 指定位置に要素を挿入 */
fun insertAt(list: List<String>, index: Int, element: String): List<String> =
    list.take(index) + element + list.drop(index)

/** リストの中央に要素を挿入 */
fun insertAtMiddle(list: List<String>, element: String): List<String> =
    insertAt(list, list.size / 2, element)

// ============================================
// buildList
// ============================================

/** 1 から n までの 2 乗のリストを作る（可変操作は buildList の内側に閉じ込める） */
fun squares(n: Int): List<Int> = buildList {
    for (i in 1..n) add(i * i)
}

// ============================================
// String の操作
// ============================================

/** 名を頭文字に省略する */
fun abbreviate(name: String): String {
    val initial = name.substring(0, 1)
    val separator = name.indexOf(' ')
    val lastName = name.substring(separator + 1)
    return "$initial. $lastName"
}
