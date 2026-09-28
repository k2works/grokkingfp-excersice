package ch02

/**
 * 第2章: ショッピングカート（純粋関数版）
 *
 * 状態を持たず、カートの中身から割引率を毎回計算します。
 */

/** Book が含まれていれば 5%、そうでなければ 0% の割引率を返す */
fun getDiscountPercentage(items: List<String>): Int =
    if ("Book" in items) 5 else 0
