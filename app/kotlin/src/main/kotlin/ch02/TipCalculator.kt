package ch02

/**
 * 第2章: チップ計算（純粋関数版）
 */

/** 6 人以上: 20%、1〜5 人: 10%、0 人: 0% のチップ率を返す */
fun getTipPercentage(names: List<String>): Int =
    when {
        names.size > 5 -> 20
        names.isNotEmpty() -> 10
        else -> 0
    }
