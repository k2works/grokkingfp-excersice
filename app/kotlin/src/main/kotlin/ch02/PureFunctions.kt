package ch02

import kotlin.random.Random

/**
 * 第2章: 純粋関数
 *
 * 純粋関数の特徴:
 * 1. 同じ入力には常に同じ出力を返す
 * 2. 副作用がない（外部状態を変更しない）
 */

// ============================================
// 純粋関数の例
// ============================================

fun increment(x: Int): Int = x + 1

fun add(a: Int, b: Int): Int = a + b

fun getFirstCharacter(s: String): Char = s[0]

/** 入力の 95% を返す */
fun applyDiscount(x: Double): Double = x * 95.0 / 100.0

/** 文字列の長さの 3 倍をスコアとする */
fun calculateStringScore(s: String): Int = s.length * 3

// ============================================
// 不純な関数の例（アンチパターン）
// ============================================

/** 不純: 乱数に依存するため、同じ入力でも毎回異なる値を返しうる */
fun randomPart(x: Double): Double = x * Random.nextDouble()

/** 不純: 呼び出すタイミングによって結果が変わる */
fun currentTime(): Long = System.currentTimeMillis()
