/**
 * 第1章: 関数型プログラミング入門
 *
 * 関数型プログラミングの基本概念を TypeScript と fp-ts で学びます。
 * 命令型プログラミングとの違いを理解し、純粋関数の利点を実感することが目標です。
 */

import { pipe } from 'fp-ts/function'

// =============================================================================
// 1.1 命令型 vs 関数型
// =============================================================================

/**
 * 命令型でワードスコアを計算する。
 * 命令型プログラミングは「どうやるか（HOW）」を記述します。
 *
 * @example
 * calculateScoreImperative("hello") // => 5
 * calculateScoreImperative("") // => 0
 */
export const calculateScoreImperative = (word: string): number => {
  let score = 0
  for (const _ of word) {
    score += 1
  }
  return score
}

/**
 * 関数型でワードスコアを計算する。
 * 関数型プログラミングは「何をするか（WHAT）」を記述します。
 *
 * @example
 * wordScore("hello") // => 5
 * wordScore("Scala") // => 5
 * wordScore("") // => 0
 */
export const wordScore = (word: string): number => word.length

// =============================================================================
// 1.2 基本的な関数定義
// =============================================================================

/**
 * 値を1増加させる。
 *
 * @example
 * increment(5) // => 6
 * increment(0) // => 1
 * increment(-1) // => 0
 */
export const increment = (x: number): number => x + 1

/**
 * 文字列の最初の文字を取得する。
 *
 * @example
 * getFirstCharacter("hello") // => "h"
 * getFirstCharacter("TypeScript") // => "T"
 */
export const getFirstCharacter = (s: string): string => s.charAt(0)

/**
 * 2つの数値を加算する。
 *
 * @example
 * add(2, 3) // => 5
 * add(-1, 1) // => 0
 */
export const add = (a: number, b: number): number => a + b

/**
 * 値を2倍にする。
 *
 * @example
 * double(5) // => 10
 * double(0) // => 0
 * double(-3) // => -6
 */
export const double = (x: number): number => x * 2

/**
 * 挨拶メッセージを生成する。
 *
 * @example
 * greet("World") // => "Hello, World!"
 * greet("TypeScript") // => "Hello, TypeScript!"
 */
export const greet = (name: string): string => `Hello, ${name}!`

/**
 * 文字列を大文字に変換する。
 *
 * @example
 * toUppercase("hello") // => "HELLO"
 * toUppercase("TypeScript") // => "TYPESCRIPT"
 */
export const toUppercase = (s: string): string => s.toUpperCase()

/**
 * 文字列を反転する。
 *
 * @example
 * reverseString("hello") // => "olleh"
 * reverseString("TypeScript") // => "tpircSepyT"
 */
export const reverseString = (s: string): string => s.split('').reverse().join('')

// =============================================================================
// 1.3 条件分岐を含む関数
// =============================================================================

/**
 * 絶対値を返す。
 *
 * @example
 * absolute(5) // => 5
 * absolute(-5) // => 5
 * absolute(0) // => 0
 */
export const absolute = (x: number): number => (x >= 0 ? x : -x)

/**
 * 2つの値のうち大きい方を返す。
 *
 * @example
 * maxValue(3, 5) // => 5
 * maxValue(5, 3) // => 5
 * maxValue(3, 3) // => 3
 */
export const maxValue = (a: number, b: number): number => (a >= b ? a : b)

/**
 * 2つの値のうち小さい方を返す。
 *
 * @example
 * minValue(3, 5) // => 3
 * minValue(5, 3) // => 3
 * minValue(3, 3) // => 3
 */
export const minValue = (a: number, b: number): number => (a <= b ? a : b)

/**
 * 値を指定された範囲内に収める。
 *
 * @example
 * clamp(5, 0, 10) // => 5
 * clamp(-5, 0, 10) // => 0
 * clamp(15, 0, 10) // => 10
 */
export const clamp = (value: number, minVal: number, maxVal: number): number =>
  maxValue(minVal, minValue(value, maxVal))

// =============================================================================
// 1.4 述語関数（真偽値を返す関数）
// =============================================================================

/**
 * 偶数かどうかを判定する。
 *
 * @example
 * isEven(4) // => true
 * isEven(3) // => false
 * isEven(0) // => true
 * isEven(-2) // => true
 */
export const isEven = (n: number): boolean => n % 2 === 0

/**
 * 正の数かどうかを判定する。
 *
 * @example
 * isPositive(5) // => true
 * isPositive(0) // => false
 * isPositive(-3) // => false
 */
export const isPositive = (n: number): boolean => n > 0

/**
 * 文字列が空かどうかを判定する。
 *
 * @example
 * isEmpty("") // => true
 * isEmpty("hello") // => false
 */
export const isEmpty = (s: string): boolean => s.length === 0

// =============================================================================
// 1.5 定数と型
// =============================================================================

/** スコアの最大値 */
export const MAX_SCORE = 100 as const

/** スコアの最小値 */
export const MIN_SCORE = 0 as const

/** デフォルトの挨拶 */
export const DEFAULT_GREETING = 'Hello' as const

/**
 * スコアを有効な範囲内に収める。
 *
 * @example
 * getBoundedScore(50) // => 50
 * getBoundedScore(150) // => 100
 * getBoundedScore(-10) // => 0
 */
export const getBoundedScore = (score: number): number => clamp(score, MIN_SCORE, MAX_SCORE)

// =============================================================================
// 1.6 fp-ts の pipe を使った関数合成
// =============================================================================

/**
 * pipe を使った関数合成の例。
 * 文字列を大文字に変換して反転する。
 *
 * @example
 * transformString("hello") // => "OLLEH"
 */
export const transformString = (s: string): string =>
  pipe(
    s,
    toUppercase,
    reverseString
  )

/**
 * pipe を使った数値変換の例。
 * 値を2倍にしてから1増加させる。
 *
 * @example
 * transformNumber(5) // => 11
 */
export const transformNumber = (x: number): number =>
  pipe(
    x,
    double,
    increment
  )

/**
 * pipe を使ったスコア計算の例。
 * 文字列の長さを2倍にして、範囲内に収める。
 *
 * @example
 * calculateBoundedScore("hello") // => 10
 * calculateBoundedScore("a very long string that exceeds max") // => 100
 */
export const calculateBoundedScore = (word: string): number =>
  pipe(
    word,
    wordScore,
    double,
    getBoundedScore
  )
