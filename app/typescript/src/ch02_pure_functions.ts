/**
 * 第2章: 純粋関数とテスト
 *
 * 純粋関数（Pure Function）の概念を学びます。
 * - 同じ入力には常に同じ出力を返す
 * - 副作用がない（外部状態を変更しない）
 *
 * 純粋関数はテストが容易で、コードの予測可能性が向上します。
 */

import { pipe, flow } from 'fp-ts/function'
import * as RA from 'fp-ts/ReadonlyArray'
import * as O from 'fp-ts/Option'

// =============================================================================
// 2.1 純粋関数の基本
// =============================================================================

/**
 * 2つの数値を加算する純粋関数。
 *
 * @example
 * add(2, 3) // => 5
 * add(-1, 1) // => 0
 */
export const add = (a: number, b: number): number => a + b

/**
 * 文字列の長さを返す純粋関数。
 *
 * @example
 * stringLength("hello") // => 5
 * stringLength("") // => 0
 */
export const stringLength = (s: string): number => s.length

/**
 * 単語のスコア（文字数）を返す純粋関数。
 *
 * @example
 * wordScore("Scala") // => 5
 * wordScore("TypeScript") // => 10
 */
export const wordScore = (word: string): number => word.length

// =============================================================================
// 2.2 ボーナススコア計算
// =============================================================================

/**
 * 文字 'c' を含む場合にボーナスを付与する。
 *
 * - 'c' を含む場合: スコア + 5
 * - 'c' を含まない場合: スコアのみ
 *
 * @example
 * bonusScore("Scala") // => 10
 * bonusScore("TypeScript") // => 15
 * bonusScore("Python") // => 6
 */
export const bonusScore = (word: string): number => {
  const base = word.length
  return word.toLowerCase().includes('c') ? base + 5 : base
}

// =============================================================================
// 2.3 ショッピングカートの割引計算
// =============================================================================

/**
 * アイテムリストから割引率を計算する純粋関数。
 *
 * - Book を含む場合: 5% 割引
 * - Book を含まない場合: 0% 割引
 *
 * @example
 * getDiscountPercentage(["Apple", "Book"]) // => 5
 * getDiscountPercentage(["Apple", "Orange"]) // => 0
 * getDiscountPercentage([]) // => 0
 */
export const getDiscountPercentage = (items: readonly string[]): number =>
  items.includes('Book') ? 5 : 0

/**
 * 割引額を計算する純粋関数。
 *
 * @example
 * calculateDiscount(100, 10) // => 10
 * calculateDiscount(100, 0) // => 0
 */
export const calculateDiscount = (price: number, discountPercent: number): number =>
  (price * discountPercent) / 100

/**
 * 最終価格を計算する純粋関数。
 *
 * @example
 * calculateFinalPrice(100, ["Apple", "Book"]) // => 95
 * calculateFinalPrice(100, ["Apple"]) // => 100
 */
export const calculateFinalPrice = (price: number, items: readonly string[]): number => {
  const discountPercent = getDiscountPercentage(items)
  const discount = calculateDiscount(price, discountPercent)
  return price - discount
}

// =============================================================================
// 2.4 チップ計算
// =============================================================================

/**
 * グループの人数からチップ率を計算する純粋関数。
 *
 * - 6人以上: 20%
 * - 1-5人: 10%
 * - 0人: 0%
 *
 * @example
 * getTipPercentage(["Alice", "Bob", "Charlie", "Dave", "Eve", "Frank"]) // => 20
 * getTipPercentage(["Alice", "Bob"]) // => 10
 * getTipPercentage([]) // => 0
 */
export const getTipPercentage = (names: readonly string[]): number => {
  const size = names.length
  if (size > 5) return 20
  if (size > 0) return 10
  return 0
}

/**
 * チップ額を計算する純粋関数。
 *
 * @example
 * calculateTip(100, ["Alice", "Bob"]) // => 10
 * calculateTip(100, ["A", "B", "C", "D", "E", "F"]) // => 20
 */
export const calculateTip = (bill: number, names: readonly string[]): number => {
  const tipPercent = getTipPercentage(names)
  return (bill * tipPercent) / 100
}

// =============================================================================
// 2.5 参照透過性の例
// =============================================================================

/**
 * 参照透過性の例を示す。
 *
 * 純粋関数は参照透過性を持つ：
 * 式をその評価結果で置き換えても、プログラムの意味が変わらない。
 *
 * @example
 * referentialTransparencyExample() // => true
 */
export const referentialTransparencyExample = (): boolean => {
  // 以下の2つは同等
  const score1 = wordScore('Scala')
  const score2 = wordScore('Scala')

  // wordScore("Scala") は常に 5 を返すので、
  // 以下のように置き換えても結果は同じ
  const total1 = wordScore('Scala') + wordScore('Java')
  const total2 = 5 + 4 // wordScore の結果で置き換え

  return score1 === score2 && score1 === 5 && total1 === total2 && total1 === 9
}

// =============================================================================
// 2.6 文字列操作の純粋関数
// =============================================================================

/**
 * 文字列に感嘆符を追加する。
 *
 * @example
 * appendExclamation("Hello") // => "Hello!"
 * appendExclamation("") // => "!"
 */
export const appendExclamation = (s: string): string => s + '!'

/**
 * 母音の数をカウントする。
 *
 * @example
 * countVowels("hello") // => 2
 * countVowels("TypeScript") // => 2
 * countVowels("xyz") // => 0
 */
export const countVowels = (s: string): number => {
  const vowels = 'aeiouAEIOU'
  return s.split('').filter((c) => vowels.includes(c)).length
}

// =============================================================================
// 2.7 リスト操作の純粋関数（fp-ts を使用）
// =============================================================================

/**
 * リストの全ての要素を2倍にする。
 *
 * @example
 * doubleAll([1, 2, 3]) // => [2, 4, 6]
 * doubleAll([]) // => []
 */
export const doubleAll = (numbers: readonly number[]): readonly number[] =>
  pipe(
    numbers,
    RA.map((n) => n * 2)
  )

/**
 * 正の数のみをフィルタリングする。
 *
 * @example
 * filterPositive([1, -2, 3, -4, 5]) // => [1, 3, 5]
 * filterPositive([-1, -2]) // => []
 */
export const filterPositive = (numbers: readonly number[]): readonly number[] =>
  pipe(
    numbers,
    RA.filter((n) => n > 0)
  )

/**
 * 最も長い文字列を見つける。
 *
 * 空のリストの場合は空文字列を返す。
 *
 * @example
 * findLongest(["apple", "banana", "cherry"]) // => "banana"
 * findLongest(["a", "bb", "ccc"]) // => "ccc"
 * findLongest([]) // => ""
 */
export const findLongest = (words: readonly string[]): string =>
  pipe(
    words,
    RA.reduce('', (longest, word) => (word.length > longest.length ? word : longest))
  )

/**
 * 全ての要素が正の数かどうかを判定する。
 *
 * 空リストの場合は true を返す（vacuous truth）。
 *
 * @example
 * allPositive([1, 2, 3]) // => true
 * allPositive([1, -2, 3]) // => false
 * allPositive([]) // => true
 */
export const allPositive = (numbers: readonly number[]): boolean =>
  pipe(
    numbers,
    RA.every((n) => n > 0)
  )

/**
 * 負の数が含まれているかどうかを判定する。
 *
 * @example
 * anyNegative([1, -2, 3]) // => true
 * anyNegative([1, 2, 3]) // => false
 * anyNegative([]) // => false
 */
export const anyNegative = (numbers: readonly number[]): boolean =>
  pipe(
    numbers,
    RA.some((n) => n < 0)
  )

/**
 * リストの合計を計算する。
 *
 * @example
 * sumList([1, 2, 3, 4, 5]) // => 15
 * sumList([]) // => 0
 */
export const sumList = (numbers: readonly number[]): number =>
  pipe(
    numbers,
    RA.reduce(0, (acc, n) => acc + n)
  )

/**
 * リストの平均を計算する。
 *
 * 空リストの場合は None を返す。
 *
 * @example
 * average([1, 2, 3, 4, 5]) // => some(3)
 * average([]) // => none
 */
export const average = (numbers: readonly number[]): O.Option<number> =>
  pipe(
    numbers,
    O.fromPredicate((arr) => arr.length > 0),
    O.map((arr) => sumList(arr) / arr.length)
  )

// =============================================================================
// 2.8 文字 'a' を除外するワードスコア
// =============================================================================

/**
 * 文字 'a' を除外してワードスコアを計算する。
 *
 * @example
 * wordScoreNoA("Scala") // => 3
 * wordScoreNoA("function") // => 8
 * wordScoreNoA("") // => 0
 * wordScoreNoA("aaa") // => 0
 */
export const wordScoreNoA = (word: string): number =>
  word.replace(/a/gi, '').length

// =============================================================================
// 2.9 高階関数の基本
// =============================================================================

/**
 * 関数を2回適用する。
 *
 * @example
 * applyTwice(x => x + 1, 5) // => 7
 * applyTwice(x => x * 2, 3) // => 12
 */
export const applyTwice = (f: (x: number) => number, x: number): number => f(f(x))

/**
 * 2つの関数を合成する。
 *
 * compose(f, g)(x) = f(g(x))
 *
 * @example
 * const double = (x: number) => x * 2
 * const addOne = (x: number) => x + 1
 * const doubleThenAdd = compose(addOne, double)
 * doubleThenAdd(5) // => 11
 */
export const compose = <A, B, C>(f: (b: B) => C, g: (a: A) => B): ((a: A) => C) =>
  (a) => f(g(a))

/**
 * fp-ts の flow を使った関数合成。
 * flow(f, g)(x) = g(f(x))
 *
 * @example
 * const double = (x: number) => x * 2
 * const addOne = (x: number) => x + 1
 * const doubleThenAdd = composeWithFlow(double, addOne)
 * doubleThenAdd(5) // => 11
 */
export const composeWithFlow = <A, B, C>(
  f: (a: A) => B,
  g: (b: B) => C
): ((a: A) => C) => flow(f, g)

// =============================================================================
// 2.10 タイムスタンプフォーマット（純粋な部分のみ）
// =============================================================================

/**
 * タイムスタンプを文字列にフォーマットする純粋関数。
 *
 * @example
 * formatTimestamp(2024, 1, 15) // => "2024-01-15"
 * formatTimestamp(2024, 12, 31) // => "2024-12-31"
 */
export const formatTimestamp = (year: number, month: number, day: number): string =>
  `${year.toString().padStart(4, '0')}-${month.toString().padStart(2, '0')}-${day.toString().padStart(2, '0')}`
