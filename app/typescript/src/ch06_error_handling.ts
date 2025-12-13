/**
 * 第6章: Option 型による安全なエラーハンドリング
 *
 * null や例外に頼らず、Option を使って型安全にエラーを扱う方法を学びます。
 * fp-ts では Option<A> は Some<A> または None を表します。
 */

import { pipe } from 'fp-ts/function'
import * as O from 'fp-ts/Option'
import * as RA from 'fp-ts/ReadonlyArray'

// =============================================================================
// 6.1 Option の基本
// =============================================================================

/**
 * 安全な除算。0で割る場合は None を返す。
 *
 * @example
 * safeDivide(10, 2) // => some(5)
 * safeDivide(10, 0) // => none
 */
export const safeDivide = (a: number, b: number): O.Option<number> =>
  b === 0 ? O.none : O.some(Math.floor(a / b))

/**
 * 文字列を数値に安全に変換する。
 *
 * @example
 * parseNumber("42") // => some(42)
 * parseNumber("abc") // => none
 */
export const parseNumber = (s: string): O.Option<number> => {
  const n = parseInt(s, 10)
  return isNaN(n) ? O.none : O.some(n)
}

/**
 * 配列の最初の要素を安全に取得する。
 *
 * @example
 * safeHead([1, 2, 3]) // => some(1)
 * safeHead([]) // => none
 */
export const safeHead = <T>(arr: readonly T[]): O.Option<T> =>
  pipe(arr, RA.head)

/**
 * 配列の最後の要素を安全に取得する。
 *
 * @example
 * safeLast([1, 2, 3]) // => some(3)
 * safeLast([]) // => none
 */
export const safeLast = <T>(arr: readonly T[]): O.Option<T> =>
  pipe(arr, RA.last)

/**
 * 配列の指定したインデックスの要素を安全に取得する。
 *
 * @example
 * safeGet([1, 2, 3], 1) // => some(2)
 * safeGet([1, 2, 3], 10) // => none
 */
export const safeGet = <T>(arr: readonly T[], index: number): O.Option<T> =>
  pipe(arr, RA.lookup(index))

// =============================================================================
// 6.2 TV番組のパース例
// =============================================================================

/** TV番組を表すイミュータブルなインターフェース */
export interface TvShow {
  readonly title: string
  readonly start: number
  readonly end: number
}

/** TvShow を作成する */
export const createTvShow = (
  title: string,
  start: number,
  end: number
): TvShow => ({ title, start, end })

/**
 * 名前を抽出する。
 *
 * @example
 * extractName("Breaking Bad (2008-2013)") // => some("Breaking Bad")
 * extractName("(2008-2013)") // => none
 */
export const extractName = (rawShow: string): O.Option<string> => {
  const bracketOpen = rawShow.indexOf('(')
  if (bracketOpen > 0) {
    return O.some(rawShow.substring(0, bracketOpen).trim())
  }
  return O.none
}

/**
 * 開始年を抽出する。
 *
 * @example
 * extractYearStart("Breaking Bad (2008-2013)") // => some(2008)
 * extractYearStart("Chernobyl (2019)") // => none (ダッシュがない)
 */
export const extractYearStart = (rawShow: string): O.Option<number> => {
  const bracketOpen = rawShow.indexOf('(')
  const dash = rawShow.indexOf('-')
  if (bracketOpen !== -1 && dash > bracketOpen + 1) {
    const yearStr = rawShow.substring(bracketOpen + 1, dash)
    return parseNumber(yearStr)
  }
  return O.none
}

/**
 * 終了年を抽出する。
 *
 * @example
 * extractYearEnd("Breaking Bad (2008-2013)") // => some(2013)
 * extractYearEnd("Chernobyl (2019)") // => none (ダッシュがない)
 */
export const extractYearEnd = (rawShow: string): O.Option<number> => {
  const dash = rawShow.indexOf('-')
  const bracketClose = rawShow.indexOf(')')
  if (dash !== -1 && bracketClose > dash + 1) {
    const yearStr = rawShow.substring(dash + 1, bracketClose)
    return parseNumber(yearStr)
  }
  return O.none
}

/**
 * 単年を抽出する（"Chernobyl (2019)" のような形式用）。
 *
 * @example
 * extractSingleYear("Chernobyl (2019)") // => some(2019)
 * extractSingleYear("Breaking Bad (2008-2013)") // => none
 */
export const extractSingleYear = (rawShow: string): O.Option<number> => {
  const dash = rawShow.indexOf('-')
  const bracketOpen = rawShow.indexOf('(')
  const bracketClose = rawShow.indexOf(')')
  if (dash === -1 && bracketOpen !== -1 && bracketClose > bracketOpen + 1) {
    const yearStr = rawShow.substring(bracketOpen + 1, bracketClose)
    return parseNumber(yearStr)
  }
  return O.none
}

/**
 * TV番組の文字列をパースする。
 *
 * @example
 * parseShow("Breaking Bad (2008-2013)")
 * // => some({ title: "Breaking Bad", start: 2008, end: 2013 })
 *
 * parseShow("Chernobyl (2019)")
 * // => some({ title: "Chernobyl", start: 2019, end: 2019 })
 *
 * parseShow("Invalid")
 * // => none
 */
export const parseShow = (rawShow: string): O.Option<TvShow> =>
  pipe(
    O.Do,
    O.bind('title', () => extractName(rawShow)),
    O.bind('start', () =>
      pipe(
        extractYearStart(rawShow),
        O.orElse(() => extractSingleYear(rawShow))
      )
    ),
    O.bind('end', () =>
      pipe(
        extractYearEnd(rawShow),
        O.orElse(() => extractSingleYear(rawShow))
      )
    ),
    O.map(({ title, start, end }) => createTvShow(title, start, end))
  )

// =============================================================================
// 6.3 Option の主要操作
// =============================================================================

/**
 * Option の値を変換する（map）。
 *
 * @example
 * mapOption(some(5), x => x * 2) // => some(10)
 * mapOption(none, x => x * 2) // => none
 */
export const mapOption = <A, B>(
  opt: O.Option<A>,
  f: (a: A) => B
): O.Option<B> => pipe(opt, O.map(f))

/**
 * Option を返す関数を適用する（flatMap/chain）。
 *
 * @example
 * flatMapOption(some(10), x => safeDivide(x, 2)) // => some(5)
 * flatMapOption(some(10), x => safeDivide(x, 0)) // => none
 */
export const flatMapOption = <A, B>(
  opt: O.Option<A>,
  f: (a: A) => O.Option<B>
): O.Option<B> => pipe(opt, O.chain(f))

/**
 * 条件に合わない場合は None にする（filter）。
 *
 * @example
 * filterOption(some(5), x => x > 3) // => some(5)
 * filterOption(some(5), x => x > 10) // => none
 */
export const filterOption = <A>(
  opt: O.Option<A>,
  predicate: (a: A) => boolean
): O.Option<A> => pipe(opt, O.filter(predicate))

/**
 * None の場合に代替を使用する（orElse）。
 *
 * @example
 * orElseOption(none, () => some(10)) // => some(10)
 * orElseOption(some(5), () => some(10)) // => some(5)
 */
export const orElseOption = <A>(
  opt: O.Option<A>,
  alternative: () => O.Option<A>
): O.Option<A> => pipe(opt, O.orElse(alternative))

/**
 * None の場合にデフォルト値を返す（getOrElse）。
 *
 * @example
 * getOrElseOption(some(5), () => 0) // => 5
 * getOrElseOption(none, () => 0) // => 0
 */
export const getOrElseOption = <A>(
  opt: O.Option<A>,
  defaultValue: () => A
): A => pipe(opt, O.getOrElse(defaultValue))

// =============================================================================
// 6.4 エラーハンドリング戦略
// =============================================================================

/**
 * Best-effort 戦略: パースできたものだけを返す。
 *
 * @example
 * parseShowsBestEffort([
 *   "Breaking Bad (2008-2013)",
 *   "Invalid",
 *   "Mad Men (2007-2015)"
 * ])
 * // => [TvShow("Breaking Bad", 2008, 2013), TvShow("Mad Men", 2007, 2015)]
 */
export const parseShowsBestEffort = (
  rawShows: readonly string[]
): readonly TvShow[] =>
  pipe(
    rawShows,
    RA.map(parseShow),
    RA.compact // None を除去して Some の値だけを抽出
  )

/**
 * All-or-nothing 戦略: 全部成功するか、全部失敗。
 *
 * @example
 * parseShowsAllOrNothing([
 *   "Breaking Bad (2008-2013)",
 *   "Mad Men (2007-2015)"
 * ])
 * // => some([TvShow("Breaking Bad", 2008, 2013), TvShow("Mad Men", 2007, 2015)])
 *
 * parseShowsAllOrNothing([
 *   "Breaking Bad (2008-2013)",
 *   "Invalid"
 * ])
 * // => none
 */
export const parseShowsAllOrNothing = (
  rawShows: readonly string[]
): O.Option<readonly TvShow[]> =>
  pipe(rawShows, RA.traverse(O.Applicative)(parseShow))

// =============================================================================
// 6.5 Option を使った検索
// =============================================================================

/**
 * 条件に合う最初の要素を検索する。
 *
 * @example
 * findFirst([1, 2, 3, 4, 5], x => x > 3) // => some(4)
 * findFirst([1, 2, 3], x => x > 10) // => none
 */
export const findFirst = <A>(
  arr: readonly A[],
  predicate: (a: A) => boolean
): O.Option<A> => pipe(arr, RA.findFirst(predicate))

/**
 * 条件に合う最後の要素を検索する。
 *
 * @example
 * findLast([1, 2, 3, 4, 5], x => x > 3) // => some(5)
 * findLast([1, 2, 3], x => x > 10) // => none
 */
export const findLast = <A>(
  arr: readonly A[],
  predicate: (a: A) => boolean
): O.Option<A> => pipe(arr, RA.findLast(predicate))

// =============================================================================
// 6.6 forall と exists
// =============================================================================

/**
 * Option の値が条件を満たすか（または None か）を判定する。
 * None の場合は true を返す。
 *
 * @example
 * forallOption(some(5), x => x < 10) // => true
 * forallOption(some(15), x => x < 10) // => false
 * forallOption(none, x => x < 10) // => true (値がないので)
 */
export const forallOption = <A>(
  opt: O.Option<A>,
  predicate: (a: A) => boolean
): boolean =>
  pipe(
    opt,
    O.match(
      () => true,
      (a) => predicate(a)
    )
  )

/**
 * Option に値があり、かつ条件を満たすかを判定する。
 * None の場合は false を返す。
 *
 * @example
 * existsOption(some(5), x => x < 10) // => true
 * existsOption(some(15), x => x < 10) // => false
 * existsOption(none, x => x < 10) // => false (値がない)
 */
export const existsOption = <A>(
  opt: O.Option<A>,
  predicate: (a: A) => boolean
): boolean =>
  pipe(
    opt,
    O.match(
      () => false,
      (a) => predicate(a)
    )
  )

// =============================================================================
// 6.7 Option のユーティリティ
// =============================================================================

/**
 * Option を配列に変換する。
 *
 * @example
 * toArray(some(5)) // => [5]
 * toArray(none) // => []
 */
export const toArray = <A>(opt: O.Option<A>): readonly A[] =>
  pipe(
    opt,
    O.match(
      () => [] as readonly A[],
      (a) => [a] as readonly A[]
    )
  )

/**
 * null/undefined を Option に変換する。
 *
 * @example
 * fromNullable(5) // => some(5)
 * fromNullable(null) // => none
 * fromNullable(undefined) // => none
 */
export const fromNullable = <A>(a: A | null | undefined): O.Option<A> =>
  O.fromNullable(a)

/**
 * 条件を満たす場合のみ Some を返す。
 *
 * @example
 * fromPredicate(5, x => x > 0) // => some(5)
 * fromPredicate(-5, x => x > 0) // => none
 */
export const fromPredicate = <A>(
  a: A,
  predicate: (a: A) => boolean
): O.Option<A> => (predicate(a) ? O.some(a) : O.none)

// =============================================================================
// 6.8 2つの数値文字列の加算
// =============================================================================

/**
 * 2つの数値文字列を加算する。
 * どちらかがパースできない場合は None を返す。
 *
 * @example
 * addStrings("10", "20") // => some(30)
 * addStrings("10", "abc") // => none
 */
export const addStrings = (a: string, b: string): O.Option<number> =>
  pipe(
    O.Do,
    O.bind('x', () => parseNumber(a)),
    O.bind('y', () => parseNumber(b)),
    O.map(({ x, y }) => x + y)
  )

// =============================================================================
// 6.9 Option と ReadonlyArray の組み合わせ
// =============================================================================

/**
 * 配列の全要素に Option を返す関数を適用し、
 * 全て Some の場合のみ結果を返す（traverse）。
 *
 * @example
 * traverseArray(["1", "2", "3"], parseNumber) // => some([1, 2, 3])
 * traverseArray(["1", "abc", "3"], parseNumber) // => none
 */
export const traverseArray = <A, B>(
  arr: readonly A[],
  f: (a: A) => O.Option<B>
): O.Option<readonly B[]> => pipe(arr, RA.traverse(O.Applicative)(f))

/**
 * 配列内の最大値を安全に取得する。
 *
 * @example
 * safeMax([1, 5, 3]) // => some(5)
 * safeMax([]) // => none
 */
export const safeMax = (numbers: readonly number[]): O.Option<number> =>
  pipe(
    numbers,
    RA.reduce(O.none as O.Option<number>, (acc, n) =>
      pipe(
        acc,
        O.match(
          () => O.some(n),
          (max) => O.some(Math.max(max, n))
        )
      )
    )
  )

/**
 * 配列内の最小値を安全に取得する。
 *
 * @example
 * safeMin([1, 5, 3]) // => some(1)
 * safeMin([]) // => none
 */
export const safeMin = (numbers: readonly number[]): O.Option<number> =>
  pipe(
    numbers,
    RA.reduce(O.none as O.Option<number>, (acc, n) =>
      pipe(
        acc,
        O.match(
          () => O.some(n),
          (min) => O.some(Math.min(min, n))
        )
      )
    )
  )
